------------------------------------------------------------------------------
--- This is the main module of the Curry2Go compiler.
--- It contains the operations to compiler complete Curry programs
--- with the `CompilerStructure` module and the actual compiler.
---
--- @author Jonas Boehm, Michael Hanus
--- @version June 2021
------------------------------------------------------------------------------

module Curry2Go.Main where

import Curry.Compiler.Distribution
import Data.IORef
import Data.List             ( find, intercalate, last, partition )
import System.Environment    ( getArgs )

import Control.Monad         ( unless, when )

import Data.Time             ( compareClockTime )
import FlatCurry.CaseCompletion ( dataDeclsOf )
import FlatCurry.Types
import FlatCurry.Files       ( flatCurryFileName, readFlatCurryWithParseOptions
                             , readFlatCurryIntWithParseOptions )
import FlatCurry.Goodies     ( funcName, funcVisibility
                             , progFuncs, progImports, progName )
import Language.Go.ShowS     ( showGoProg )
import ICurry.Types
import ICurry.Compiler       ( flatCurry2ICurryWithProgs
                             , flatCurry2ICurryWithProgsAndOptions )
import ICurry.Options        ( ICOptions(..), defaultICOptions )
import ReadShowTerm          ( readUnqualifiedTerm, showTerm )
import System.CurryPath
import System.Console.GetOpt
import System.Directory
import System.FilePath
import System.Process        ( exitWith, system )
import System.FrontendExec

import CompilerStructure
import Curry2Go.Compiler
import Curry2Go.Config       ( compilerMajorVersion, compilerMinorVersion
                             , compilerRevisionVersion
                             , compilerName, lowerCompilerName
                             , curry2goDir, upperCompilerName )
import Curry2Go.InstallPath  ( curry2GoHomeDir )
import Curry2Go.PkgConfig    ( packageVersion )

--- Implementation of CompStruct for the curry2go compiler.

--- Lookup module dir and source file and run an action on this information.
doOnModuleSource :: String -> ((String,String) -> IO a) -> IO a
doOnModuleSource mname modact =
  lookupModuleSourceInLoadPath mname >>= \path -> case path of
    Nothing      -> error $ "Unknown module " ++ mname
    Just dirfile -> modact dirfile

--- Returns the filepath relative to `curry2goDir` where
--- the compiled version of the module `m` will be stored.
modPackage :: String -> String
modPackage m =
  combine (modNameToPath m) (last (splitModuleIdentifiers m) ++ ".go")

--- Returns the output path for a compiled Curry module.
getGoTargetFilePath :: String -> IO String
getGoTargetFilePath m = doOnModuleSource m
  (\ (dir, _) -> return (joinPath [dir, curry2goDir, modPackage m]))

--- Returns `True` if the module has already a compilation target which
--- is newer than the source and the FlatCurry file (if present).
isCompiledCurryModule :: CGOptions -> String -> IO Bool
isCompiledCurryModule opts mname = doOnModuleSource mname $ \ (mdir,msrc) -> do
  printVerb opts 2 $ "Source file: " ++ msrc
  let targetfile = joinPath [mdir, curry2goDir, modPackage mname]
  printVerb opts 2 $ "Target file: " ++ targetfile
  extarget <- doesFileExist targetfile
  if extarget
    then do
      mtime <- getModificationTime msrc
      ttime <- getModificationTime targetfile
      if compareClockTime mtime ttime == GT
        then return False
        else do
          let fcyfile = joinPath [mdir, curry2goDir, mname ++ ".fcy"]
          printVerb opts 2 $ "FlatCurry file: " ++ fcyfile
          exfcy <- doesFileExist fcyfile
          if exfcy
            then do
              ftime <- getModificationTime fcyfile
              return $ compareClockTime ftime ttime == LT
            else return True -- fcy does not exist: not relevant here
    else return False
  

--- Gets the path to the source file of a Curry module.
getCurrySourcePath :: String -> IO String
getCurrySourcePath m = doOnModuleSource m (return . snd)

------------------------------------------------------------------------------
-- Loading and caching FlatCurry interfaces.
-- In order to speed up reading of interfaces (like the Prelude interface),
-- a cache file with reduced interface information is stored in the
-- `curry2go` target directory. If it is up-to-date, the interface
-- is reconstructed from the information in the cache file.
--
-- IMPORTANT NOTE: the cache file contains the minimum information
-- required by the `icurry` compiler for interfaces. Thus, if something
-- is changed in the `icurry` compiler, the structure of cache file
-- might need to be adapted. 

--- Load a FlatCurry interface for a module if not already done.
loadInterface :: CGOptions -> IORef GSInfo -> String -> IO Prog
loadInterface opts sref mname = do
  gsinfo <- readIORef sref
  maybe (readInterface gsinfo)
        return
        (find (\fp -> progName fp == mname) (gsProgs gsinfo))
 where
  readInterface gsinfo = do
    let outPath = combine curry2goDir (modPackage mname)
        outDir  = takeDirectory outPath
        intfile = outDir </> "INTERFACE"
    showCreateDirectory opts outDir
    sourcefile <- getCurrySourcePath mname
    int <- ifNewerFile sourcefile
                       intfile
                       (readInterfaceFromFlatCurry intfile)
                       (readIntFile opts intfile)
    writeIORef sref (gsinfo { gsProgs = int : gsProgs gsinfo } )
    return int

  readInterfaceFromFlatCurry intfile = do
    printVerb opts 2 $ "Reading interface of '" ++ mname ++ "'"
    int <- showReadFlatCurryIntWithParseOptions opts mname
    writeIntFile opts intfile int
    return int

--- Writes the `INTERFACE` file with reduced `fint` information.
writeIntFile :: CGOptions -> String -> Prog -> IO ()
writeIntFile opts intfile fint = do
  printVerb opts 2 $ "Writing interface cache file '" ++ intfile ++ "'"
  let pubfuns       = publicFunsOfProg fint
      (mfuns,ofuns) = partition ((== progName fint) . fst) pubfuns
  writeFile intfile
    (showTerm ( progName fint
              , progImports fint
              , consDeclsOfProg fint
              , if null ofuns then map snd mfuns else []
              , if null ofuns then []            else pubfuns))
 where
  -- compute list of data constructors
  consDeclsOfProg fcy = map (\ (_,cars) -> cars) (dataDeclsOf fcy)

  -- compute list of public function names split into names qualified
  -- by the module name or not
  publicFunsOfProg fcprog =
      (map funcName
           (filter (\f -> funcVisibility f == FlatCurry.Types.Public)
                   (progFuncs fcprog)))

--- Reads an `INTERFACE` file and reconstruct a FlatCurry interface.
readIntFile :: CGOptions -> String -> IO Prog
readIntFile opts intfile = do
  printVerb opts 2 $ "Reading interface cache file '" ++ intfile ++ "'"
  (mname,imps,consmap,mfuns,ofuns) <-
    readFile intfile >>= return . readUnqualifiedTerm ["Prelude"]
  return (Prog mname imps (map consItem2TypeDecl consmap)
               (map funcItem2Func (map (\f->(mname,f)) mfuns ++ ofuns))
               notUsed)
 where
  consItem2TypeDecl consitems =
    Type notUsed FlatCurry.Types.Public notUsed (map consItem2Cons consitems)

  consItem2Cons (cn,ar) = Cons cn ar FlatCurry.Types.Public notUsed

  funcItem2Func qn = Func qn notUsed FlatCurry.Types.Public notUsed notUsed

notUsed :: _
notUsed = error "Internal error: access to unused interface component"

------------------------------------------------------------------------------
--- Gets the imported modules of a Curry module.
getCurryImports :: CGOptions -> IORef GSInfo -> String -> IO [String]
getCurryImports opts sref mname = do
  let outPath = combine curry2goDir (modPackage mname)
      outDir  = takeDirectory outPath
      impFile = outDir </> "IMPORTS"
  showCreateDirectory opts outDir
  source <- getCurrySourcePath mname
  ifNewerFile source
              impFile
              (readImportsFromInterface outDir impFile)
              (readImportsFromCache impFile)
 where
  readImportsFromInterface outDir impFile = do
    imps <- loadInterface opts sref mname >>= return . progImports
    showCreateDirectory opts outDir
    printVerb opts 2 $ "Writing imports cache file '" ++ impFile ++ "'"
    writeFile impFile (unlines imps)
    return imps

  readImportsFromCache impfile = do
    printVerb opts 2 $ "Reading imports cache file '" ++ impfile ++ "'"
    readFile impfile >>= return . lines

------------------------------------------------------------------------------
--- Loads an IProg from the name of a Curry module.
loadICurry :: CGOptions -> IORef GSInfo -> String -> IO IProg
loadICurry opts sref mname = do
  prog           <- showReadFlatCurryWithParseOptions opts mname
  impints        <- mapM (loadInterface opts sref) (progImports prog)
  gsinfo         <- readIORef sref
  (icopts,iprog) <- flatCurry2ICurryWithProgsAndOptions (gsICOpts gsinfo)
                                                        impints prog
  writeIORef sref (gsinfo { gsICOpts = icopts })
  return iprog

showReadFlatCurryWithParseOptions :: CGOptions -> String -> IO Prog
showReadFlatCurryWithParseOptions opts mname = do
  frontendparams <- c2gFrontendParams opts
  when (verbosity opts > 2) $ do
    cmd <- getFrontendCall FCY frontendparams mname
    printVerb opts 3$ "Executing: " ++ cmd
  readFlatCurryWithParseOptions mname frontendparams

showReadFlatCurryIntWithParseOptions :: CGOptions -> String -> IO Prog
showReadFlatCurryIntWithParseOptions opts mname = do
  frontendparams <- c2gFrontendParams opts
  when (verbosity opts > 2) $ do
    cmd <- getFrontendCall FINT frontendparams mname
    printVerb opts 3 $ "Executing: " ++ cmd
  readFlatCurryIntWithParseOptions mname frontendparams

-- The front-end parameters for Curry2Go.
c2gFrontendParams :: CGOptions -> IO FrontendParams
c2gFrontendParams opts = do
  frontendroot <- if curryCompiler == "curry2go"
                    then curry2GoHomeDir
                    else return installDir 
  let frontendpath = frontendroot </> "bin" </> curryCompiler ++ "-frontend"
  return (setFrontendPath frontendpath $
          setQuiet (verbosity opts < 2) $
          setDefinitions [curry2goDef] $
          setOutDir curry2goDir $
          defaultParams)
 where
  curry2goDef = ("__" ++ upperCompilerName ++ "__",
                 compilerMajorVersion * 100 + compilerMinorVersion)

-- The ICurry compiler options for Curry2Go.
c2gICOptions :: CGOptions -> IO ICOptions
c2gICOptions opts = do
  frontendparams <- c2gFrontendParams opts
  return defaultICOptions { optVerb = 0, optFrontendParams = frontendparams }

--- Copies external files that are in the include folder or
--- next to the source file into the directory with the
--- compiled version of a Curry module.
postProcess :: CGOptions -> String -> IO ()
postProcess opts mname = do
  let outPath = combine curry2goDir (modPackage mname)
      outDir  = takeDirectory outPath
  showCreateDirectory opts outDir
  fPath       <- getTargetFilePath (goStruct opts) mname
  extFilePath <- getExtFilePath
  extInSource <- doesFileExist extFilePath
  let extFileName = takeFileName extFilePath
  if extInSource
    then copyIfNewer extFilePath (combine outDir extFileName)
    else do
      c2ghome <- curry2GoHomeDir
      let c2gExtFile = c2ghome </> "external_files" </> extFileName
      extInC2GInclude <- doesFileExist c2gExtFile
      when extInC2GInclude $ do
        content <- readFile c2gExtFile
        let packageName = (words content) !! 1
        when (packageName == removeDots mname) $
          copyIfNewer c2gExtFile (combine outDir extFileName)
  copyIfNewer fPath outPath
 where
  copyIfNewer source target = do
    ifNewerFile source target (showCopyFile source target)  (return ())

  showCopyFile source target = do
    printVerb opts 2 $ "Copying '" ++ source ++ "' to '" ++ target ++ "'..."
    -- copyFile source target
    ec <- system $ unwords ["/bin/cp", source, target]
    unless (ec == 0) $
      error $ "Error during copying '" ++ source ++ "' to '" ++ target ++ "'!"

  getExtFilePath = do
    path <- getCurrySourcePath mname
    return $
      replaceFileName path
        (stripCurrySuffix (takeFileName path) ++ "_external.go")

------------------------------------------------------------------------------
--- The structure for the Curry2Go compilation process.
--- The compiler cache manages the list of already loaded FlatCurry interfaces
--- to avoid multiple readings.
goStruct :: CGOptions -> CompStruct IProg GSInfo
goStruct opts = defaultStruct
  { cmpVerbosity      = verbosity opts
  , cmpTime           = ctimeOpt opts
  , getTargetFilePath = getGoTargetFilePath
  , excludeModules    = []
  , getProg           = loadICurry opts
  , getImports        = if noimports opts then (\_ _ -> return [])
                                       else getCurryImports opts
  , isCompiled        = isCompiledCurryModule opts
  , postProc          = postProcess opts
  }

-- The state of the Go compilation process.
-- It consists of a list of already loaded FlatCurry interfaces and
-- the current options for the ICurry compiler.
data GSInfo = GSInfo
  { gsProgs    :: [Prog]    -- loaded interfaces
  , gsICOpts   :: ICOptions -- current options for the ICurry compiler
  }

initGSInfo :: ICOptions -> GSInfo
initGSInfo icopts = GSInfo [] icopts

------------------------------------------------------------------------------
--- Implementation of compiler invokation.

--- main function
main :: IO ()
main = do
  args <- getArgs
  (opts, paths) <- processOptions args
  case paths of
    []  -> error "Input path missing!"
    [p] -> runModuleAction (curry2Go opts) (stripCurrySuffix p)
    _   -> error "Too many paths given!"

c2goBanner :: String
c2goBanner = unlines [bannerLine, bannerText, bannerLine]
 where
  bannerText = compilerName ++ " Compiler (Version " ++ packageVersion ++
               ", installed with " ++ curryCompiler ++ "-" ++ distVersion ++ ")"
  bannerLine = take (length bannerText) (repeat '-')
  distVersion = intercalate "." $ map show
                  [ curryCompilerMajorVersion
                  , curryCompilerMinorVersion
                  , curryCompilerRevisionVersion ]

--- Compiles a Curry program into a Go program.
--- @param opts    - compiler options 
--- @param mainmod - name of main module
curry2Go :: CGOptions -> String -> IO ()
curry2Go opts mainmod = do
  unless (nobanner opts) $ printVerb opts { ctimeOpt = False } 1 c2goBanner
  printVerb opts 1 $ "Compiling program '" ++ mainmod ++ "'..."
  -- read main FlatCurry in order to be sure that all imports are up-to-date
  -- and show warnings to the user if not in quiet mode (but avoid reading
  -- the prelude if not necessary in order to speed up REPL start)
  fprog      <- parseMainFlatCurry
  initicopts <- c2gICOptions opts
  sref       <- newIORef (initGSInfo initicopts)
  let gostruct = (goStruct opts) {compProg = compileIProg2GoString opts}
  compile gostruct sref mainmod
  createModFile curry2goDir
  printVerb opts 2 $ "Go programs written into '" ++ curry2goDir ++ "'"
  if not (genMain opts)
    then return ()
    else do
      impints <- mapM (loadInterface opts sref) (progImports fprog)
      gsinfo <- readIORef sref
      IProg modname _ _ funcs <- flatCurry2ICurryWithProgs (gsICOpts gsinfo)
                                                           impints fprog
      generateMainProg modname funcs
      createExecutable modname
      when (runOpt opts) $ execProgram modname
 where
  parseMainFlatCurry = do
    let verb     = verbosity opts
        opts4fcy = opts { verbosity = if verb == 1 then 2 else verb }
    if mainmod /= "Prelude" || genMain opts
      then showReadFlatCurryWithParseOptions opts4fcy mainmod
      else do
        preludeready <- isCompiledCurryModule opts "Prelude"
        if preludeready
          then return (error "Internal error: unread prelude")
          else showReadFlatCurryWithParseOptions opts4fcy mainmod

  createModFile dir = do
    c2ghome <- curry2GoHomeDir
    let content = unlines ["module curry2go"
                    , "require gocurry v1.0.0"
                    , "replace gocurry => "
                    ++ (c2ghome </> "go" </> "src" </> "gocurry")]
    writeFile (combine dir "go.mod") content
    
  generateMainProg modname funcs = do
    let mainprogname = removeDots modname ++ "Main.go"
    printVerb opts 1 $ "Generating main program '" ++ mainprogname ++ "'"
    let mainprog = showGoProg
                     (createMainProg funcs (opts {modName = "main"})) ""
    printVerb opts 4 $ "Main Go program:\n\n" ++ mainprog
    let mainfile = combine curry2goDir mainprogname
    writeFile mainfile mainprog
    printVerb opts 2 $ "...written to " ++ mainfile

  createExecutable modname = do
    printVerb opts 1 "Creating executable..."
    oldDir <- getCurrentDirectory
    setCurrentDirectory curry2goDir
    let mainname = removeDots modname ++ "Main"
    let bcmd = "env \"GO111MODULE=auto\" go build " ++ mainname ++ ".go"
    printVerb opts 3 $ "...with command: " ++ bcmd
    i <- system bcmd
    when (i /= 0) $ error "Build failed!"
    setCurrentDirectory oldDir
    renameFile (curry2goDir </> mainname) modname
    printVerb opts 2 $ "Executable stored in: " ++ modname

  execProgram modname = do
    printVerb opts 1 "Running..."
    let rcmd = "./" ++ modname
    printVerb opts 3 $ "...with command: " ++ rcmd
    system rcmd
    return ()

showCreateDirectory :: CGOptions -> String -> IO ()
showCreateDirectory opts dirname = do
  printVerb opts 3 $ "Creating directory '" ++ dirname ++ "'..."
  createDirectoryIfMissing True dirname

------------------------------------------------------------------------------
--- Turns command line arguments into options and arguments.
processOptions :: [String] -> IO (CGOptions, [String])
processOptions argv = do
  let (funopts, args, opterrors) = getOpt Permute options argv
      opts = foldr (\f x -> f x) defaultCGOptions funopts
  unless (null opterrors)
    (putStr (unlines opterrors) >> putStr usageText >> exitWith 1)
  when (help opts) $ do
    unless (nobanner opts) $ putStrLn c2goBanner
    putStr usageText
    exitWith 0
  printArgs argv
  when (printName opts || printNumVer opts || printBaseVer opts) (exitWith 0)
  when (not (genMain opts) && runOpt opts) $
    error "Options 'compile' and 'run' cannot be combined!"
  return (opts, args)
 
--- Prints text for certain compiler flags, that need to be
--- printed in the same order as they were provided.
printArgs :: [String] -> IO ()
printArgs []     = return ()
printArgs (x:xs) = case x of
  "--compiler-name"   -> putStrLn lowerCompilerName >> printArgs xs
  "--numeric-version" -> putStrLn packageVersion >> printArgs xs
  "--base-version"    -> printBaseVersion >> printArgs xs
  _                   -> printArgs xs
 where
  printBaseVersion = do
    c2ghome <- curry2GoHomeDir
    bvs <- readFile (c2ghome </> "lib" </> "VERSION")
    putStrLn (head (lines bvs))

--- Help text
usageText :: String
usageText = usageInfo "Usage: curry2go [options] <input>\n" options

--- Definition of command line options.
options :: [OptDescr (CGOptions -> CGOptions)]
options = 
  [ Option "h?" ["help"]
    (NoArg (\opts -> opts {help = True})) "print help and exit"
  , Option "q" ["quiet"]
           (NoArg (\opts -> opts { verbosity = 0 }))
           "run quietly (no output, only exit code)"
  , Option "v" ["verbosity"]
      (OptArg (maybe (\opts -> opts { verbosity = 2}) checkVerb) "<n>")
         "verbosity level:\n0: quiet (same as `-q')\n1: show status messages (default)\n2: show target file names (same as `-v')\n3: show invoked commands\n4: show all details"
  , Option "" ["dfs"]
    (NoArg (\opts -> opts {strat = DFS})) "use depth first search (default)"
  , Option ""   ["bfs"]
    (NoArg (\opts -> opts {strat = BFS})) "use breadth first search"
  , Option ""   ["fs"]
    (OptArg (maybe (\opts -> opts {strat = FS}) 
    (\s opts -> opts {strat = FS, maxTasks = safeRead s})) "<n>") 
    "use fair search\nn = maximum number of concurrent computations\n(default: 0 = infinite)"
  , Option "c" ["compile"]
           (NoArg (\opts -> opts {genMain = False}))
           "only compile, do not generate executable"
  , Option "r" ["run"]
           (NoArg (\opts -> opts {runOpt = True}))
           "run program after compilation"
  , Option "t" ["time"]
    (OptArg (maybe (\opts -> opts {timeOpt = True})
    (\s opts -> opts {timeOpt = True, times = safeRead s})) "<n>")
    "print execution time\nn>1: average over runs n"
  , Option "" ["ctime"]
    (NoArg (\opts -> opts {ctimeOpt = True}))
    "print compilation messages with elapsed time"
  , Option "" ["first"]
    (NoArg (\opts -> opts {maxResults = 1}))
    "stop evaluation after the first result"
  , Option "n" ["results"] 
    (ReqArg (\s opts -> opts {maxResults = safeRead s}) "<n>")
    "set maximum number of results to be computed\n(default: 0 = infinite)"
  , Option "i" ["interactive"]
    (NoArg (\opts -> opts {interact = True}))
    "interactive result printing\n(ask to print next result)"
  , Option "s" ["main"]
    (ReqArg (\s opts -> opts {mainName = s}) "<f>")
    "set name of main function to f (default: main)"
  , Option "" ["noimports"]
    (NoArg (\opts -> opts {noimports = True}))
    "do not compile import modules"
  , Option "" ["nobanner"]
    (NoArg (\opts -> opts {nobanner = True}))
    "do not print banner on start up"
  , Option "" ["hnf"]
    (NoArg (\opts -> opts {onlyHnf = True})) "only compute hnf"
  , Option "" ["compiler-name"]
    (NoArg (\opts -> opts {printName = True})) "print the compiler name and exit"
  , Option "" ["numeric-version"]
    (NoArg (\opts -> opts {printNumVer = True})) "print the numeric version and exit"
  , Option "" ["base-version"]
    (NoArg (\opts -> opts {printBaseVer = True})) "print the base version and exit"
  ]
 where
  safeRead s = case reads s of
    [(n,"")] -> n
    _        -> error "Invalid argument! Use -h for help."

  checkVerb s opts = if n >= 0 && n <= 4
                       then opts { verbosity = n }
                       else error "Illegal verbosity level (use `-h' for help)"
   where n = safeRead s

------------------------------------------------------------------------------
-- Auxiliaries

--- If `file1` is newer then `file2` or `file2` does not exist,
--- run `action1`, otherwise run `action2`.
ifNewerFile :: String -> String -> IO a -> IO a -> IO a
ifNewerFile file1 file2 action1 action2 = do
  exf2 <- doesFileExist file2
  if exf2
    then do
      time1  <- getModificationTime file1
      time2  <- getModificationTime file2
      if compareClockTime time1 time2 == GT
        then action1
        else action2
    else action1

------------------------------------------------------------------------------
