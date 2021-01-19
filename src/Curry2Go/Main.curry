module Curry2Go.Main where

import Go.Show
import Go.Types
import CompilerStructure
import ICurry.Types
import ICurry.Compiler
import System.Environment
import System.CurryPath
import System.Console.GetOpt
import System.Directory
import Data.Char
import System.FilePath
import System.Process
import Data.List
import System.FrontendExec
import FlatCurry.Files
import Control.Monad

import Curry2Go.Compiler
import Curry2Go.Config   ( packageVersion )

--- Implementation of CompStruct for the curry2go compiler.

--- Gets the path to the source file of a curry module.
loadCurryPath :: String -> IO String
loadCurryPath inp =
  lookupModuleSourceInLoadPath (stripCurrySuffix inp) >>= \path -> case path of
    Nothing          -> error ("Unknown module " ++ inp)
    Just (dir, file) -> return (combine dir file)

--- Loads an IProg from the name of a curry module.
--- Copies external files that are in the include folder or
--- next to the source file.
loadCurry :: String -> IO IProg
loadCurry inp = do extFilePath <- getExtFilePath
                   extFileName <- return (takeFileName extFilePath)
                   home <- getHomeDirectory
                   extInSource <- doesFileExist extFilePath
                   if extInSource
                     then copyFile extFilePath
                       (combine (getFileDir goStruct inp) extFileName)
                     else do 
                       extInInclude <- doesFileExist
                         (joinPath [home, ".gocurry", "include", extFileName])
                       when extInInclude (copyFile
                         (joinPath [home, ".gocurry", "include", extFileName])
                         (combine (getFileDir goStruct inp) extFileName))
                   prog <- readFlatCurryWithParseOptions (stripCurrySuffix inp)
                     (setQuiet True (setDefinitions [] defaultParams))
                   flatCurry2ICurry (defaultICOptions {optVerb = 0}) prog
 where
  getExtFilePath = do
    path <- loadCurryPath (stripCurrySuffix inp)
    return $
      replaceFileName path
        ("external_" ++ takeFileName (replaceExtension path "go"))


goStruct :: CompStruct IProg
goStruct = defaultStruct
  { outputDir      = combine ".curry" ("curry2go-" ++ packageVersion)
  , filePath       =
      (\s -> combine (modNameToPath s) 
        (last (splitModuleIdentifiers s) ++ ".go"))
  , excludeModules = ["Prelude"]
  , getProg = loadCurry
  , getPath = loadCurryPath
  , getImports = (\(IProg _ imports _ _) -> imports)
  }

--- Implementation of compiler io.

--- main function
main :: IO()
main = do
  args <- getArgs
  (opts, paths) <- processOptions args
  case paths of
    []        -> error "Input path missing!"
    [i]       -> curry2Go (stripCurrySuffix i) opts
    _         -> error "Too many paths given!"

c2goBanner :: String
c2goBanner = unlines [bannerLine, bannerText, bannerLine]
 where
  bannerText =
    "Curry->Go Compiler (Version " ++ packageVersion ++ ")"
  bannerLine = take (length bannerText) (repeat '-')

--- Compiles a curry program into a go program.
--- @param mainmod - name of main module
--- @param opts    - compiler options 
curry2Go :: String -> CGOptions -> IO ()
curry2Go mainmod opts = do
  printVerb opts 1 c2goBanner
  home <- getHomeDirectory
  let includedir = home ++ [pathSeparator] ++ ".gocurry" ++ [pathSeparator] ++
                  "include"
  printVerb opts 3 $ "Creating directory: " ++ includedir
  createDirectoryIfMissing True includedir
  printVerb opts 1 "Compiling..."
  compile (goStruct {compProg = compileIProg2GoString opts}) mainmod
  printVerb opts 2 $ "Go programs written to " ++ outputDir goStruct
  IProg moduleName _ _ funcs <- icCompile (defaultICOptions {optVerb=0}) mainmod
  when (genMain opts) $ do
    let mainprogname = removeDots moduleName ++ ".go"
    printVerb opts 1 $ "Generating main program '" ++ mainprogname ++ "'"
    let mainprog = showGoProg (createMainProg funcs (opts {modName = "main"}))
    printVerb opts 4 $ "Main Go program:\n\n" ++ mainprog
    let mainfile = combine (outputDir goStruct) mainprogname
    writeFile mainfile mainprog
    printVerb opts 2 $ "...written to " ++ mainfile
  when (genMain opts) $ do
    printVerb opts 1 "Creating executable..."
    let bcmd = "go build " ++ outputDir goStruct ++ [pathSeparator] ++
               removeDots moduleName ++ ".go"
    printVerb opts 2 $ "...with command: " ++ bcmd
    i <- system bcmd
    when (i /= 0) $ error "Build failed!"
    printVerb opts 2 $ "Executable stored in: " ++ removeDots moduleName
  when (run opts) $ do
    printVerb opts 1 "Running..."
    let rcmd = "./" ++ removeDots moduleName
    printVerb opts 2 $ "...with command: " ++ rcmd
    system rcmd
    return ()

--- Turns command line arguments into options and arguments.
processOptions :: [String] -> IO (CGOptions, [String])
processOptions argv = do
  let (funopts, args, opterrors) = getOpt Permute options argv
      opts = foldr (\f x -> f x) defaultCGOptions funopts
  unless (null opterrors)
    (putStr (unlines opterrors) >> putStr usageText >> exitWith 1)
  when (help opts) $ do
    putStr $ c2goBanner ++ "\n" ++ usageText
    exitWith 0
  printArgs argv
  when (printName opts || printNumVer opts || printBaseVer opts) (exitWith 0)
  when (not (genMain opts) && run opts) $
    error "Options 'compile' and 'run' cannot be combined!"
  return (opts, args)
 
--- Prints text for certain compiler flags, that need to be
--- printed in the same order as they were provided.
printArgs :: [String] -> IO ()
printArgs []     = return ()
printArgs (x:xs) = case x of
  "--compiler-name"   -> putStrLn "curry2go" >> printArgs xs
  "--numeric-version" -> putStrLn packageVersion >> printArgs xs
  "--base-version"    -> putStrLn "3.0.0" >> printArgs xs
  _                   -> printArgs xs

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
         "verbosity level:\n0: quiet (same as `-q')\n1: show status messages (default)\n2: show commands (same as `-v')\n3: show intermedate infos\n4: show all details"
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
           (NoArg (\opts -> opts {run = True}))
           "run program after compilation"
  , Option "t" ["time"]
    (OptArg (maybe (\opts -> opts {time = True})
    (\s opts -> opts {time = True, times = safeRead s})) "<n>")
    "print execution time\nn>1: average over runs n"
  , Option "" ["first"]
    (NoArg (\opts -> opts {maxResults = 1}))
    "stop evaluation after the first result"
  , Option "n" ["results"] 
    (ReqArg (\s opts -> opts {maxResults = safeRead s}) "<n>")
    "set maximum number of results to be computed\n(default: 0 = infinite)"
  , Option "i" ["interactive"]
    (NoArg (\opts -> opts {interact = True}))
    "interactive result printing\n(ask to print next result)"
  , Option "m" ["main"]
    (ReqArg (\s opts -> opts {mainName = s}) "<f>")
    "set name of main function to f (default: main)"
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

