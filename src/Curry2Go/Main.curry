module Curry2Go.Main where

import Curry2Go.Compiler
import Go.Show
import Go.Types
import CompilerStructure
import ICurry.Types
import ICurry.Compiler
import System
import System.CurryPath
import GetOpt
import Directory
import Char
import FilePath
import List
import System.FrontendExec
import FlatCurry.Files

--- Implementation of CompStruct for the curry2go compiler.

--- Gets the path to the source file of a curry module.
loadCurryPath :: String -> IO String
loadCurryPath inp = lookupModuleSourceInLoadPath inp >>= (\path -> case path of
                      Nothing          -> error ("Unknown module " ++ inp)
                      Just (dir, file) -> return (combine dir file))

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
  getExtFilePath =
    loadCurryPath inp
    >>= (\path -> return (replaceFileName path 
      ("external_" ++ takeFileName (replaceExtension path "go"))))
  

goStruct :: CompStruct IProg
goStruct = defaultStruct
  { outputDir      = ".gocurry"
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
  (opts, paths) <- (processOptions args)
  case paths of
    []        -> error "Input path missing!"
    [i]       -> curry2Go i opts
    _         -> error "Too many paths given!"

--- Compiles a curry program into a go program.
--- @param inp  - path to curry program
--- @param opts - compiler options 
curry2Go :: String -> CGOptions -> IO()
curry2Go inp opts = do
  home <- getHomeDirectory
  createDirectoryIfMissing True 
    (home ++ [pathSeparator] ++ ".gocurry" ++ [pathSeparator] ++ "include")
  putStrLn "Compiling..."
  compile (goStruct {compProg = compileIProg2GoString opts}) inp
  IProg moduleName _ _ funcs <- (icCompile (defaultICOptions {optVerb=0}) inp)
  when (genMain opts) (putStrLn "Generating Main"
    >> (writeFile (".gocurry/" ++ removeDots moduleName ++ "Main.go")
      (showGoProg (createMainProg funcs (opts {modName = "main"})))))
  putStrLn "Saved to ./.gocurry!"
  if (run opts) then do
    putStrLn "Building..."
    i <- system ("go build .gocurry/" ++ removeDots moduleName ++ "Main.go")
    when (i /= 0) (error "Build failed!")
    putStrLn "Running..."
    system ("./" ++ removeDots moduleName ++ "Main")
    return ()
                else return ()

--- Turns command line arguments into options and arguments.
processOptions :: [String] -> IO (CGOptions, [String])
processOptions argv = do
  let (funopts, args, opterrors) = getOpt Permute options argv
      opts = foldr (\f x -> f x) defaultCGOptions funopts
  unless (null opterrors)
    (putStr (unlines opterrors) >> (putStr usageText) >> (exitWith 1))
  when (help opts) ((putStr usageText) >> (exitWith 0))
  return (opts, args)

--- Help text
usageText :: String
usageText = usageInfo "Usage: curry2go [options] <input>\n" options

--- Definition of command line options.
options :: [OptDescr (CGOptions -> CGOptions)]
options = 
  [ Option "h?" ["help"]
    (NoArg (\opts -> opts {help = True})) "print help and exit"
  , Option ""   ["nomain"] 
    (NoArg (\opts -> opts {genMain = False})) "don't generate a main package"
  , Option ""   ["bfs"]
    (NoArg (\opts -> opts {strat = BFS})) "use breadth first search"
  , Option ""   ["fs"]
    (OptArg (maybe (\opts -> opts {strat = FS}) 
    (\s opts -> opts {strat = FS, maxTasks = (safeRead (reads s))})) "<n>") 
    "use fair search. n = maximum number of concurrent computations. (0 = infinite)"
  , Option "n" ["results"] 
    (ReqArg (\s opts -> opts {maxResults = (safeRead (reads s))}) "<n>")
    "maximum number of results. (0 = infinite)"
  , Option "r" ["run"]
    (NoArg (\opts -> opts {run = True})) "run the program after compilation"
  , Option "t" ["time"]
    (OptArg (maybe (\opts -> opts {time = True})
    (\s opts -> opts {time = True, times = (safeRead (reads s))})) "<n>")
    "Average the time over the number of runs n."
  , Option "i" ["interactive"]
    (NoArg (\opts -> opts {interact = True}))
    "Interactive result printing"
  ]
 where
  safeRead result = case result of
    [(n,"")] -> n
    _        -> error "Invalid argument! Use -h for help."
