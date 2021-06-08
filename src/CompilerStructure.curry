module CompilerStructure
  ( CompStruct (..), defaultStruct, getFilePath, getFileDir, compile )
 where

import Control.Monad    ( when )
import Data.IORef

import System.Directory ( createDirectoryIfMissing, doesFileExist
                        , getModificationTime)
import Data.Time        ( compareClockTime )
import System.FilePath  ( combine, takeDirectory )

--- Data type for compiler options regarding output structure and modules.
--- It is parameterized over the type of programs to be compiled
--- and the type of an `IORef` keeping some state accross compilation steps,
--- e.g., to cache already loaded modules or interfaces.
--- The compiled version of a module `m` will be saved as
--- `outputDir </> (filePath m)`.
data CompStruct p s = CompStruct
  { -- verbosity level of the compilation process:
    -- 0: quiet
    -- 1: show status messages
    -- 2: show commands
    -- 3: show intermedate infos
    -- 4: show all details
    cmpVerbosity   :: Int
    -- directory where all compiled files will be saved
  , outputDir      :: String
    -- function taking a module name and converting it to a path
    -- for the compiled version
  , filePath       :: String -> IO String
    -- list of modules to ignore if they appear as an import
  , excludeModules :: [String]
    -- returns a program from a module name
  , getProg        :: IORef s -> String -> IO p
    -- returns the path to the source file of a program
  , getPath        :: String -> IO String
    -- gets the imports of a module
  , getImports     :: IORef s -> String -> IO [String]
    -- compiles a program to a String in the target language
  , compProg       :: (p -> String)
    -- post processing function that runs after compilation/skipping of a module
  , postProc       :: String -> IO ()
  }

--- Default CompStruct.
defaultStruct :: CompStruct a s
defaultStruct = CompStruct
  { cmpVerbosity   = 0
  , outputDir      = ""
  , filePath       = return
  , excludeModules = []
  , getProg        = error "Undefined getProg"
  , getPath        = error "Undefined getPath"
  , getImports     = error "Undefined getImports"
  , compProg       = error "Undefined compProg"
  , postProc       = \_ -> return ()
  }

printStatus :: CompStruct _ _ -> String -> IO ()
printStatus struct s = if cmpVerbosity struct == 0 then return ()
                                                   else putStr s

------------------------------------------------------------------------------
-- Compiler state with lists of already compiled and skipped modules.
data CompState =
  CompState { compiledMods :: [String]
            , skippedMods  :: [String]
            }

-- Adds a module to the list of already compiled modules.
addCompiledModule :: IORef CompState -> String -> IO ()
addCompiledModule cref mname = do
  cstate <- readIORef cref
  writeIORef cref (cstate { compiledMods = mname : compiledMods cstate })

-- Adds a module to the list of already skipped modules.
addSkippedModule :: IORef CompState -> String -> IO ()
addSkippedModule cref mname = do
  cstate <- readIORef cref
  writeIORef cref (cstate { skippedMods = mname : skippedMods cstate })

------------------------------------------------------------------------------

--- Returns the path to the file containing the compiled version of a program.
--- @param path - PathStruct containing directory information
--- @param name - name of the curry module
getFilePath :: CompStruct a s -> String -> IO String
getFilePath struct name = do fPath <- filePath struct name
                             return (combine (outputDir struct) fPath)

--- Returns the directory where the compiled version of a program will be saved.
--- @param path - PathStruct containing directory information
--- @param name - name of the curry module
getFileDir :: CompStruct a s -> String -> IO String
getFileDir struct name = do fPath <- getFilePath struct name
                            return (takeDirectory fPath)

--- Compiles a program with its imports according to the CompStruct.
--- and compilation function given.
--- @param struct - CompStruct with compilation information
--- @param istate - the initial state used during compilation
--- @param inp    - path to the program to start compilation with
compile :: CompStruct a s -> IORef s -> String -> IO ()
compile struct sref inp = do
  createDirectoryIfMissing True (outputDir struct)
  cref <- newIORef (CompState [] [])
  compileProg struct cref sref inp
  cst <- readIORef cref
  printStatus struct $
    "Compilation summary: " ++
    showNumMods (skippedMods cst) ++ " skipped, " ++
    showNumMods (compiledMods cst) ++ " compiled.\n"
  return ()
 where
  showNumMods ms =
    let l = length ms
    in (if l == 0 then "no" else show l) ++
       " module" ++ (if l > 1 then "s" else "")

--- Calls the compilation function on a program and 
--- saves the output according to the given CompStruct,
--- unless it has not been modified since the last compilation.
--- @param struct  - CompStruct with compilation information
--- @param cref    - IORef for the compiler state
--- @param sref    - IORef for the compilation cache
--- @param name    - name of the program to compile
--- @return        - Bool indicating whether
---                - the program was (re)compiled(True) or not(False).
compileProg :: CompStruct a s -> IORef CompState -> IORef s -> String
            -> IO Bool
compileProg struct cref sref name = do
  fDir  <- getFileDir struct name
  fPath <- getFilePath struct name
  createDirectoryIfMissing True fDir
  printStatus struct $ "Processing module '" ++ name ++ "'..."
  alreadyExists <- doesFileExist fPath
  impcmpld  <- translateImports
  if alreadyExists
    then do
      modulePath  <- getPath struct name
      lastMod     <- getModificationTime modulePath
      lastCompile <- getModificationTime fPath
      if compareClockTime lastMod lastCompile == GT || impcmpld
        then translateProg fPath
        else do
          postProc struct name
          addSkippedModule cref name
          printStatusLn $ "Skipping compilation of '" ++ name ++ "'"
          return False
    else translateProg fPath
 where
  translateImports = do
    impmods <- getImports struct sref name
    printStatusLn $ "imports: " ++ unwords impmods
    if null impmods
      then return False
      else compileImports struct cref sref impmods

  translateProg targetpath = do
    printStatus struct $ "Compiling module '" ++ name ++ "'..."
    prog <- getProg struct sref name
    let target = compProg struct prog
    if cmpVerbosity struct > 1
      then printStatusLn $ "to: " ++ targetpath
      else printStatusLn "done"
    when (cmpVerbosity struct > 3) $
      putStrLn $ "Compiled target program:\n" ++ target
    writeFile targetpath target
    postProc struct name
    addCompiledModule cref name
    return True

  printStatusLn s = printStatus struct (s ++ "\n")


--- Calls compileProg on every imported module 
--- if it is not in the excludedModules list.
--- @param struct  - CompStruct with compilation information
--- @param cref    - IORef for the compiler state
--- @param sref    - IORef for the compilation cache
--- @param imports - list of imported modules
--- @return        - Bool indicating whether any
---                - import was (re)compiled(True) or not(False).
compileImports :: CompStruct a s -> IORef CompState -> IORef s -> [String]
               -> IO Bool
compileImports _      _    _    []     = return False
compileImports struct cref sref (x:xs) 
  | elem x (excludeModules struct) = compileImports struct cref sref xs
  | otherwise                      = do
    b <- compileImports struct cref sref xs
    cstate <- readIORef cref
    if x `elem`  compiledMods cstate
      then return True
      else if x `elem` skippedMods cstate
             then return b
             else do b2 <- compileProg struct cref sref x
                     return (b || b2)
