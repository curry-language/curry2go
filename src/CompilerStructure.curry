------------------------------------------------------------------------------
--- This library provides a generic operation `compile` to compile
--- a program module and all its imports with a given compiler
--- (implemented in Curry).
--- The concrete compiler operations used by `compile` are specified
--- with a data type `CompStruct`.
---
--- @author Jonas Boehm, Michael Hanus
--- @version June 2021
------------------------------------------------------------------------------

module CompilerStructure
  ( CompStruct (..), defaultStruct, compile, printWithElapsedTime )
 where

import Control.Monad    ( when )
import Data.IORef

import System.Directory ( createDirectoryIfMissing )
import Data.Time        ( compareClockTime )
import Debug.Profile    -- for show run-time
import System.FilePath  ( combine, takeDirectory )

------------------------------------------------------------------------------
--- Data type for compiler options regarding output structure and modules.
--- It is parameterized over the type `p` of a representation of
--- source programs to be compiled (e.g., FlatCurry or ICurry programs)
--- and the type `s` of an `IORef` keeping some state accross compilation
--- steps, e.g., to cache already loaded modules or interfaces.
data CompStruct p s = CompStruct
  { -- verbosity level of the compilation process:
    -- 0: quiet
    -- 1: show status messages
    -- 2: show commands
    -- 3: show intermedate infos
    -- 4: show all details
    cmpVerbosity   :: Int
    -- print compilation messages with elapsed time?
  , cmpTime        :: Bool
    -- returns the file path to store the compiled target file for a
    -- given module name
  , getTargetFilePath  :: String -> IO String
    -- list of modules to ignore if they appear as an import
  , excludeModules :: [String]
    -- returns a program from a module name
  , getProg        :: IORef s -> String -> IO p
    -- gets the imports of a module
  , getImports     :: IORef s -> String -> IO [String]
    -- returns `True` if the module has already a compilation target which
    -- is newer than the source file (or other intermediate representations)
  , isCompiled     :: String -> IO Bool
    -- compiles a program to a String in the target language
  , compProg       :: (p -> String)
    -- post processing function that runs after compilation/skipping of a module
  , postProc       :: String -> IO ()
  }

--- Default CompStruct.
defaultStruct :: CompStruct a s
defaultStruct = CompStruct
  { cmpVerbosity      = 0
  , cmpTime           = False
  , getTargetFilePath = return
  , excludeModules    = []
  , getProg           = error "Undefined getProg"
  , getImports        = error "Undefined getImports"
  , isCompiled        = \_ -> return False
  , compProg          = error "Undefined compProg"
  , postProc          = \_ -> return ()
  }

printVerb :: CompStruct _ _ -> Int -> String -> IO ()
printVerb struct v s =
  when (cmpVerbosity struct >= v) $ printWithElapsedTime (cmpTime struct) s

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

--- Compiles a program with its imports according to the CompStruct.
--- and compilation function given.
--- @param struct - CompStruct with compilation information
--- @param istate - the initial state used during compilation
--- @param inp    - path to the program to start compilation with
compile :: CompStruct a s -> IORef s -> String -> IO ()
compile struct sref inp = do
  cref <- newIORef (CompState [] [])
  compileProg struct cref sref inp
  cst <- readIORef cref
  printVerb struct 1 $
    "Compilation summary: " ++
    showNumMods (skippedMods cst) ++ " skipped, " ++
    showNumMods (compiledMods cst) ++ " compiled."
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
  fPath <- getTargetFilePath struct name
  let targetdir = takeDirectory fPath
  printVerb struct 3 $ "Creating directory '" ++ targetdir ++ "'..."
  createDirectoryIfMissing True targetdir
  printStatus $ "Processing module '" ++ name ++ "'..."
  impcmpld <- translateImports
  if impcmpld
    then translateProg fPath
    else do
      modcmpld <- isCompiled struct name
      if modcmpld
        then do
          postProc struct name
          addSkippedModule cref name
          printStatus $ "Skipping compilation of '" ++ name ++ "'"
          return False
        else translateProg fPath
 where
  translateImports = do
    impmods <- getImports struct sref name
    printVerb struct 2 $ "Imports of '" ++ name ++ "': " ++ unwords impmods
    if null impmods
      then return False
      else compileImports struct cref sref impmods

  translateProg targetpath = do
    printStatus $ "Compiling module '" ++ name ++ "'" ++
      (if cmpVerbosity struct > 1 then " to '" ++ targetpath ++ "'" else "") ++
      "..."
    prog <- getProg struct sref name
    let target = compProg struct prog
    writeFile targetpath target
    printVerb struct 2 $ "'" ++ name ++ "' compiled!"
    printVerb struct 4 $ "Compiled target program:\n" ++ target
    postProc struct name
    addCompiledModule cref name
    return True

  printStatus = printVerb struct 1


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

------------------------------------------------------------------------------
-- Auxiliaries

--- Prints a given string. If the first argument is `True`,
--- add also the elapsed time in front of the printed string.
printWithElapsedTime :: Bool -> String -> IO ()
printWithElapsedTime withtime s =
  if withtime
    then do
      runtime <- getProcessInfos >>= return . maybe 0 id . lookup ElapsedTime
      putStrLn $ "[" ++ showTime runtime ++ "s] " ++ s
    else putStrLn s
 where
  showTime t = show (t `div` 1000) ++ "." ++ show2 ((t `mod` 1000) `div` 10)
  show2 i = if i < 10 then '0' : show i else show i

------------------------------------------------------------------------------
