module CompilerStructure (CompStruct (..), defaultStruct, getFilePath, getFileDir, compile) where

import System.Directory
import Data.Time
import ICurry.Types
import ICurry.Compiler
import FlatCurry.Files
import System.FilePath
import System.CurryPath
import System.FrontendExec
import Global

--- Data type for compiler options regarding output structure and modules.
--- The compiled version of a module m will be saved as outputDir/(filePath m).
data CompStruct a = CompStruct
  { outputDir       :: String                -- directory where all compiled files will be saved.
  , filePath        :: (String -> String)    -- function taking a module name, 
                                             -- converting it to a path for the compiled version.
  , excludeModules  :: [String]              -- list of modules to ignore if they appear as an import.
  , getProg         :: (String -> IO a)      -- returns a program from a module name.
  , getPath         :: (String -> IO String) -- returns the path to the source file of a program.
  , getImports      :: (a -> [String])       -- returns the names of imported programs.
  , compProg            :: (a -> String)         -- compiles a program to a String in the target language.
  }

--- Default CompStruct.
defaultStruct :: CompStruct a
defaultStruct = CompStruct
  { outputDir = ""
  , filePath  = id
  , excludeModules = []
  , getProg  =  \_ -> error "Undefined getProg"
  , getPath  =  \_ -> error "Undefined getPath"
  , getImports = \_ -> error "Undefined getImports"
  , compProg = \_ -> error "Undefined compProg"
  }

--- List of already compiled modules.
compiledModules :: Global [String]
compiledModules = global [] Temporary

--- Returns the path to the file containing the compiled version of a program.
--- @param path - PathStruct containing directory information
--- @param name - name of the curry module
getFilePath :: CompStruct a -> String -> String
getFilePath struct name = combine (outputDir struct) (filePath struct name)

--- Returns the directory where the compiled version of a program will be saved.
--- @param path - PathStruct containing directory information
--- @param name - name of the curry module
getFileDir :: CompStruct a -> String -> String
getFileDir struct name = takeDirectory (getFilePath struct name)


--- Compiles a program with its imports according to the CompStruct.
--- and compilation function given.
--- @param struct - CompStruct with compilation information
--- @param quiet  - no printing of status information?
--- @param inp    - path to the program to start compilation with
compile :: CompStruct a -> Bool -> String -> IO ()
compile struct quiet inp = do
  createDirectoryIfMissing True (outputDir struct)
  compileProg struct quiet inp
  return ()

--- Calls the compilation function on a program and 
--- saves the output according to the given CompStruct,
--- unless it has not been modified since the last compilation.
--- @param struct  - CompStruct with compilation information
--- @param quiet   - no printing of status information?
--- @param name    - name of the program to compile
--- @return        - Bool indicating whether
---                - the program was (re)compiled(True) or not(False).
compileProg :: CompStruct a -> Bool -> String -> IO Bool
compileProg struct quiet name =
  do createDirectoryIfMissing True (getFileDir struct name)
     prog <- getProg struct name
     alreadyExists <- doesFileExist (getFilePath struct name)
     if alreadyExists
       then do
         modulePath <- getPath struct name
         lastMod <- getModificationTime modulePath
         lastCompile <- getModificationTime (getFilePath struct name)
         compAnyway <- compileImports struct quiet (getImports struct prog)
         if compareClockTime lastMod lastCompile == GT || compAnyway
           then do
             writeFile (getFilePath struct name) (compProg struct prog)
             compModules <- readGlobal compiledModules 
             writeGlobal compiledModules (name:compModules)
             printStatus ("Compiled " ++ name) >> return True
                 else printStatus ("Skipping " ++ name) >> return False
       else do
         compileImports struct quiet (getImports struct prog)
         writeFile (getFilePath struct name) (compProg struct prog)
         compModules <- readGlobal compiledModules 
         writeGlobal compiledModules (name:compModules)
         printStatus ("Compiled " ++ name) >> return True
 where
  printStatus s = if quiet then return () else putStrLn s

--- Calls compileProg on every imported module 
--- if it is not in the excludedModules list.
--- @param struct  - CompStruct with compilation information
--- @param quiet   - no printing of status information?
--- @param imports - list of imported modules
--- @return        - Bool indicating whether any
---                - import was (re)compiled(True) or not(False).
compileImports :: CompStruct a -> Bool -> [String] -> IO Bool
compileImports _      _     []     = return False
compileImports struct quiet (x:xs) 
  | elem x (excludeModules struct) = compileImports struct quiet xs
  | otherwise                    = do
    b <- compileImports struct quiet xs
    compMods <- readGlobal compiledModules
    if elem x compMods then return True
                       else compileProg struct quiet x
                            >>= (\b2 -> return (b || b2))
