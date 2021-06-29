------------------------------------------------------------------------------
--- A REPL for the Curry->Go compiler based on the universal REPL.
---
--- @author  Michael Hanus
--- @version June 2021
------------------------------------------------------------------------------

module Curry2Go.REPL where

import Curry.Compiler.Distribution ( curryCompiler, installDir )
import Data.List          ( intercalate )

import REPL.Compiler
import REPL.Main          ( mainREPL )
import System.CurryPath   ( inCurrySubdir, modNameToPath, sysLibPath )
import System.FilePath    ( (</>) )

import Curry2Go.Config      ( compilerName, lowerCompilerName, curry2goDir
                            , compilerMajorVersion, compilerMinorVersion
                            , compilerRevisionVersion )
import Curry2Go.InstallPath ( curry2GoHomeDir )
import Curry2Go.PkgConfig   ( packageVersion )

main :: IO ()
main = do
  c2godir <- curry2GoHomeDir
  cmpdate <- readFile (c2godir </> "COMPDATE") >>= return . head . lines
  mainREPL (c2go c2godir cmpdate)

--- Specification of the Curry2Go compiler (paramterized over the
--- root directory of the Curry2Go compiler):
c2go :: String -> String -> CCDescription
c2go c2goDir cmpdate = CCDescription
  lowerCompilerName          -- the compiler name
  (compilerMajorVersion, compilerMinorVersion, compilerRevisionVersion)
  (c2goBanner cmpdate)       -- the banner
  c2goDir                    -- home directory of the compiler
  "info@curry-lang.org"      -- contact email
  frontendpath               -- executable of the Curry front end
  (c2goDir </> "bin" </> "curry2goc") -- compiler executable
  (c2goDir </> "lib")        -- base library path
  Nothing                    -- compile program with load command
  True                       -- use CURRYPATH variable
  (\s -> "-v" ++ s)          -- option to pass verbosity
  (\_ -> "")                 -- option to pass parser options (ignored)
  (\s -> "--compile --nobanner " ++ s) -- option to compile only
  ("--noimports " ++)        -- option to create an executable
  cleanCmd                   -- command to clean module
  [stratOpt, intOpt, firstOpt, ctimeOpt]
 where
  frontendpath = (if curryCompiler == "curry2go" then c2goDir else installDir)
                   </> "bin" </> curryCompiler ++ "-frontend"

  cleanCmd m = unwords
    [ "/bin/rm -rf", curry2goDir </> m ++ ".*", modNameToPath m ++ ".curry"
    , curry2goDir </> m ++ "Main.go", curry2goDir </> m ]

c2goBanner :: String -> String
c2goBanner cmpdate = unlines [bannerLine, bannerText, bannerLine]
 where
  bannerText = compilerName ++ " Interactive Environment (Version " ++
               packageVersion ++ " of " ++ cmpdate ++ ")"
  bannerLine = take (length bannerText) (repeat '-')

stratOpt :: CCOption
stratOpt = CCOption
  "fs/dfs/bfs     "
  "search strategy (fair / depth-first / breadth-first)"
  [ ("fs" ,"--fs")
  , ("dfs","--dfs")
  , ("bfs","--bfs")
  ]

intOpt :: CCOption
intOpt = CCOption
  "+/-interactive "
  "turn on/off interactive evaluation of main expression"
  [ ("-interactive","")
  , ("+interactive","--interactive")
  ]

firstOpt :: CCOption
firstOpt = CCOption
  "+/-first       "
  "turn on/off printing only first value/solution"
  [ ("-first","")
  , ("+first","--first")
  ]

ctimeOpt :: CCOption
ctimeOpt = CCOption
  "+/-ctime       "
  "turn on/off showing compile messages with elapsed time"
  [ ("-ctime","")
  , ("+ctime","--ctime")
  ]

------------------------------------------------------------------------------
