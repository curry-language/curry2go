------------------------------------------------------------------------------
--- A REPL for the Curry->Go compiler based on the universal REPL.
---
--- @author  Michael Hanus
--- @version March 2021
------------------------------------------------------------------------------

module Curry2Go.REPL where

import Data.List          ( intercalate )

import REPL.Compiler
import REPL.Main          ( mainREPL )
import System.CurryPath   ( inCurrySubdir, modNameToPath, sysLibPath )
import System.FilePath    ( (</>) )

import Curry2Go.Config    ( compilerName, lowerCompilerName, curry2goDir )
import Curry2Go.PkgConfig ( packageExecutables, packagePath, packageVersion )

main :: IO ()
main = mainREPL c2go

--- Specification of the Curry->Go compiler:

c2go :: CCDescription
c2go = CCDescription
  lowerCompilerName          -- the compiler name
  c2goBanner                 -- the banner
  c2goHome                   -- home directory of the compiler
  "info@curry-lang.org"      -- contact email
  (head packageExecutables)  -- compiler executable
  (c2goHome ++ "/lib")       -- base library path
  False                      -- parser should read untyped FlatCurry
  True                       -- use CURRYPATH variable
  (\s -> "-v" ++ s)          -- option to pass verbosity
  (\_ -> "")                 -- option to pass parser options (ignored)
  (\s -> "--compile " ++ s)  -- option to compile only
  (\s -> s)                  -- option to create an executable
  cleanCmd                   -- command to clean module
  [stratOpt, intOpt, firstOpt]
 where
  cleanCmd m = unwords
    [ "/bin/rm -rf", inCurrySubdir m ++ ".*", modNameToPath m ++ ".curry"
    , curry2goDir </> m ++ ".go", curry2goDir </> m ]

c2goHome :: String
c2goHome = packagePath

c2goBanner :: String
c2goBanner = unlines [bannerLine, bannerText, bannerLine]
 where
  bannerText = compilerName ++ " Interactive Environment (Version " ++
               packageVersion ++ ")"
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

------------------------------------------------------------------------------