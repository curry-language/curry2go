------------------------------------------------------------------------------
--- A REPL for the Curry->Go compiler based on the universal REPL.
---
--- @author  Michael Hanus
--- @version February 2022
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
  cmpdate <- readFile (c2godir </> "COMPILERDATE") >>= return . head . lines
  mainREPL (c2go c2godir cmpdate)

--- Specification of the Curry2Go compiler (paramterized over the
--- root directory of the Curry2Go compiler):
c2go :: String -> String -> CCDescription
c2go c2goDir cmpdate = CCDescription
  lowerCompilerName          -- the compiler name
  (compilerMajorVersion, compilerMinorVersion, compilerRevisionVersion)
  (c2goBanner cmpdate)       -- the banner
  -- description of specific REPL options:
  [ ("-n|--nocypm",
     "do not invoke `cypm' to compute package load path")
  , ("--noreadline",
     "do not use input line editing via command `rlwrap'")
  ]
  c2goDir                    -- home directory of the compiler
  "info@curry-lang.org"      -- contact email
  frontendpath               -- executable of the Curry front end
  (c2goDir </> "bin" </> "curry2goc") -- compiler executable
  (c2goDir </> "lib")        -- base library path
  Nothing                    -- compile program with load command
  True                       -- use CURRYPATH variable
  (\s -> "-v" ++ s)          -- option to pass verbosity
  parseopts                  -- option to pass parser options
  (\s -> "--compile --nobanner " ++ s) -- option to compile only
  ("--noimports " ++)        -- option to create an executable
  cleanCmd                   -- command to clean module
  [stratOpt, intOpt, firstOpt, ctimeOpt, debugOpt, resultsOpt, errDepthtOpt]
 where
  parseopts s = if null s then "" else "--parse-options=\"" ++ s ++ "\""

  frontendpath = (if curryCompiler == "curry2go" then c2goDir else installDir)
                   </> "bin" </> curryCompiler ++ "-frontend"

  cleanCmd m = unwords
    [ "/bin/rm -rf", quote (curry2goDir </> m) ++ ".*"
    , quote $ modNameToPath m ++ ".curry"
    , quote $ curry2goDir </> m ++ "Main.go", quote $ curry2goDir </> m ]

--- Puts a file argument into quotes to avoid problems with files containing
--- blanks.
quote :: String -> String
quote s = "\"" ++ s ++ "\""

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
  [ ConstOpt "fs"  "--fs"
  , ConstOpt "dfs" "--dfs"
  , ConstOpt "bfs" "--bfs"
  ]

intOpt :: CCOption
intOpt = CCOption
  "+/-interactive "
  "turn on/off interactive evaluation of main expression"
  [ ConstOpt "-interactive" ""
  , ConstOpt "+interactive" "--interactive"
  ]

firstOpt :: CCOption
firstOpt = CCOption
  "+/-first       "
  "turn on/off printing only first value/solution"
  [ ConstOpt "-first" ""
  , ConstOpt "+first" "--first"
  ]

ctimeOpt :: CCOption
ctimeOpt = CCOption
  "+/-ctime       "
  "turn on/off showing compile messages with elapsed time"
  [ ConstOpt "-ctime" ""
  , ConstOpt "+ctime" "--ctime"
  ]

debugOpt :: CCOption
debugOpt = CCOption
  "+/-debug       "
  "turn on/off debugging mode"
  [ ConstOpt "-debug" ""
  , ConstOpt "+debug" "--debug"
  ]

resultsOpt :: CCOption
resultsOpt = CCOption
  "results <n>   "
  "set maximum number of results to be computed\n(default: 0 = unlimited)"
  [ ArgOpt "results" "0" showOpt ]
 where
  showOpt s = case reads s :: [(Int,String)] of
    [(n,"")] | n >= 0 -> Just ("--results=" ++ s)
    _                 -> Nothing

errDepthtOpt :: CCOption
errDepthtOpt = CCOption
  "errdepth <n>   "
  "set print depth of expressions in error messages:\nn>0: last n nodes from error point\nn=0: do not print expressions (default)\nn<0: print complete expression"
  [ ArgOpt "errdepth" "0" showOpt ]
 where
  showOpt s = case reads s :: [(Int,String)] of
    [(_,"")] -> Just ("--errdepth=" ++ s)
    _        -> Nothing

------------------------------------------------------------------------------
