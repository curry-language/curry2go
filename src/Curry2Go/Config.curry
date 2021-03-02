------------------------------------------------------------------------------
--- Configurations (name, versions,...) of the compiler.
---
--- @author  Michael Hanus
--- @version March 2021
------------------------------------------------------------------------------

module Curry2Go.Config where

import Data.Char ( toLower, toUpper )
import Data.List ( splitOn )

import Curry2Go.PkgConfig ( packageVersion )

--- The name of the compiler (used at various places, e.g., lowercase
--- in CPM package specifations and for directories with intermediate files,
--- uppercase for conditional compiling,...).
compilerName :: String
compilerName = "Curry2Go"

lowerCompilerName :: String
lowerCompilerName = map toLower compilerName

upperCompilerName :: String
upperCompilerName = map toUpper compilerName

--- The major version of the compiler.
compilerMajorVersion :: Int
compilerMajorVersion = case reads (head (splitOn "." packageVersion)) of
  [(n,"")] -> n
  _        -> 0

--- The major version of the compiler.
compilerMinorVersion :: Int
compilerMinorVersion = case reads ((splitOn "." packageVersion) !! 1) of
  [(n,"")] -> n
  _        -> 0
