------------------------------------------------------------------------------
--- Configurations (name, versions,...) of the compiler.
---
--- @author  Michael Hanus
--- @version March 2021
------------------------------------------------------------------------------

module Curry2Go.Config
  ( compilerName, lowerCompilerName, upperCompilerName
  , compilerMajorVersion, compilerMinorVersion, compilerRevisionVersion
  , curry2goDir
  ) where

import Data.Char ( toLower, toUpper )
import Data.List ( intercalate, splitOn )

import System.FilePath    ( (</>) )

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

--- The major version of the compiler.
compilerRevisionVersion :: Int
compilerRevisionVersion = case reads ((splitOn "." packageVersion) !! 2) of
  [(n,_)] -> n -- maybe there is a suffix with a pre-release identifier
  _       -> 0

--- The subdirectory where intermediate program files and the compiled
--- Go target files will be stored, e.g., `.curry/curry2go-1.0.0`.
curry2goDir :: String
curry2goDir =
  ".curry" </> lowerCompilerName ++ "-" ++
  intercalate "."
    (map show [compilerMajorVersion, compilerMinorVersion,
               compilerRevisionVersion])
