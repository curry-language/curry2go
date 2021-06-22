------------------------------------------------------------------------------
--- Get infos about the installation path (root directory) of the
--- Curry2Go system.
---
--- @author  Michael Hanus
--- @version June 2021
------------------------------------------------------------------------------

module Curry2Go.InstallPath ( curry2GoHomeDir )
 where

import System.Environment ( getEnv )
import Curry2Go.PkgConfig ( packagePath )

--- Returns the installation directory of the Curry2Go system
--- (either the environment variable `CURRY2GOHOME` or the package path).
curry2GoHomeDir :: IO String
curry2GoHomeDir = do
  envhome <- getEnv "CURRY2GOHOME"
  if null envhome
    then return packagePath
    else return envhome
