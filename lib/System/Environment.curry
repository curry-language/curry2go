------------------------------------------------------------------------------
--- Library to access parts of the system environment.
---
--- @author Michael Hanus, Bernd Brassel, Bjoern Peemoeller
--- @version November 2020
--- @category general
------------------------------------------------------------------------------

module System.Environment
  ( getArgs, getEnv, setEnv, unsetEnv, getProgName
  , getHostname, isPosix, isWindows
  ) where

--- Returns the list of the program's command line arguments.
--- The program name is not included.

getArgs :: IO [String]
getArgs external

--- Returns the value of an environment variable.
--- The empty string is returned for undefined environment variables.

getEnv :: String -> IO String
getEnv evar = prim_getEnviron $## evar

prim_getEnviron :: String -> IO String
prim_getEnviron external

--- Set an environment variable to a value.
--- The new value will be passed to subsequent shell commands
--- (see <code>system</code>) and visible to subsequent calls to
--- <code>getEnv</code> (but it is not visible in the environment
--- of the process that started the program execution).

setEnv :: String -> String -> IO ()
setEnv evar val = (prim_setEnviron $## evar) $## val

prim_setEnviron :: String -> String -> IO ()
prim_setEnviron external

--- Removes an environment variable that has been set by
--- <code>setEnv</code>.

unsetEnv :: String -> IO ()
unsetEnv evar = prim_unsetEnviron $## evar

prim_unsetEnviron :: String -> IO ()
prim_unsetEnviron external

--- Returns the hostname of the machine running this process.

getHostname :: IO String
getHostname external

--- Returns the name of the current program, i.e., the name of the
--- main module currently executed.

getProgName :: IO String
getProgName external

--- Is the underlying operating system a POSIX system (unix, MacOS)?
isPosix :: Bool
isPosix = not isWindows

--- Is the underlying operating system a Windows system?
isWindows :: Bool
isWindows external
