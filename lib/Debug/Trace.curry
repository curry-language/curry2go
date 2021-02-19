------------------------------------------------------------------------------
--- This library contains some useful operation for debugging programs.
---
--- @author Bjoern Peemoeller
--- @version September 2014
--- @category general
------------------------------------------------------------------------------

module Debug.Trace
  ( trace, traceId, traceShow, traceShowId, traceIO
  , assert, assertIO
  ) where

import Control.Monad    (unless)
import System.IO        (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

--- Prints the first argument as a side effect and behaves as identity on the
--- second argument.
trace :: String -> a -> a
trace s x = unsafePerformIO (traceIO s >> return x)

--- Prints the first argument as a side effect and returns it afterwards.
traceId :: String -> String
traceId a = trace a a

--- Prints the first argument using `show` and returns the second argument
--- afterwards.
traceShow :: Show a => a -> b -> b
traceShow a b = trace (show a) b

--- Prints the first argument using `show` and returns it afterwards.
traceShowId :: Show a => a -> a
traceShowId a = trace (show a) a

--- Output a trace message from the `IO` monad.
traceIO :: String -> IO ()
traceIO m = hPutStrLn stderr m

--- Assert a condition w.r.t. an error message.
--- If the condition is not met it fails with the given error message,
--- otherwise the third argument is returned.
assert :: Bool -> String -> a -> a
assert cond s x = if cond then x else error s

--- Assert a condition w.r.t. an error message from the `IO` monad.
--- If the condition is not met it fails with the given error message.
assertIO :: Bool -> String -> IO ()
assertIO cond s = unless cond $ error s
