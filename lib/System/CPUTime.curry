module System.CPUTime where

--- Returns the current cpu time of the process in milliseconds.
getCPUTime :: IO Int
getCPUTime external

--- Returns the current elapsed time of the process in milliseconds.
--- This operation is not supported in KiCS2 (there it always returns 0),
--- but only included for compatibility reasons.
getElapsedTime :: IO Int
getElapsedTime external
