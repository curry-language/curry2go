-----------------------------------------------------------------------------
--- Library for IO operations like reading and writing files
--- that are not already contained in the prelude.
---
--- @author Michael Hanus, Bernd Brassel
--- @version March 2021
-----------------------------------------------------------------------------

module System.IO
  ( Handle, IOMode(..), SeekMode(..), stdin, stdout, stderr
  , openFile, hClose, hFlush, hIsEOF, isEOF
  , hSeek, hWaitForInput, hWaitForInputs, hReady
  , hGetChar, hGetLine, hGetContents, getContents
  , hPutChar, hPutStr, hPutStrLn, hPrint
  , hIsReadable, hIsWritable, hIsTerminalDevice
  ) where

import Data.Either

--- The abstract type of a handle for a stream.
external data Handle -- internally defined

instance Eq Handle where
  h1 == h2 = (handle_eq $# h2) $# h1

handle_eq :: Handle -> Handle -> Bool
handle_eq external

--- The modes for opening a file.
data IOMode = ReadMode | WriteMode | AppendMode

--- The modes for positioning with `hSeek` in a file.
data SeekMode = AbsoluteSeek | RelativeSeek | SeekFromEnd


--- Standard input stream.
stdin :: Handle
stdin external

--- Standard output stream.
stdout :: Handle
stdout external

--- Standard error stream.
stderr :: Handle
stderr external

--- Opens a file in specified mode and returns a handle to it.
openFile :: String -> IOMode -> IO Handle
openFile filename mode = (prim_openFile $## filename) $# mode

prim_openFile :: String -> IOMode -> IO Handle
prim_openFile external

--- Closes a file handle and flushes the buffer in case of output file.
hClose :: Handle -> IO ()
hClose h = prim_hClose $# h

prim_hClose :: Handle -> IO ()
prim_hClose external

--- Flushes the buffer associated to handle in case of output file.
hFlush :: Handle -> IO ()
hFlush h = prim_hFlush $# h

prim_hFlush :: Handle -> IO ()
prim_hFlush external

--- Is handle at end of file?
hIsEOF :: Handle -> IO Bool
hIsEOF h = prim_hIsEOF $# h

prim_hIsEOF :: Handle -> IO Bool
prim_hIsEOF external

--- Is standard input at end of file?
isEOF :: IO Bool
isEOF = hIsEOF stdin


--- Set the position of a handle to a seekable stream (e.g., a file).
--- If the second argument is `AbsoluteSeek`,
--- `SeekFromEnd`, or `RelativeSeek`,
--- the position is set relative to the beginning of the file,
--- to the end of the file, or to the current position, respectively.
hSeek :: Handle -> SeekMode -> Int -> IO ()
hSeek h sm pos = ((prim_hSeek $# h) $# sm) $# pos

prim_hSeek :: Handle -> SeekMode -> Int -> IO ()
prim_hSeek external


--- Waits until input is available on the given handle.
--- If no input is available within t milliseconds, it returns False,
--- otherwise it returns True.
--- @param handle - a handle for an input stream
--- @param timeout - milliseconds to wait for input (< 0 : no time out)
hWaitForInput :: Handle -> Int -> IO Bool
hWaitForInput handle timeout = (prim_hWaitForInput $# handle)  $## timeout

prim_hWaitForInput :: Handle -> Int -> IO Bool
prim_hWaitForInput external

--- Waits until input is available on some of the given handles.
--- If no input is available within t milliseconds, it returns -1,
--- otherwise it returns the index of the corresponding handle with the available
--- data.
--- @param handles - a list of handles for input streams
--- @param timeout - milliseconds to wait for input (< 0 : no time out)
--- @return -1 if no input is available within the time out, otherwise i
---         if (handles!!i) has data available
hWaitForInputs :: [Handle] -> Int -> IO Int
hWaitForInputs handles timeout = (prim_hWaitForInputs $## handles) $## timeout

prim_hWaitForInputs :: [Handle] -> Int -> IO Int
prim_hWaitForInputs external


--- Checks whether an input is available on a given handle.
hReady :: Handle -> IO Bool
hReady h = hWaitForInput h 0


--- Reads a character from an input handle and returns it.
--- Throws an error if the end of file has been reached.
hGetChar    :: Handle -> IO Char
hGetChar h = prim_hGetChar $# h

prim_hGetChar :: Handle -> IO Char
prim_hGetChar external

--- Reads a line from an input handle and returns it.
--- Throws an error if the end of file has been reached while reading
--- the *first* character. If the end of file is reached later in the line,
--- it ist treated as a line terminator and the (partial) line is returned.
hGetLine  :: Handle -> IO String
hGetLine h = do c  <- hGetChar h
                if c == '\n'
                   then return []
                   else do eof <- hIsEOF h
                           if eof then return [c]
                                  else do cs <- hGetLine h
                                          return (c:cs)


--- Reads the complete contents from an input handle and closes the input handle
--- before returning the contents.
hGetContents  :: Handle -> IO String
hGetContents h = do
  eof <- hIsEOF h
  if eof then hClose h >> return ""
         else do c <- hGetChar h
                 cs <- hGetContents h
                 return (c:cs)

--- Reads the complete contents from the standard input stream until EOF.
getContents  :: IO String
getContents = hGetContents stdin

--- Puts a character to an output handle.
hPutChar    :: Handle -> Char -> IO ()
hPutChar h c = (prim_hPutChar $# h)  $## c

prim_hPutChar :: Handle -> Char -> IO ()
prim_hPutChar external

--- Puts a string to an output handle.
hPutStr :: Handle -> String -> IO ()
hPutStr _ []     = return ()
hPutStr h (c:cs) = hPutChar h c >> hPutStr h cs

--- Puts a string with a newline to an output handle.
hPutStrLn :: Handle -> String -> IO ()
hPutStrLn h s = hPutStr h s >> hPutChar h '\n'

--- Converts a term into a string and puts it to an output handle.
hPrint :: Show a => Handle -> a -> IO ()
hPrint h = hPutStrLn h . show


--- Is the handle readable?
hIsReadable :: Handle -> IO Bool
hIsReadable  h = prim_hIsReadable $# h

prim_hIsReadable :: Handle -> IO Bool
prim_hIsReadable external

--- Is the handle writable?
hIsWritable :: Handle -> IO Bool
hIsWritable h = prim_hIsWritable $# h

prim_hIsWritable :: Handle -> IO Bool
prim_hIsWritable external

--- Is the handle connected to a terminal?
hIsTerminalDevice :: Handle -> IO Bool
hIsTerminalDevice h = prim_hIsTerminalDevice $# h

prim_hIsTerminalDevice :: Handle -> IO Bool
prim_hIsTerminalDevice external
