------------------------------------------------------------------------------
--- Library with some useful functions on characters.
---
--- @author Michael Hanus, Bjoern Peemoeller
--- @version January 2015
--- @category general
------------------------------------------------------------------------------

module Data.Char
  ( isAscii, isLatin1, isAsciiUpper, isAsciiLower, isControl
  , isUpper, isLower, isAlpha, isDigit, isAlphaNum
  , isBinDigit, isOctDigit, isHexDigit, isSpace
  , toUpper, toLower, digitToInt, intToDigit
  , ord, chr
  ) where

--- Returns true if the argument is an ASCII character.
isAscii        :: Char -> Bool
isAscii c      =  c < '\x80'

--- Returns true if the argument is an Latin-1 character.
isLatin1       :: Char -> Bool
isLatin1 c     =  c < '\xff'

--- Returns true if the argument is an ASCII lowercase letter.
isAsciiLower    :: Char -> Bool
isAsciiLower c  =  c >= 'a' && c <= 'z'

--- Returns true if the argument is an ASCII uppercase letter.
isAsciiUpper    :: Char -> Bool
isAsciiUpper c  =  c >= 'A' && c <= 'Z'

--- Returns true if the argument is a control character.
isControl       :: Char -> Bool
isControl c     =  c < '\x20' || c >= '\x7f' && c <= '\x9f'

--- Converts lowercase into uppercase letters.
toUpper         :: Char -> Char
toUpper c       |  isLower c = chr (ord c - ord 'a' + ord 'A')
                |  otherwise = c

--- Converts uppercase into lowercase letters.
toLower         :: Char -> Char
toLower c       |  isUpper c = chr (ord c - ord 'A' + ord 'a')
                |  otherwise = c

--- Converts a (hexadecimal) digit character into an integer.
digitToInt      :: Char -> Int
digitToInt c
  | isDigit c
  =  ord c - ord '0'
  | ord c >= ord 'A' && ord c <= ord 'F'
  =  ord c - ord 'A' + 10
  | ord c >= ord 'a' && ord c <= ord 'f'
  =  ord c - ord 'a' + 10
  | otherwise
  =  error "Char.digitToInt: argument is not a digit"

--- Converts an integer into a (hexadecimal) digit character.
intToDigit      :: Int -> Char
intToDigit i
  | i >= 0  && i <=  9  =  chr (ord '0' + i)
  | i >= 10 && i <= 15  =  chr (ord 'A' + i - 10)
  | otherwise           =  error "Char.intToDigit: argument not a digit value"
