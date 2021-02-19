------------------------------------------------------------------------------
--- Library with some functions for reading and converting numeric tokens.
--
--- @author Michael Hanus, Frank Huch, Bjoern Peemoeller
--- @version November 2016
--- @category general
------------------------------------------------------------------------------

module Numeric
  ( readInt, readNat, readHex, readOct, readBin
  ) where

import Data.Char ( digitToInt, isBinDigit, isOctDigit
                 , isDigit, isHexDigit, isSpace)
import Data.Maybe

--- Read a (possibly negative) integer as a first token in a string.
--- The string might contain leadings blanks and the integer is read
--- up to the first non-digit.
--- On success returns `[(v,s)]`, where `v` is the value of the integer
--- and `s` is the remaing string without the integer token.
readInt :: ReadS Int
readInt str = case dropWhile isSpace str of
  []       -> []
  '-':str1 -> map (\(n,s) -> (-n, s)) (readNat str1)
  str1     -> readNat str1

--- Read a natural number as a first token in a string.
--- The string might contain leadings blanks and the number is read
--- up to the first non-digit.
--- On success returns `[(v,s)]`, where `v` is the value of the number
--- and s is the remaing string without the number token.
readNat :: ReadS Int
readNat str = maybeToList $
  readNumPrefix (dropWhile isSpace str) Nothing 10 isDigit digitToInt

--- Read a hexadecimal number as a first token in a string.
--- The string might contain leadings blanks and the number is read
--- up to the first non-hexadecimal digit.
--- On success returns `[(v,s)]`, where `v` is the value of the number
--- and s is the remaing string without the number token.
readHex :: ReadS Int
readHex l = maybeToList $
  readNumPrefix (dropWhile isSpace l) Nothing 16 isHexDigit digitToInt

--- Read an octal number as a first token in a string.
--- The string might contain leadings blanks and the number is read
--- up to the first non-octal digit.
--- On success returns `[(v,s)]`, where `v` is the value of the number
--- and s is the remaing string without the number token.
readOct :: ReadS Int
readOct l = maybeToList $
  readNumPrefix (dropWhile isSpace l) Nothing 8 isOctDigit digitToInt

--- Read a binary number as a first token in a string.
--- The string might contain leadings blanks and the number is read
--- up to the first non-binary digit.
--- On success returns `[(v,s)]`, where `v` is the value of the number
--- and s is the remaing string without the number token.
readBin :: ReadS Int
readBin l = maybeToList $
  readNumPrefix (dropWhile isSpace l) Nothing 2 isBinDigit digitToInt

--- Read an integral number prefix where the value of an already read number
--- prefix is provided as the second argument.
--- The third argument is the base, the fourth argument
--- is a predicate to distinguish valid digits, and the fifth argument converts
--- valid digits into integer values.
readNumPrefix :: String -> Maybe Int -> Int -> (Char -> Bool) -> (Char -> Int)
              -> Maybe (Int, String)
readNumPrefix []     Nothing  _    _       _       = Nothing
readNumPrefix []     (Just n) _    _       _       = Just (n,"")
readNumPrefix (c:cs) (Just n) base isdigit valueof
   | isdigit c = readNumPrefix cs (Just (base*n+valueof c)) base isdigit valueof
   | otherwise = Just (n,c:cs)
readNumPrefix (c:cs) Nothing base isdigit valueof
   | isdigit c = readNumPrefix cs (Just (valueof c)) base isdigit valueof
   | otherwise = Nothing
