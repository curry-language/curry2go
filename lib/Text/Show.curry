--------------------------------------------------------------------------------
--- This library provides a type and combinators for show functions using
--- functional lists.
---
--- @author  Bjoern Peemoeller
--- @version April 2016
--------------------------------------------------------------------------------
module Text.Show ( ShowS
                 , showString, showChar, showParen, shows
                 ) where

--- The type synonym `ShowS` represents strings as difference lists.
--- Composing functions of this type allows concatenation of lists
--- in constant time.
type ShowS = String -> String

--- Prepend a string
showString :: String -> ShowS
showString s = (s ++)

--- Prepend a single character
showChar :: Char -> ShowS
showChar c = (c:)

--- Surround the inner show function with parentheses if the first argument
--- evaluates to `True`.
showParen  :: Bool -> ShowS -> ShowS
showParen True  s = showChar '(' . s . showChar ')'
showParen False s = s

--- Convert a value to `ShowS` using the standard show function.
shows :: Show a => a -> ShowS
shows = showString . show
