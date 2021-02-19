--- ----------------------------------------------------------------------------
--- Library with some useful functions on the `Maybe` datatype.
---
--- @author Frank Huch, Bernd Brassel, Bjoern Peemoeller
--- @version October 2014
--- @category general
--- ----------------------------------------------------------------------------

module Data.Maybe
  ( Maybe (..)
  , maybe
  , isJust, isNothing
  , fromJust, fromMaybe
  , listToMaybe, maybeToList
  , catMaybes, mapMaybe
  ) where

--- Return `True` iff the argument is of the form `Just _`.
isJust :: Maybe _ -> Bool
isJust (Just _) = True
isJust Nothing  = False

--- Return `True` iff the argument is of the form `Nothing`.
isNothing :: Maybe _ -> Bool
isNothing Nothing  = True
isNothing (Just _) = False

--- Extract the argument from the `Just` constructor and throw an error
--- if the argument is `Nothing`.
fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing  = error "Maybe.fromJust: Nothing"

--- Extract the argument from the `Just` constructor or return the provided
--- default value if the argument is `Nothing`.
fromMaybe :: a -> Maybe a -> a
fromMaybe d Nothing  = d
fromMaybe _ (Just a) = a

--- Return `Nothing` on an empty list or `Just x` where `x` is the first
--- list element.
listToMaybe :: [a] -> Maybe a
listToMaybe []      = Nothing
listToMaybe (a : _) = Just a

--- Return an empty list for `Nothing` or a singleton list for `Just x`.
maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]

--- Return the list of all `Just` values.
catMaybes :: [Maybe a] -> [a]
catMaybes ms = [ m | (Just m) <- ms ]

--- Apply a function which may throw out elements using the `Nothing`
--- constructor to a list of elements.
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = catMaybes . map f
