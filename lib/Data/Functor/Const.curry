-----------------------------------------------------------------------------
--- This simple module defines the const functor known from Haskell's
--- base libraries. It defines a wrapper around a constant value that
--- "ignores" functions mapped over it.
-----------------------------------------------------------------------------

module Data.Functor.Const
  ( Const (..)
  ) where

--- The `Const` functor which returns a constant for any `fmap`, i.e.,
--- a wrapper around a constant value that ignores functions mapped over it.
---
--- Example:
---
---     > fmap (++ "world") (Const "Hello")
---     Const "Hello"
newtype Const a _ = Const { getConst :: a }
  deriving (Eq, Ord, Read, Show)

instance Functor (Const a) where
  fmap _ (Const x) = Const x
