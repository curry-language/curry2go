-----------------------------------------------------------------------------
--- This simple module defines the identify functor and monad and
--- has been adapted from the same Haskell module (by Andy Gill).
--- It defines a a trivial type constructor `Identity` which
--- can be used with functions parameterized by functor or monad classes
--- or as a simple base to specialize monad transformers.
-----------------------------------------------------------------------------

module Data.Functor.Identity where

--- The `Identity` type constructor with `Functor`, `Applicative`,
--- and `Monad` instances.
newtype Identity a = Identity { runIdentity :: a }
  deriving (Eq, Ord, Read, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  m >>= k = k (runIdentity m)
  return a = Identity a
