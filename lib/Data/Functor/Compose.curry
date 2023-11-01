-----------------------------------------------------------------------------
--- This simple module defines the compose functor known from Haskell's
--- base libraries. The compose functor is the composition of two functors
--- which always is a functor too.
-----------------------------------------------------------------------------

module Data.Functor.Compose
  ( Compose (..)
  ) where

newtype Compose f g a = Compose { getCompose :: f (g a) }

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose x) = Compose (fmap (fmap f) x)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure x = Compose (pure (pure x))
  Compose f <*> Compose x = Compose (liftA2 (<*>) f x)
  liftA2 f (Compose x) (Compose y) = Compose (liftA2 (liftA2 f) x y)
