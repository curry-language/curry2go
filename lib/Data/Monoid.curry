-------------------------------------------------------------------------------
-- | Version: April 2025
--
-- Library with some useful `Monoid` instances.
-------------------------------------------------------------------------------

module Data.Monoid
  ( All (..), Any (..), Sum (..), Product (..), First (..), Last (..)
  ) where

-- | Boolean monoid under (&&)
newtype All = All { getAll :: Bool }
  deriving (Eq, Ord, Show, Read)

instance Monoid All where
  mempty = All True
  mappend (All x) (All y) = All (x && y)

-- | Boolean monoid under (||)
newtype Any = Any { getAny :: Bool }
  deriving (Eq, Ord, Show, Read)

instance Monoid Any where
  mempty = Any False
  mappend (Any x) (Any y) = Any (x || y)

-- | Monoid under addition.
newtype Sum a = Sum { getSum :: a }
  deriving (Eq, Ord, Show, Read)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  mappend (Sum x) (Sum y) = Sum (x + y)

instance Functor Sum where
  fmap f (Sum x) = Sum (f x)

instance Applicative Sum where
  pure = Sum
  Sum f <*> Sum x = Sum (f x)

instance Monad Sum where
  return = Sum
  Sum x >>= f = f x

-- | Monoid under multiplication.
newtype Product a = Product { getProduct :: a }
  deriving (Eq, Ord, Show, Read)

instance Num a => Monoid (Product a) where
  mempty = Product 1
  mappend (Product x) (Product y) = Product (x * y)

instance Functor Product where
  fmap f (Product x) = Product (f x)

instance Applicative Product where
  pure = Product
  Product f <*> Product x = Product (f x)

instance Monad Product where
  return = Product
  Product x >>= f = f x

-- | Maybe monoid returning the leftmost Just value.
newtype First a = First { getFirst :: Maybe a }
  deriving (Eq, Ord, Show, Read)

instance Functor First where
  fmap f (First x) = First (f <$> x)

instance Applicative First where
  pure = First . pure
  First f <*> First x = First (f <*> x)

instance Monad First where
  return = pure
  First x >>= f = First (x >>= getFirst . f)

instance Monoid (First a) where
  mempty = First Nothing
  mappend (First x) (First y) = First (x <|> y)

-- | Maybe monoid returning the rightmost Just value.
newtype Last a = Last { getLast :: Maybe a }
  deriving (Eq, Ord, Show, Read)

instance Monoid (Last a) where
  mempty = Last Nothing
  mappend (Last x) (Last y) = Last (y <|> x)

instance Functor Last where
  fmap f (Last x) = Last (f <$> x)

instance Applicative Last where
  pure = Last . pure
  Last f <*> Last x = Last (f <*> x)

instance Monad Last where
  return = pure
  Last x >>= f = Last (x >>= getLast . f)
