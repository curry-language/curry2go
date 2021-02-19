module Control.Applicative
  ( Applicative(..), liftA, liftA3, when
  , sequenceA, sequenceA_
  ) where

--- Lift a function to actions.
--- This function may be used as a value for `fmap` in a `Functor` instance.
liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f a = pure f <*> a

--- Lift a ternary function to actions.
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = liftA2 f a b <*> c

-- | Conditional execution of 'Applicative' expressions.
when :: (Applicative f) => Bool -> f () -> f ()
when p s  = if p then s else pure ()

--- Evaluate each action in the list from left to right, and
--- collect the results. For a version that ignores the results
--- see 'sequenceA_'.
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA []     = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

--- Evaluate each action in the structure from left to right, and
--- ignore the results. For a version that doesn't ignore the results
--- see 'sequenceA'.
sequenceA_ :: (Applicative f) => [f a] -> f ()
sequenceA_ = foldr (*>) (pure ())
