module Control.Monad
    ( Functor(..), Applicative(..), Monad(..)
    , filterM, (>=>), (<=<), forever, mapAndUnzipM, zipWithM
    , zipWithM_, foldM, foldM_, replicateM, replicateM_
    , when, unless, liftM3, join, void
    ) where

import Control.Applicative

--- This generalizes the list-based 'filter' function.
filterM :: (Applicative m) => (a -> m Bool) -> [a] -> m [a]
filterM p = foldr (\ x -> liftA2 (\ flg -> if flg
                                             then (x:)
                                             else id)
                                 (p x))
                  (pure [])

infixr 1 <=<, >=>

--- Left-to-right composition of Kleisli arrows.
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g = \x -> f x >>= g

--- Right-to-left composition of Kleisli arrows. @('>=>')@, with the arguments
--- flipped.
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(<=<) = flip (>=>)

--- Repeat an action indefinitely.
forever :: (Applicative f) => f a -> f b
forever a = let a' = a *> a' in a'

-- -----------------------------------------------------------------------------
-- Other monad functions

--- The 'mapAndUnzipM' function maps its first argument over a list, returning
--- the result as a pair of lists. This function is mainly used with complicated
--- data structures or a state-transforming monad.
mapAndUnzipM :: (Applicative m) => (a -> m (b,c)) -> [a] -> m ([b], [c])
mapAndUnzipM f xs =  unzip <$> sequenceA (map f xs)

--- The 'zipWithM' function generalizes 'zipWith' to
--- arbitrary applicative functors.
zipWithM :: (Applicative m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f xs ys  =  sequenceA (zipWith f xs ys)

--- 'zipWithM_' is the extension of 'zipWithM' which ignores the final result.
zipWithM_ :: (Applicative m) => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithM_ f xs ys =  sequenceA_ (zipWith f xs ys)

--- The 'foldM' function is analogous to 'foldl', except that its result is
--- encapsulated in a monad.
foldM :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m b
foldM f z0 xs = foldr f' return xs z0
  where f' x k z = f z x >>= k

--- Like 'foldM', but discards the result.
foldM_ :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m ()
foldM_ f a xs  = foldM f a xs >> return ()

--- @'replicateM' n act@ performs the action @n@ times,
--- gathering the results.
replicateM :: (Applicative m) => Int -> m a -> m [a]
replicateM cnt0 f =
    loop cnt0
  where
    loop cnt
        | cnt <= 0  = pure []
        | otherwise = liftA2 (:) f (loop (cnt - 1))

--- Like 'replicateM', but discards the result.
replicateM_ :: (Applicative m) => Int -> m a -> m ()
replicateM_ cnt0 f =
    loop cnt0
  where
    loop cnt
        | cnt <= 0  = pure ()
        | otherwise = f *> loop (cnt - 1)


--- The reverse of 'when'.
unless :: (Applicative f) => Bool -> f () -> f ()
unless p s =  if p then pure () else s

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f ma mb mc = do
  a <- ma 
  b <- mb 
  c <- mc
  return (f a b c)

--- Removes one level of monadic structure, i.e. 'flattens' the monad.
join :: Monad m => m (m a) -> m a
join = (>>= id)

--- Ignores the result of the evaluation.
void :: Functor f => f a -> f ()
void = fmap (const ())
