--- ----------------------------------------------------------------------------
--- This module provides some utility functions for function application.
---
--- @author Bjoern Peemoeller
--- @version July 2013
--- ----------------------------------------------------------------------------
module Data.Function (fix, on) where

--- `fix f` is the least fixed point of the function `f`,
--- i.e. the least defined `x` such that `f x = x`.
fix :: (a -> a) -> a
fix f = let x = f x in x

--- `on f g x y` applies the binary operation `f` to the results of
--- applying operation `g` to two arguments `x` and `y`.
--- Thus, it transforms two inputs and combines the outputs.
---
---     (*) `on` f = \x y -> f x * f y
---
--- A typical usage of this operation is:
---
---     sortBy (compare `on` fst)
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on op f x y = f x `op` f y
