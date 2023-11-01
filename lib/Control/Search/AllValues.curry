------------------------------------------------------------------------------
--- Library with operations to encapsulate search, i.e., non-deterministic
--- computations, as I/O operations in order to make the results dependend
--- on the external world, e.g., the schedule for non-determinism.
---
--- To encapsulate search in non-I/O computations, one can use
--- set functions (see module `Control.Search.SetFunctions`.
---
--- @author Michael Hanus
--- @version October 2023
------------------------------------------------------------------------------

module Control.Search.AllValues
  ( getAllValues, getOneValue, getAllFailures )
 where

import Control.Search.Unsafe

------------------------------------------------------------------------------

--- Gets all values of an expression (similarly to Prolog's `findall`).
--- Conceptually, the value is computed on a copy of the expression,
--- i.e., the evaluation of the expression does not share any results.
--- In PAKCS, the evaluation suspends as long as the expression
--- contains unbound variables or the computed
--- value contains unbound variables.
getAllValues :: a -> IO [a]
getAllValues e = return (allValues e)

--- Gets one value of an expression. Returns `Nothing` if the search space
--- is finitely failed.
--- Conceptually, the value is computed on a copy of the expression,
--- i.e., the evaluation of the expression does not share any results.
--- In PAKCS, the evaluation suspends as long as the expression
--- contains unbound variables or the computed
--- value contains unbound variables.
getOneValue :: a -> IO (Maybe a)
getOneValue x = return (oneValue x)

--- Returns a list of values that do not satisfy a given constraint.
--- @param x - an expression (a generator evaluable to various values)
--- @param c - a constraint that should not be satisfied
--- @return A list of all values of e such that (c e) is not provable
getAllFailures :: a -> (a -> Bool) -> IO [a]
getAllFailures generator test = do
  xs <- getAllValues generator
  failures <- mapM (naf test) xs
  return $ concat failures

-- (naf c x) returns [x] if (c x) fails, and [] otherwise.
naf :: (a -> Bool) -> a -> IO [a]
naf c x = getOneValue (c x) >>= return . maybe [x] (const [])

------------------------------------------------------------------------------
