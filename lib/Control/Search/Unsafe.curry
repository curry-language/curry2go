------------------------------------------------------------------------------
--- Library with operations to encapsulate search, i.e., non-deterministic
--- computations. Note that these operations are not fully declarative,
--- i.e., the results depend on the order of evaluation and program rules.
--- This is due to the fact that the search operators work on a copy
--- of the current expression to be encapsulated.
--- The potential problems of this method are discussed in this paper:
---
--- > B. Brassel, M. Hanus, F. Huch:
--- > Encapsulating Non-Determinism in Functional Logic Computations
--- > Journal of Functional and Logic Programming, No. 6, EAPLS, 2004
---
--- There are newer and better approaches the encapsulate search,
--- in particular, set functions (see module `Control.Search.SetFunctions`
--- which should be used.
---
--- @author Michael Hanus
--- @version October 2023
------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}

module Control.Search.Unsafe
  ( allValues, someValue, oneValue, isFail
#ifdef __PAKCS__
  , rewriteAll, rewriteSome
#endif
  ) where

#ifdef __KICS2__
import qualified Control.Search.SearchTree as ST
#endif

------------------------------------------------------------------------------

--- Returns all values of an expression.
--- Conceptually, the value is computed on a copy of the expression,
--- i.e., the evaluation of the expression does not share any results.
--- In PAKCS, the evaluation suspends as long as the expression
--- contains unbound variables or the computed
--- value contains unbound variables.
---
--- Note that this operation is not purely declarative since the ordering
--- of the computed values depends on the ordering of the program rules.
allValues :: a -> [a]
#ifdef __KICS2__
allValues e = ST.allValuesDFS (ST.someSearchTree e)
#else
allValues external
#endif

--- Returns just one value for an expression.
--- If the expression has no value, `Nothing` is returned.
--- Conceptually, the value is computed on a copy of the expression,
--- i.e., the evaluation of the expression does not share any results.
--- In PAKCS, the evaluation suspends as long as the expression
--- contains unbound variables or the computed
--- value contains unbound variables.
---
--- Note that this operation is not purely declarative since
--- the computed value depends on the ordering of the program rules.
--- Thus, this operation should be used only if the expression
--- has a single value.
oneValue :: a -> Maybe a
#ifdef __KICS2__
oneValue x =
  let vals = ST.allValuesWith ST.dfsStrategy (ST.someSearchTree x)
  in (if null vals then Nothing else Just (head vals))
#else
oneValue external
#endif

--- Returns some value for an expression.
--- If the expression has no value, the computation fails.
--- Conceptually, the value is computed on a copy of the expression,
--- i.e., the evaluation of the expression does not share any results.
--- In PAKCS, the evaluation suspends as long as the expression
--- contains unbound variables or the computed
--- value contains unbound variables.
---
--- Note that this operation is not purely declarative since
--- the computed value depends on the ordering of the program rules.
--- Thus, this operation should be used only if the expression
--- has a single value.
someValue :: a -> a
someValue x = case oneValue x of Just v  -> v
                                 Nothing -> failed

--- Does the computation of the argument to a value fail?
--- Conceptually, the argument is evaluated on a copy, i.e.,
--- even if the computation does not fail, it has not been evaluated.
isFail :: a -> Bool
isFail x = case oneValue x of Nothing -> True
                              Just _  -> False

#ifdef __PAKCS__
------------------------------------------------------------------------------
--- Gets all values computable by term rewriting.
--- In contrast to `allValues`, this operation does not wait
--- until all "outside" variables are bound to values,
--- but it returns all values computable by term rewriting
--- and ignores all computations that requires bindings for outside variables.
rewriteAll :: a -> [a]
rewriteAll external

--- Similarly to 'rewriteAll' but returns only some value computable
--- by term rewriting. Returns `Nothing` if there is no such value.
rewriteSome :: a -> Maybe a
rewriteSome external
#endif

------------------------------------------------------------------------------
