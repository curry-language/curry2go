------------------------------------------------------------------------------
--- Library containing *unsafe* operations.
--- These operations should be carefully used (e.g., for testing or debugging).
--- These operations should not be used in application programs!
---
--- @author Michael Hanus, Bjoern Peemoeller
--- @version April 2021
------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}

module System.IO.Unsafe
  ( unsafePerformIO, trace
  , spawnConstraint, isVar, identicalVar, isGround, compareAnyTerm
  , showAnyTerm, showAnyExpression
  , readsAnyUnqualifiedTerm, readAnyUnqualifiedTerm
  ) where

import Data.Char (isSpace)
import System.IO (hPutStrLn, stderr)

--- Performs and hides an I/O action in a computation (use with care!).
unsafePerformIO :: IO a -> a
unsafePerformIO external

--- Prints the first argument as a side effect and behaves as identity on the
--- second argument.
trace :: String -> a -> a
trace s x = unsafePerformIO (hPutStrLn stderr s >> return x)

--- Spawns a constraint and returns the second argument.
--- This function can be considered as defined by
--- `spawnConstraint c x | c = x`.
--- However, the evaluation of the constraint and the right-hand side
--- are performed concurrently, i.e., a suspension of the constraint
--- does not imply a blocking of the right-hand side and the
--- right-hand side might be evaluated before the constraint is successfully
--- solved.
--- Thus, a computation might return a result even if some of the
--- spawned constraints are suspended (use the PAKCS option
--- `+suspend` to show such suspended goals).
spawnConstraint :: Bool -> a -> a
#ifdef __KICS2__
spawnConstraint c x | c = x -- non-concurrent implementation
#else
spawnConstraint external
#endif

--- Tests whether the first argument evaluates to a currently unbound
--- variable (use with care!).
isVar :: Data a => a -> Bool
isVar v = prim_isVar $! v

prim_isVar :: a -> Bool
#ifdef __KICS2__
prim_isVar = error "System.IO.Unsafe.isVar: not yet implemented"
#else
prim_isVar external
#endif

--- Tests whether both arguments evaluate to the identical currently unbound
--- variable (use with care!).
--- For instance,
---
---     identicalVar (id x) (fst (x,1))  where x free
---
--- evaluates to `True`, whereas
---
---     identicalVar x y  where x,y free
---
--- and
---
---     let x=1 in identicalVar x x
---
--- evaluate to `False`
identicalVar :: Data a => a -> a -> Bool
identicalVar x y = (prim_identicalVar $! y) $! x

--- `let x=1 in identicalVar x x` evaluate to `False`
prim_identicalVar :: a -> a -> Bool
#ifdef __KICS2__
prim_identicalVar = error "System.IO.Unsafe.identicalVar: not yet implemented"
#else
prim_identicalVar external
#endif

--- Tests whether the argument evaluates to a ground value
--- (use with care!).
isGround :: Data a => a -> Bool
isGround v = prim_isGround $!! v

prim_isGround :: _ -> Bool
#ifdef __KICS2__
prim_isGround = error "System.IO.Unsafe.isGround: not yet implemented"
#else
prim_isGround external
#endif

--- Comparison of any data terms, possibly containing variables.
--- Data constructors are compared in the order of their definition
--- in the datatype declarations and recursively in the arguments.
--- Variables are compared in some internal order.
compareAnyTerm :: a -> a -> Ordering
#ifdef __KICS2__
compareAnyTerm = error "System.IO.Unsafe.compareAnyTerm: not yet implemented"
#else
compareAnyTerm external
#endif

------------------------------------------------------------------------------
-- Read and show operations on arbitrary terms and expressions

--- Transforms the normal form of a term into a string representation
--- in standard prefix notation.
--- Thus, showAnyTerm evaluates its argument to normal form.
--- This function is similar to the function `ReadShowTerm.showTerm`
--- but it also transforms logic variables into a string representation
--- that can be read back by `Unsafe.read(s)AnyUnqualifiedTerm`.
--- Thus, the result depends on the evaluation and binding status of
--- logic variables so that it should be used with care!
showAnyTerm :: _ -> String
showAnyTerm x = prim_showAnyTerm $!! x

prim_showAnyTerm :: _ -> String
#ifdef __KICS2__
prim_showAnyTerm = error "System.IO.Unsafe.showAnyTerm: not yet implemented"
#else
prim_showAnyTerm external
#endif


--- Transforms a string containing a term in standard prefix notation
--- without module qualifiers into the corresponding data term.
--- The string might contain logical variable encodings produced by showAnyTerm.
--- In case of a successful parse, the result is a one element list
--- containing a pair of the data term and the remaining unparsed string.

readsAnyUnqualifiedTerm :: [String] -> String -> [(_,String)]
readsAnyUnqualifiedTerm []                _ =
  error "ReadShowTerm.readsAnyUnqualifiedTerm: list of module prefixes is empty"
readsAnyUnqualifiedTerm (prefix:prefixes) s =
  readsAnyUnqualifiedTermWithPrefixes (prefix:prefixes) s

readsAnyUnqualifiedTermWithPrefixes :: [String] -> String -> [(_,String)]
readsAnyUnqualifiedTermWithPrefixes prefixes s =
  (prim_readsAnyUnqualifiedTerm $## prefixes) $## s

prim_readsAnyUnqualifiedTerm :: [String] -> String -> [(_,String)]
#ifdef __KICS2__
prim_readsAnyUnqualifiedTerm =
  error "System.IO.Unsafe.readsAnyUnqualifiedTerm: not yet implemented"
#else
prim_readsAnyUnqualifiedTerm external
#endif


--- Transforms a string containing a term in standard prefix notation
--- without module qualifiers into the corresponding data term.
--- The string might contain logical variable encodings produced by
--- `showAnyTerm`.

readAnyUnqualifiedTerm :: [String] -> String -> _
readAnyUnqualifiedTerm prefixes s = case result of
  [(term,tail)]
     -> if all isSpace tail
          then term
          else error ("Unsafe.readAnyUnqualifiedTerm: no parse, " ++
                      "unmatched string after term: " ++ tail)
  [] ->  error "Unsafe.readAnyUnqualifiedTerm: no parse"
  _  ->  error "Unsafe.readAnyUnqualifiedTerm: ambiguous parse"
 where result = readsAnyUnqualifiedTerm prefixes s

--- Transforms any expression (even not in normal form)
--- into a string representation
--- in standard prefix notation without module qualifiers.
--- The result depends on the evaluation and binding status of
--- logic variables so that it should be used with care!
showAnyExpression :: _ -> String
#ifdef __KICS2__
showAnyExpression =
  error "System.IO.Unsafe.showAnyExpression: not yet implemented"
#else
showAnyExpression external
#endif
