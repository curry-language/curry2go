------------------------------------------------------------------------------
--- Library containing unsafe operations.
--- These operations should be carefully used (e.g., for testing or debugging).
--- These operations should not be used in application programs!
---
--- @author Michael Hanus, Bjoern Peemoeller
--- @version September 2013
--- @category general
------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}

module System.IO.Unsafe
  ( unsafePerformIO, trace
#ifdef __PAKCS__
  , spawnConstraint, isVar, identicalVar, isGround, compareAnyTerm
  , showAnyTerm, showAnyQTerm, showAnyExpression, showAnyQExpression
  , readsAnyUnqualifiedTerm, readAnyUnqualifiedTerm
  , readsAnyQTerm, readAnyQTerm
  , readsAnyQExpression, readAnyQExpression
#endif
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

#ifdef __PAKCS__

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
spawnConstraint external

--- Tests whether the first argument evaluates to a currently unbound
--- variable (use with care!).
isVar :: _ -> Bool
isVar v = prim_isVar $! v

prim_isVar :: _ -> Bool
prim_isVar external

--- Tests whether both arguments evaluate to the identical currently unbound
--- variable (use with care!).
--- For instance, `identicalVar (id x) (fst (x,1))` evaluates to
--- `True` whereas `identicalVar x y` and
--- `let x=1 in identicalVar x x` evaluate to `False`
identicalVar :: a -> a -> Bool
identicalVar x y = (prim_identicalVar $! y) $! x

--- `let x=1 in identicalVar x x` evaluate to `False`
prim_identicalVar :: a -> a -> Bool
prim_identicalVar external

--- Tests whether the argument evaluates to a ground value
--- (use with care!).
isGround :: _ -> Bool
isGround v = prim_isGround $!! v

prim_isGround :: _ -> Bool
prim_isGround external

--- Comparison of any data terms, possibly containing variables.
--- Data constructors are compared in the order of their definition
--- in the datatype declarations and recursively in the arguments.
--- Variables are compared in some internal order.
compareAnyTerm :: a -> a -> Ordering
compareAnyTerm external


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
prim_showAnyTerm external

--- Transforms the normal form of a term into a string representation
--- in standard prefix notation.
--- Thus, showAnyQTerm evaluates its argument to normal form.
--- This function is similar to the function `ReadShowTerm.showQTerm`
--- but it also transforms logic variables into a string representation
--- that can be read back by `Unsafe.read(s)AnyQTerm`.
--- Thus, the result depends on the evaluation and binding status of
--- logic variables so that it should be used with care!
showAnyQTerm :: _ -> String
showAnyQTerm x = prim_showAnyQTerm $!! x

prim_showAnyQTerm :: _ -> String
prim_showAnyQTerm external


--- Transform a string containing a term in standard prefix notation
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
prim_readsAnyUnqualifiedTerm external


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

--- Transforms a string containing a term in standard prefix notation
--- with qualified constructor names into the corresponding data term.
--- The string might contain logical variable encodings produced by
--- `showAnyQTerm`.
--- In case of a successful parse, the result is a one element list
--- containing a pair of the data term and the remaining unparsed string.

readsAnyQTerm :: String -> [(_,String)]
readsAnyQTerm s = prim_readsAnyQTerm $## s

prim_readsAnyQTerm :: String -> [(_,String)]
prim_readsAnyQTerm external

--- Transforms a string containing a term in standard prefix notation
--- with qualified constructor names into the corresponding data term.
--- The string might contain logical variable encodings produced by
--- `showAnyQTerm`.

readAnyQTerm :: String -> _
readAnyQTerm s = case result of
  [(term,tail)] -> if all isSpace tail
                     then term
                     else error "Unsafe.readAnyQTerm: no parse"
  []            ->  error "Unsafe.readAnyQTerm: no parse"
  _             ->  error "Unsafe.readAnyQTerm: ambiguous parse"
 where result = readsAnyQTerm s


--- Transforms any expression (even not in normal form)
--- into a string representation
--- in standard prefix notation without module qualifiers.
--- The result depends on the evaluation and binding status of
--- logic variables so that it should be used with care!
showAnyExpression :: _ -> String
showAnyExpression external

--- Transforms any expression (even not in normal form)
--- into a string representation
--- in standard prefix notation with module qualifiers.
--- The result depends on the evaluation and binding status of
--- logic variables so that it should be used with care!
showAnyQExpression :: _ -> String
showAnyQExpression external


--- Transforms a string containing an expression in standard prefix notation
--- with qualified constructor names into the corresponding expression.
--- The string might contain logical variable and defined function
--- encodings produced by showAnyQExpression.
--- In case of a successful parse, the result is a one element list
--- containing a pair of the expression and the remaining unparsed string.

readsAnyQExpression :: String -> [(_,String)]
readsAnyQExpression s = prim_readsAnyQExpression $## s

prim_readsAnyQExpression :: String -> [(_,String)]
prim_readsAnyQExpression external

--- Transforms a string containing an expression in standard prefix notation
--- with qualified constructor names into the corresponding expression.
--- The string might contain logical variable and defined function
--- encodings produced by showAnyQExpression.

readAnyQExpression :: String -> _
readAnyQExpression s = case result of
  [(term,tail)] -> if all isSpace tail
                     then term
                     else error "Unsafe.readAnyQExpression: no parse"
  []            ->  error "Unsafe.readAnyQExpression: no parse"
  _             ->  error "Unsafe.readAnyQExpression: ambiguous parse"
 where result = readsAnyQExpression s
#endif
