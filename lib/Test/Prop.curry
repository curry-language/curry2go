-------------------------------------------------------------------------
--- This module defines the interface of properties that can be checked
--- with the CurryCheck tool, an automatic property-based test tool
--- based on the EasyCheck library.
--- The ideas behind EasyCheck are described in
--- [this paper](http://www-ps.informatik.uni-kiel.de/~sebf/pub/flops08.html).
--- CurryCheck automatically tests properties defined with this library.
--- CurryCheck supports the definition of unit tests
--- (also for I/O operations) and property tests parameterized
--- over some arguments. CurryCheck is described in more detail in
--- [this paper](http://www.informatik.uni-kiel.de/~mh/papers/LOPSTR16.html).
---
--- Basically, this module is a stub clone of the EasyCheck library
--- which contains only the interface of the operations used to specify
--- properties. Hence, this library does not import any other library.
--- This supports the definition of properties in any other module
--- (execept for the prelude).
---
--- @author Sebastian Fischer (with extensions by Michael Hanus)
--- @version January 2019
-------------------------------------------------------------------------

module Test.Prop (

  -- test specification:
  PropIO, returns, sameReturns, toError, toIOError,

  Prop, (==>), for, forAll,

  is, isAlways, isEventually, uniquely, always, eventually,
  failing, successful, deterministic, (-=-), (<~>), (~>), (<~), (<~~>),
  (#), (#<), (#>), (<=>),
  solutionOf,

  -- test annotations
  label, trivial, classify, collect, collectAs,

  -- enumerating values
  valuesOf

  ) where

import Test.Prop.Types

infix  1 `is`, `isAlways`, `isEventually`
infix  1 -=-, <~>, ~>, <~, <~~>, `trivial`, #, #<, #>, <=>
infix  1 `returns`, `sameReturns`
infixr 0 ==>

-------------------------------------------------------------------------
-- Properties involving I/O actions:

--- The property `returns a x` is satisfied if the execution of the
--- I/O action `a` returns the value `x`.
returns :: (Eq a, Show a) => IO a -> a -> PropIO
returns _ _ = propUndefinedError "returns"

--- The property `sameReturns a1 a2` is satisfied if the execution of the
--- I/O actions `a1` and `a2` return identical values.
sameReturns :: (Eq a, Show a) => IO a -> IO a -> PropIO
sameReturns _ _ = propUndefinedError "sameReturns"

--- The property `toError a` is satisfied if the evaluation of the argument
--- to normal form yields an exception.
toError :: a -> PropIO
toError _ = propUndefinedError "toError"

--- The property `toIOError a` is satisfied if the execution of the
--- I/O action `a` causes an exception.
toIOError :: IO a -> PropIO
toIOError _ = propUndefinedError "toIOError"


-------------------------------------------------------------------------
-- Standard properties to be checked:

--- The property `x -=- y` is satisfied if `x` and `y` have deterministic
--- values that are equal.
(-=-) ::(Eq a, Show a) =>  a -> a -> Prop
_ -=- _ = propUndefinedError "-=-"

--- The property `x <~> y` is satisfied if the sets of the values of
--- `x` and `y` are equal.
(<~>) :: (Eq a, Show a) => a -> a -> Prop
_ <~> _ = propUndefinedError "<~>"

--- The property `x ~> y` is satisfied if `x` evaluates to every value of `y`.
--- Thus, the set of values of `y` must be a subset of the set of values of `x`.
(~>) :: (Eq a, Show a) => a -> a -> Prop
_ ~> _ = propUndefinedError "~>"

--- The property `x <~ y` is satisfied if `y` evaluates to every value of `x`.
--- Thus, the set of values of `x` must be a subset of the set of values of `y`.
(<~) :: (Eq a, Show a) => a -> a -> Prop
_ <~ _ = propUndefinedError "<~"

--- The property `x <~~> y` is satisfied if the multisets of the values of
--- `x` and `y` are equal.
(<~~>) :: (Eq a, Show a) => a -> a -> Prop
_ <~~> _ = propUndefinedError "<~~>"

--- A conditional property is tested if the condition evaluates to `True`.
(==>) :: Bool -> Prop -> Prop
_ ==> _ = propUndefinedError "==>"

--- `solutionOf p` returns (non-deterministically) a solution
--- of predicate `p`. This operation is useful to test solutions
--- of predicates.
solutionOf :: Data a => (a -> Bool) -> a
solutionOf pred = pred x &> x where x free

--- The property `is x p` is satisfied if `x` has a deterministic value
--- which satisfies `p`.
is :: Show a => a -> (a -> Bool) -> Prop
is _ _ = propUndefinedError "is"

--- The property `isAlways x p` is satisfied if all values of `x` satisfy `p`.
isAlways :: Show a => a -> (a -> Bool) -> Prop
isAlways _ = propUndefinedError "isAlways"

--- The property `isEventually x p` is satisfied if some value of `x`
--- satisfies `p`.
isEventually :: Show a => a -> (a -> Bool) -> Prop
isEventually _ = propUndefinedError "isEventually"

--- The property `uniquely x` is satisfied if `x` has a deterministic value
--- which is true.
uniquely :: Bool -> Prop
uniquely _ = propUndefinedError "uniquely"

--- The property `always x` is satisfied if all values of `x` are true.
always :: Bool -> Prop
always _ = propUndefinedError "always"

--- The property `eventually x` is satisfied if some value of `x` is true.
eventually :: Bool -> Prop
eventually _ = propUndefinedError "eventually"

--- The property `failing x` is satisfied if `x` has no value.
failing :: Show a => a -> Prop
failing _ = propUndefinedError "failing"

--- The property `successful x` is satisfied if `x` has at least one value.
successful :: Show a => a -> Prop
successful _ = propUndefinedError "successful"

--- The property `deterministic x` is satisfied if `x` has exactly one value.
deterministic :: Show a => a -> Prop
deterministic _ = propUndefinedError "deterministic"

--- The property `x # n` is satisfied if `x` has `n` values.
(#) :: (Eq a, Show a) => a -> Int -> Prop
_ # _ =  propUndefinedError "#"

--- The property `x #< n` is satisfied if `x` has less than `n` values.
(#<) :: (Eq a, Show a) => a -> Int -> Prop
_ #< _ = propUndefinedError "#<"

--- The property `x #> n` is satisfied if `x` has more than `n` values.
(#>) :: (Eq a, Show a) => a -> Int -> Prop
_ #> _ = propUndefinedError "#>"

--- The property `for x p` is satisfied if all values `y` of `x`
--- satisfy property `p y`.
for :: Show a => a -> (a -> Prop) -> Prop
for _ _ = propUndefinedError "for"

--- The property `forAll xs p` is satisfied if all values `x` of the list `xs`
--- satisfy property `p x`.
forAll :: Show a => [a] -> (a -> Prop) -> Prop
forAll _ _ = propUndefinedError "forAll"

--- The property `f <=> g` is satisfied if `f` and `g` are equivalent
--- operations, i.e., they can be replaced in any context without changing
--- the computed results.
(<=>) :: a -> a -> Prop
_ <=> _ = propUndefinedError "#"

-------------------------------------------------------------------------
-- Test Annotations

--- Assign a label to a property.
--- All labeled tests are counted and shown at the end.
label :: String -> Prop -> Prop
label _ _ = propUndefinedError "label"

--- Assign a label to a property if the first argument is `True`.
--- All labeled tests are counted and shown at the end.
--- Hence, this combinator can be used to classify tests:
---
---     multIsComm x y = classify (x<0 || y<0) "Negative" $ x*y -=- y*x
---
classify :: Bool -> String -> Prop -> Prop
classify _ _ _ = propUndefinedError "classify"

--- Assign the label "trivial" to a property if the first argument is `True`.
--- All labeled tests are counted and shown at the end.
trivial :: Bool -> Prop -> Prop
trivial _ _ = propUndefinedError "trivial"

--- Assign a label showing the given argument to a property.
--- All labeled tests are counted and shown at the end.
collect :: Show a => a -> Prop -> Prop
collect _ _ = propUndefinedError "collect"

--- Assign a label showing a given name and the given argument to a property.
--- All labeled tests are counted and shown at the end.
collectAs :: Show a => String -> a -> Prop -> Prop
collectAs _ _ _ = propUndefinedError "collectAs"

-------------------------------------------------------------------------
-- Value generation

--- Computes the list of all values of the given argument
--- according to a given strategy (here:
--- randomized diagonalization of levels with flattening).
valuesOf :: a -> [a]
valuesOf =
  error "Test.Prop.valuesOf undefined. Use Test.EasyCheck to actually run it!"

propUndefinedError :: String -> _
propUndefinedError op = error $
  "Test.Prop." ++ op ++ " undefined. Use Test.EasyCheck to actually run it!"

-------------------------------------------------------------------------
