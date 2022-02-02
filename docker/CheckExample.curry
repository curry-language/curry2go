-- Basic examples for the use of CurryCheck with user-defined data
-- This is used in the Dockerfile to initialize CurryCheck.

import Test.Prop

-- Natural numbers defined by s-terms (Z=zero, S=successor):
data Nat = Z | S Nat
 deriving (Eq,Show)

-- addition on natural numbers:
add         :: Nat -> Nat -> Nat
add Z     n = n
add (S m) n = S(add m n)

-- Property: the addition operator is commutative
add_commutative x y = add x y -=- add y x
