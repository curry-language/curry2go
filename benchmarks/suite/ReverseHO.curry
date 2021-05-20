-- Curry benchmark:
-- linear reverse of a user-defined list with higher-order functions

data Nat = O | S Nat

add O n = n
add (S x) y = S (add x y)

double x = add x x

mult O _ = O
mult (S x) y = add y (mult x y)

two = S (S O)
four = double two
nat16 = mult four four
nat256 = mult nat16 nat16
nat4096 = mult nat256 nat16
nat16384 = mult nat4096 four
nat1M = mult nat16384 (mult nat16 four)
nat4M = mult nat16384 nat256

data List a = Nil | Cons a (List a)

data MyBool = MyTrue | MyFalse

--- Reverses the order of all elements in a list.
rev :: List a -> List a
rev = myfoldl (myflip Cons) Nil

-- Version without eta-reduce
rev2 :: List a -> List a
rev2 xs = myfoldl (myflip Cons) Nil xs

myfoldl :: (a -> b -> a) -> a -> List b -> a
myfoldl _ z Nil         = z
myfoldl f z (Cons x xs) = myfoldl f (f z x) xs

myflip :: (a -> b -> c) -> b -> a -> c
myflip f x y = f y x

natList O = Nil
natList (S x) = Cons (S x) (natList x)

isList Nil = MyTrue
isList (Cons _ xs) = isList xs

goal0 = rev (Cons MyTrue (Cons MyFalse (Cons MyFalse Nil)))

revHO_256 = rev (natList nat256)
revHO_16K = isList (rev (natList nat16384))
revHO_1M  = isList (rev (natList nat1M))

main = revHO_1M
