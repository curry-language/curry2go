-- Curry benchmark: naive reverse on a user-defined list

data Nat = O | S Nat

add :: Nat -> Nat -> Nat
add O     n = n
add (S m) n = S (add m n)

mult :: Nat -> Nat -> Nat
mult O     _ = O
mult (S m) n = add n (mult m n)

two :: Nat
two = S (S O)

four :: Nat
four = mult two two

nat16 :: Nat
nat16 = mult four four

nat256 :: Nat
nat256 = mult nat16 nat16

nat4096 :: Nat
nat4096 = mult nat256 nat16

nat16384 :: Nat
nat16384 = mult nat4096 four

data MyList a = Cons a (MyList a) | Nil

data MyBool = MyTrue | MyFalse

append :: MyList a -> MyList a -> MyList a
append Nil         ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

rev :: MyList a -> MyList a
rev Nil         = Nil
rev (Cons x xs) = append (rev xs) (Cons x Nil)

natList :: Nat -> MyList Nat
natList O     = Nil
natList (S x) = Cons (S x) (natList x)

isList :: MyList _ -> MyBool
isList Nil         = MyTrue
isList (Cons _ xs) = isList xs

goal0 :: MyList MyBool
goal0 = rev (Cons MyTrue (Cons MyFalse (Cons MyFalse Nil)))

goal1 :: MyList Nat
goal1 = rev (natList nat16)

goal2 :: MyList Nat
goal2 = rev (natList nat256)

goal3 :: MyBool
goal3 = isList (rev (natList nat4096))  -- 8.394.753 rev. steps

goal4 :: MyBool
goal4 = isList (rev (natList nat16384)) -- 134.242.305 rev. steps

main :: MyBool
main = goal3
