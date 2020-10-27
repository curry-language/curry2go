-- Curry benchmark: naive reverse on built-in lists

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

data MyBool = MyTrue | MyFalse

append :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x : (append xs ys)

rev :: [a] -> [a]
rev []     = []
rev (x:xs) = append (rev xs) [x]

natList :: Nat -> [Nat]
natList O     = []
natList (S x) = (S x) : (natList x)

isList :: [_] -> MyBool
isList []     = MyTrue
isList (_:xs) = isList xs

goal1 :: [Nat]
goal1 = rev (natList nat16)

goal2 :: [Nat]
goal2 = rev (natList nat256)

goal3 :: MyBool
goal3 = isList (rev (natList nat4096))

goal4 :: MyBool
goal4 = isList (rev (natList nat16384))

main :: MyBool
main = goal3
