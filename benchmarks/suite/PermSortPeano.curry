-- Permutation sort with Peano numbers and user-defined lists

{-# OPTIONS_CYMAKE -Wno-incomplete-patterns #-}

data MyBool = MyTrue | MyFalse

ifThenElse :: MyBool -> a -> a -> a
ifThenElse MyTrue  x _ = x
ifThenElse MyFalse _ y = y

guard :: MyBool -> a -> a
guard MyTrue x = x


data Nat = O | S Nat

dec :: Nat -> Nat
dec (S x) = x

leq :: Nat -> Nat -> MyBool
leq O     _     = MyTrue
leq (S _) O     = MyFalse
leq (S x) (S y) = leq x y

isNat :: Nat -> MyBool
isNat O = MyTrue
isNat (S x) = isNat x

add :: Nat -> Nat -> Nat
add O n = n
add (S x) y = S (add x y)

double :: Nat -> Nat
double x = add x x

mult :: Nat -> Nat -> Nat
mult O _ = O
mult (S x) y = add y (mult x y)

two :: Nat
two = S (S O)

three :: Nat
three = S two

four :: Nat
four = double two

nat13 :: Nat
nat13 = S (mult three four)

nat14 :: Nat
nat14 = add two (mult three four)

nat15 :: Nat
nat15 = S nat14

----------------------------------------------------------------
data List a = Nil | Cons a (List a)

app :: List a -> List a  -> List a
app Nil         ys = ys
app (Cons x xs) ys = Cons x (app xs ys)

len :: List a -> Nat
len Nil         = O
len (Cons _ xs) = S (len xs)

insert :: a -> List a  -> List a
insert x Nil         = Cons x Nil
insert x (Cons y ys) = (Cons x (Cons y ys)) ? (Cons y (insert x ys))

perm :: List a  -> List a
perm Nil         = Nil
perm (Cons x xs) = insert x (perm xs)

sorted :: List Nat  -> List Nat
sorted Nil       = Nil
sorted (Cons x Nil) = Cons x Nil
sorted (Cons x (Cons y ys)) = guard (leq x y) (Cons x (sorted (Cons y ys)))

psort :: List Nat  -> List Nat
psort xs = sorted (perm xs)

descList :: Nat -> Nat -> List Nat
descList up low =
  ifThenElse (leq low up) (Cons up (descList (dec up) low)) Nil

-- psort (2:[n,n-1 .. 3]++[1])
sortDescList :: Nat -> List Nat
sortDescList n = psort (Cons two (app (descList n three) (Cons (S O) Nil)))

psort_13 :: List Nat
psort_13 = sortDescList nat13

psort_14 :: List Nat
psort_14 = sortDescList nat14

psort_13_conc :: List Nat
psort_13_conc = let x = psort_13 in app (app x x) x

psort_13_length :: Nat
psort_13_length = let x = len psort_13 in add (add x x) x

main :: List Nat
main = psort_13
