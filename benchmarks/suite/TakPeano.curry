data MyBool = MyTrue | MyFalse

ifThenElse :: MyBool -> a -> a -> a
ifThenElse MyTrue  x _ = x
ifThenElse MyFalse _ y = y

data Nat = O | S Nat

add :: Nat -> Nat -> Nat
add O     y = y
add (S x) y = S (add x y)

double :: Nat -> Nat
double x = add x x

dec :: Nat -> Nat
dec (S x) = x

leq :: Nat -> Nat -> TakPeano.MyBool
leq O     _     = MyTrue
leq (S _) O     = MyFalse
leq (S x) (S y) = leq x y

tak :: Nat -> Nat -> Nat -> Nat
tak x y z = ifThenElse (leq x y)
                       z
                       (tak (tak (dec x) y z)
                            (tak (dec y) z x)
                            (tak (dec z) x y))

two :: Nat
two = S (S O)

four :: Nat
four = double two

n8 :: Nat
n8 = double four

n16 :: Nat
n16 = double n8

n24 :: Nat
n24 = add n8 n16

n27 :: Nat
n27 = add (S two) n24


takPeano_24_16_8 :: Nat
takPeano_24_16_8 = tak n24 n16 n8

takPeano_27_16_8 :: Nat
takPeano_27_16_8 = tak n27 n16 n8
--goal2 = tak 33 17 8

takPeano :: Nat
takPeano = takPeano_24_16_8

main :: Nat
main = takPeano_24_16_8 --takPeano_27_16_8
