data Nat = O | S Nat

add :: Nat -> Nat -> Nat
add O     n = n
add (S m) n = S (add m n)

half :: Nat -> Nat
half O         = O
half (S O)     = O
half (S (S x)) = S (half x)

coin :: Nat
coin = O ? S O

main :: Nat
main = add (S (half (S coin))) (S O)
