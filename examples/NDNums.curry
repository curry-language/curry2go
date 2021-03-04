-- Examples for showing incompleteness of DFS

data Nat = O | S Nat

add :: Nat -> Nat -> Nat
add O     n = n
add (S m) n = S (add m n)

mult :: Nat -> Nat -> Nat
mult O     _ = O
mult (S m) n = add n (mult m n)

eqNat :: Nat -> Nat -> Bool
eqNat O     O     = True
eqNat (S m) (S n) = eqNat m n

two = S (S O)
four = add two two
nat16 = mult four four
nat256 = mult nat16 nat16
nat4096 = mult nat256 nat16
nat16384 = mult nat4096 four

-- greater or equal arbitrary natural number:
ndnum n = ndnum (S n) ? n

main = eqNat (ndnum O) nat256
-- DFS: no result
-- BFS: one result, try: curry2goc -r --bfs --first NDNums
