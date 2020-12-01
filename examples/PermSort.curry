
data Nat = O | S Nat

leq :: Nat -> Nat -> Bool
leq O     _     = True
leq (S _) O     = False
leq (S m) (S n) = leq m n

-- computing permutations
ndinsert x xs     = x : xs
ndinsert x (y:ys) = y : ndinsert x ys

perm []     = []
perm (x:xs) = ndinsert x (perm xs)

sorted [] = True
sorted [_] = True
sorted (x:y:zs) = cond (leq x y) (sorted (y:zs))

cond True x = x

idSorted xs = cond (sorted xs) xs

psort xs = idSorted (perm xs)

main :: [Nat]
main = psort [S (S (S (S O))), S (S (S O)), S (S O), S O, O]
