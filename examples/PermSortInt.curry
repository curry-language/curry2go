-- The classical permutation sort example with built-in integers.

{-# OPTIONS_FRONTEND -Wno-incomplete-patterns -Wno-overlapping #-}

-- computing permutations
ndinsert :: a -> [a] -> [a]
ndinsert x xs     = x : xs
ndinsert x (y:ys) = y : ndinsert x ys

perm :: [a] -> [a]
perm []     = []
perm (x:xs) = ndinsert x (perm xs)

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [_] = True
sorted (x:y:zs) = cond (x <= y) (sorted (y:zs))

cond :: Bool -> a -> a
cond True x = x

idSorted :: [Int] -> [Int]
idSorted xs = cond (sorted xs) xs

psort :: [Int] -> [Int]
psort xs = idSorted (perm xs)

main :: [Int]
main = psort [15,14 .. 1]
