
-- computing permutations
ndinsert x xs     = x : xs
ndinsert x (y:ys) = y : ndinsert x ys

perm []     = []
perm (x:xs) = ndinsert x (perm xs)

main :: [Int]
main = perm [1,2,3,4]
