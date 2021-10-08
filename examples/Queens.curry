{-# OPTIONS_FRONTEND -Wno-overlapping #-}

-- A permuation of a list.
perm :: [a] -> [a]
perm []     = []
perm (x:xs) = insert (perm xs)
 where insert ys     = x : ys
       insert (y:ys) = y : insert ys

-- A solution to the queens problem is a safe permutation.
queens :: Int -> [Int]
queens n | safe xs = xs where xs = perm [1..n]

-- A position is safe of no queen can attack another.
safe :: [Int] -> Bool
safe []     = True
safe (x:xs) = noattack 1 x xs &> safe xs

noattack :: Int -> Int -> [Int] -> Bool
noattack _ _ []                                = True
noattack n x (y:ys) | x/=y && x/=y+n && x/=y-n = noattack (n+1) x ys

main :: [Int]
main = queens 4
