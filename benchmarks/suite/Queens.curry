-- Compute the number of solutions to queens placements.
-- This implementation uses prelude operations and list comprehensions,
-- thus, higher-order operations like `map`.

queens nq = length (gen nq)
 where
  gen :: Int -> [[Int]]
  gen n = if n==0
            then [[]]
            else [ (q:b) | b <- gen (n-1), q <- [1..nq], safe q 1 b]

safe :: Int -> Int -> [Int] -> Bool
safe _ _ [] = True
safe x d (q:l) = x /= q && x /= q+d && x /= q-d && safe x (d+1) l

queens_10 = queens 10
queens_11 = queens 11

main = queens_11
