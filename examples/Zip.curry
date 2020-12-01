-- Testing deep patterns

zip               :: [a] -> [b] -> [(a,b)]
zip []     _      = []
zip (_:_)  []     = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

ones :: [Int]
ones = let xs = 1 : xs in xs

main :: [(Int,Int)]
main = zip [1,2,3] ones
