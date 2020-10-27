-- Example: naive reverse on integer lists

append :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x : (append xs ys)

rev :: [a] -> [a]
rev []     = []
rev (x:xs) = append (rev xs) [x]

main :: [Int]
main = rev [1,2,3,4]

