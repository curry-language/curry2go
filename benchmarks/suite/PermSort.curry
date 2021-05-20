
insert x [] = [x]
insert x (y:ys) = x:y:ys ? y : (insert x ys)

perm [] = []
perm (x:xs) = insert x (perm xs)

sorted :: [Int] -> [Int]
sorted []       = []
sorted [x]      = [x]
sorted (x:y:ys) | x <= y = x : sorted (y:ys)

psort xs = sorted (perm xs)

sortDescList n = psort (2:[n,n-1 .. 3]++[1])

--main = psort [2,14,13,12,11,10,9,8,7,6,5,4,3,1]
main = sortDescList 13
