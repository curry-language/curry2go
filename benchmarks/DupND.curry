-- Examples for duplicating non-deterministic computations:

-- arbitrary number between 1 and n:
someNum :: Int -> Int
someNum n = if (n<=0) then 0 else (n ? someNum (n-1))

isZero :: Int -> Bool
isZero 0 = True

-- lazy non-deterministic duplication:
addSomeNum1 :: Int -> Int
addSomeNum1 n = let x = someNum n in x

addSomeNum2 :: Int -> Int
addSomeNum2 n = let x = someNum n in x+x

addSomeNum3 :: Int -> Int
addSomeNum3 n = let x = someNum n in x+x+x

addSomeNum4 :: Int -> Int
addSomeNum4 n = let x = someNum n in x+x+x+x

addSomeNum5 :: Int -> Int
addSomeNum5 n = let x = someNum n in x+x+x+x+x

addNum1 :: Bool
addNum1 = isZero (addSomeNum1 2000)

addNum2 :: Bool
addNum2 = isZero (addSomeNum2 2000)

addNum3 :: Bool
addNum3 = isZero (addSomeNum3 2000)

addNum4 :: Bool
addNum4 = isZero (addSomeNum4 2000)

addNum5 :: Bool
addNum5 = isZero (addSomeNum5 2000)

main :: Bool
main = addNum5

-- arbitrary tuples between 1 and n:
ndpair :: Int -> (Int,Int)
ndpair n   = if (n==1) then (1,1)   else ((n,n)   ? ndpair (n-1))

ndtriple :: Int -> (Int,Int,Int)
ndtriple n = if (n==1) then (1,1,1) else ((n,n,n) ? ndtriple (n-1))

-- lazy non-deterministic duplication:
addPair :: Int -> Int
addPair   n = x+y   where (x,y) = ndpair n

addTriple :: Int -> Int
addTriple n = x+y+z where (x,y,z) = ndtriple n

main10 :: Int
main10 = addPair   2000

main11 :: Int
main11 = addTriple 2000
