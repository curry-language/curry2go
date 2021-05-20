-- Examples for duplicating non-deterministic computations when
-- pure pull-tabbing is applied.

{-# OPTIONS_CYMAKE -Wno-incomplete-patterns #-}

-- arbitrary number between 1 and n:
someNum :: Int -> Int
someNum n = if (n<=0) then 0 else (n ? someNum (n-1))

isZero :: Int -> Bool
isZero 0 = True

-- Duplication of a non-deterministic number:
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

addSomeNum10 :: Int -> Int
addSomeNum10 n = let x = someNum n in x+x+x+x+x+x+x+x+x+x+x+x

addNum_1 :: Bool
addNum_1 = isZero (addSomeNum1 2000)

addNum_2 :: Bool
addNum_2 = isZero (addSomeNum2 2000)

addNum_3 :: Bool
addNum_3 = isZero (addSomeNum3 2000)

addNum_4 :: Bool
addNum_4 = isZero (addSomeNum4 2000)

addNum_5 :: Bool
addNum_5 = isZero (addSomeNum5 2000)

addNum_10 :: Bool
addNum_10 = isZero (addSomeNum10 2000)

main :: Bool
main = addNum_10
