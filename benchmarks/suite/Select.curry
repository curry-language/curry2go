{-# OPTIONS_CYMAKE -Wno-incomplete-patterns #-}

-- Returns some element of a list (same as `Prelude.anyOf`).
someOf :: [a] -> a
someOf (x:xs) = x ? someOf xs

-- Deletes the first occurrence of an element in a list.
del :: Eq a => a -> [a] -> [a]
del x (y:ys) = if x == y then ys else y : del x ys

-- Returns the sum of all list elements.
sum :: [Int] -> Int
sum []     = 0
sum (x:xs) = x + sum xs

-- Returns the sum of a value plus the sum of a list without the first
-- occurrence of this value.
sumUp :: [Int] -> Int -> Int
sumUp xs m = m + sum (del m xs)

-- Returns all `sumUp` for some non-deterministically select element.
select :: [Int] -> Int
select xs = sumUp xs (someOf xs)

select_50 :: Int
select_50 = select [1..50]

select_75 :: Int
select_75 = select [1..75]

select_100 :: Int
select_100 = select [1..100]

select_150 :: Int
select_150 = select [1..150]
