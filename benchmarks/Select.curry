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

select50 :: Int
select50 = select [1..50]

select75 :: Int
select75 = select [1..75]

select100 :: Int
select100 = select [1..100]

select150 :: Int
select150 = select [1..150]

main :: Int
main = select100
