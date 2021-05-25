-- Benchmark to measure sharing across non-determinism

{-# OPTIONS_CYMAKE -Wno-incomplete-patterns #-}

suCC :: Int -> Int
suCC x = x + 1

isdivs :: Int  -> Int -> Bool
isdivs n x = mod x n /= 0

the_filter :: [Int] -> [Int]
the_filter (n:ns) = myfilter (isdivs n) ns

primes :: [Int]
primes = mymap myhead (myiterate the_filter (myiterate suCC 2))

myfilter :: (Int -> Bool) -> [Int] -> [Int]
myfilter _ []     = []
myfilter p (x:xs) = if p x then x : myfilter p xs
                           else myfilter p xs

myiterate :: (a -> a) -> a -> [a]
myiterate f x = x : myiterate f (f x)

mymap :: (a -> b) -> [a] -> [b]
mymap _ []     = []
mymap f (x:xs) = f x : mymap f xs


myhead :: [Int] -> Int
myhead (x : _) = x

at :: [Int] -> Int -> Int
at (x:xs) n = if n==0  then x 
                       else at xs (n - 1)

---------------
-- Permutation sort:

ndinsert :: a -> [a] -> [a]
ndinsert x xs     = (x : xs) ? ndinsert2 x xs

ndinsert2 :: a -> [a] -> [a]
ndinsert2 x (y:ys) = y : ndinsert x ys

perm :: [a] -> [a]
perm []     = []
perm (x:xs) = ndinsert x (perm xs)

sorted :: [Int] -> [Int]
sorted []       = []
sorted [x]      = [x]
sorted (x:y:ys) = guard (x <= y) (x:sorted (y:ys))

psort :: [Int] -> [Int]
psort xs = sorted (perm xs)

guard :: Bool -> a -> a
guard True x = x

myand :: Bool -> Bool -> Bool
myand True y  = y
myand False _ = False

---------------
-- Insertion sort:
isort  :: [Int] -> [Int]
isort []     = []
isort (x:xs) = insert (isort xs)
 where
  insert []        = [x]
  insert zs@(y:ys) | x <= y    = x : zs
                   | otherwise = y : insert ys

---------------

primeList :: [Int]
primeList = [primes!!303, primes!!302, primes!!301, primes!!300]

isort_primes4 :: [Int]
isort_primes4 = isort primeList

psort_primes4 :: [Int]
psort_primes4 = psort primeList

main :: [Int]
main = psort_primes4

psort_primes8 :: [Int]
psort_primes8 =
  psort [primes!!307, primes!!306, primes!!305, primes!!304,
         primes!!303, primes!!302, primes!!301, primes!!300]
