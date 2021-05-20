-- Computing prime numbers via sieve and primitive higher-order functions

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

primesHO_1000 = at primes 1000
primesHO_2000 = at primes 2000

main = primesHO_2000
