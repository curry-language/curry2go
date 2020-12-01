-- Recursive factorial function.
fac :: Int -> Int
fac n = if n<=0 then 1
                else fac (n-1) * n

main = fac 6
