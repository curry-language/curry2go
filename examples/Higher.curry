
not True  = False
not False = True

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

main :: [Bool]
main = map not [True,False]

main1 :: [Maybe Int]
main1 = map Just [1,2,3]

-- Partial applications as result:
id :: a -> a
id x = x

fsnd :: a -> b -> b
fsnd _ = id

main2 :: [Int]
main2 = map (fsnd 42) [1,2,3]
