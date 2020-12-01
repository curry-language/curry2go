
-- infinite list of 1,2,1,2,...
oneTwo :: [Int]
oneTwo = let x = 1 : y
             y = 2 : x
         in x

head :: [a] -> a
head (x:_) = x

main :: Int
main = head oneTwo
