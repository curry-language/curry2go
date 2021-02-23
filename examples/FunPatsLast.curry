-- Examples for using functional patterns

last :: Data a => [a] -> a
last (_ ++ [x]) = x

main :: Int
main = last [failed, div 1 0, 42]
