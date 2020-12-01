-- Examples for using functional patterns

last :: [a] -> a
last (_ ++ [x]) = x

main :: Int
main = last [failed, div 1 0, 42]
