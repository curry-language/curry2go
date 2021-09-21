--------------------------------------------------------------------------------
-- Dijsktra's Dutch National Flag problem with function patterns

data Color = Red | White | Blue
 deriving (Eq,Show)

solve (x++[White]++y++[Red  ]++z) = solve (x++[Red]++y++[White]++z)
solve (x++[Blue ]++y++[Red  ]++z) = solve (x++[Red]++y++[Blue]++z)
solve (x++[Blue ]++y++[White]++z) = solve (x++[White]++y++[Blue]++z)
solve flag | isDutchFlag flag = flag
 where
  isDutchFlag (uni Red ++ uni White ++ uni Blue) = success

uni :: Color -> [Color]
uni _ = []
uni color = color : uni color

main :: [Color]
main = solve [White,Red,Blue,Red,Blue,White]
-- > [Red, Red, White, White, Blue, Blue]
