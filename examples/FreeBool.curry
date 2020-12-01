-- Example for free Boolean variables.

not :: Bool -> Bool
not True  = False
not False = True

xor :: Bool -> Bool -> Bool
xor False x = x
xor True  x = not x

xorSelf :: Bool -> Bool
xorSelf x = xor x x

main :: Bool
main = xorSelf b where b free
