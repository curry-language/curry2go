-- The "classical" xorSelf example from the KiCS2 paper demonstrating
-- that it is necessary to add identifiers to choice structures
-- in order to correctly implement call-time choice.

not :: Bool -> Bool
not True  = False
not False = True

xor :: Bool -> Bool -> Bool
xor False x = x
xor True  x = not x

xorSelf :: Bool -> Bool
xorSelf x = xor x x

aBool :: Bool
aBool = False ? True

main :: Bool
main = xorSelf aBool
