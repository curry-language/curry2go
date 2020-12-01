-- Example for case expressions over literals.

isZero :: Int -> Bool
isZero 0 = True
isZero 2 = True

main = isZero x &> x where x free

isSpecialChar :: Char -> Bool
isSpecialChar '\n' = True
isSpecialChar '\b' = True
isSpecialChar ' '  = True

main1 = isSpecialChar x &> x where x free

-- Allowed but not meaningful...
isSomeFloat :: Float -> Bool
isSomeFloat 0.0 = True
isSomeFloat 3.14159 = True

main2 = isSomeFloat x &> x where x free
