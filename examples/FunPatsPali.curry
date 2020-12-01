--------------------------------------------------------------------------------
-- define a palindrome constraint with functional patterns:
pali :: [a] -> Bool
pali (xs ++ reverse xs) = True
pali (xs ++ _ : reverse xs) = True

test1 = pali [True,False,True]         --> True
test2 = (pali [True,False,False,True]) --> True
test3 = (pali [True,False])            --> fail

longPali n = take n (repeat True) ++ take n (repeat False) ++ [False] ++
             take n (repeat False) ++ take n (repeat True)

main = pali (longPali 100)
