-- Examples for using unification

last xs | xs =:= _ ++ [x] = x
 where x free

main :: (Bool, Int)
main = (last [False,True], last [0,1,2,3,4,5*5])


main0 :: Int
main0 = last [0,1,2,3,4,5*5]

main1 = 1+1 =:= 2

main2 :: Int
main2 = (1+1 =:= x) &> x where x free

main3 = (x =:= [True]) &> x where x free

main4 :: Int
main4 = ([1+1] =:= [x]) &> x where x free

main5 = id x =:= x &> x where x free

main6 = id x =:= y &> (x,y,z) where x,y,z free
