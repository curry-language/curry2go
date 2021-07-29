-- An example testing the correct treatment of owner task setting
-- in memoized pull-tab steps. If the new nodes in a pull-tab step
-- (children of the resulting choice node) obtain owner tasks
-- of the new processes, as in
--
--     evalArg_0 (?_0 0 1) -> ?_0 (evalArg_1 0) (evalArg_2 1)
-- 
-- then a wrong result will be computed. If the new nodes gets the
-- owner tasks of the current task, as in
--
--     evalArg_0 (?_0 0 1) -> ?_0 (evalArg_0 0) (evalArg_0 1)
--
-- the result will be correct.

data B = F | T | B

coin :: B
coin = F
coin = T

toTupel :: B -> B -> (B,B)
toTupel F b = toTupelF b
toTupel T b = toTupelT b

toTupelF :: B -> (B,B)
toTupelF F = (F, F)
toTupelF T = (F, T)

toTupelT :: B -> (B,B)
toTupelT F = (T, F)
toTupelT T = (T, T)

evalArg :: B -> B
evalArg F = F
evalArg T = T

test :: B -> (B,B)
test x = toTupel (evalArg x) (evalArg (x ? B))

main :: (B,B)
main = test coin
-- Results:
-- (F,F)
-- (T,T)
