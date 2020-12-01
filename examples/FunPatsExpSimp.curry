
--------------------------------------------------------------------------------
-- define tree transformations and search by function patterns:
data Exp = Lit Nat | Var VarName | Add Exp Exp | Mul Exp Exp

data Nat = Z | S Nat
data VarName = X1 | X2 | X3
data Position = Lt | Rt

evalTo :: Exp -> Exp
evalTo e = Add (Lit Z) e
         ? Add e (Lit Z)
         ? Mul (Lit (S Z)) e
         ? Mul e (Lit (S Z))

replace :: Exp -> [Position] -> Exp -> Exp
replace _         []    x = x
replace (Add l r) (Lt:p) x = Add (replace l p x) r
replace (Add l r) (Rt:p) x = Add l (replace r p x)
replace (Mul l r) (Lt:p) x = Mul (replace l p x) r
replace (Mul l r) (Rt:p) x = Mul l (replace r p x)

simplify :: Exp -> Exp
simplify (replace c p (evalTo x)) = replace c p x

test5 = (simplify (Mul (Lit (S Z)) (Var X1))) --> (Var X1)

genExpWithMult1 :: Int -> Exp
genExpWithMult1 n = if n==0 then Mul (Lit (S Z)) (Var X1)
                            else Mul (Lit (S (S Z))) (genExpWithMult1 (n-1))

expSize :: Exp -> Int
expSize (Lit _) = 1
expSize (Var _) = 1
expSize (Add e1 e2) = expSize e1 + expSize e2 + 1
expSize (Mul e1 e2) = expSize e1 + expSize e2 + 1

-- make a single simplifcation step in an expression having 2003 nodes
main :: Int
main = expSize (simplify (genExpWithMult1 1000)) --> 2001
