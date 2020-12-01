
--------------------------------------------------------------------------------
-- define tree transformations and search by function patterns:
data Exp = Lit Nat | Var VarName | Add Exp Exp | Mul Exp Exp

data Nat = Z | S Nat
data VarName = X1 | X2 | X3
data Position = Lt | Rt

evalTo e = Add (Lit Z) e
         ? Add e (Lit Z)
         ? Mul (Lit (S Z)) e
         ? Mul e (Lit (S Z))

replace _         []    x = x
replace (Add l r) (Lt:p) x = Add (replace l p x) r
replace (Add l r) (Rt:p) x = Add l (replace r p x)
replace (Mul l r) (Lt:p) x = Mul (replace l p x) r
replace (Mul l r) (Rt:p) x = Mul l (replace r p x)

genExpWithVar n = if n==0 then Add (Var X1) (Lit Z)
                          else Mul (Lit (S Z)) (genExpWithVar (n-1))

-- return some variable occurring in an expression:
varInExp :: Exp -> VarName
varInExp (replace _ _ (Var v)) = v

test7 = varInExp (Mul (Var X2) (Var X1)) --> X2 ? X1

-- find a variable in an expression having 2003 nodes
main = varInExp (genExpWithVar 1000) --> X1
