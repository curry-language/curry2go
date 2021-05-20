-- A purely functional benchmark: naive reverse

data Nat = O | S Nat

add :: Nat -> Nat -> Nat
add O     n = n
add (S m) n = S (add m n)

double :: Nat -> Nat
double x = add x x

mult :: Nat -> Nat -> Nat
mult O     _ = O
mult (S m) n = add n (mult m n)

two :: Nat
two = S (S O)

four :: Nat
four = double two

nat16 :: Nat
nat16 = mult four four

nat256 :: Nat
nat256 = mult nat16 nat16

nat4096 :: Nat
nat4096 = mult nat256 nat16

nat16384 :: Nat
nat16384 = mult nat4096 four

app :: [a] -> [a] -> [a]
app []     ys = ys
app (x:xs) ys = x : app xs ys

rev :: [a] -> [a]
rev []     = []
rev (x:xs) = app (rev xs) [x]

isList :: [_] -> Bool
isList []     = True
isList (x:xs) = isList xs

natList :: Nat -> [Nat]
natList O     = []
natList (S x) = (S x) : (natList x)

cond :: Bool -> a -> a
cond True x = x

nfList :: [a] -> [a]
nfList xs = cond (isList xs) xs

goal1 :: [Nat]
goal1 = nfList (rev (natList nat16))

goal2 :: [Nat]
goal2 = nfList (rev (natList nat256))

nrev_4096 :: Bool
nrev_4096  = isList (rev (natList nat4096))

nrev_16384 :: Bool
nrev_16384 = isList (rev (natList nat16384))

main :: Bool
main = nrev_4096
