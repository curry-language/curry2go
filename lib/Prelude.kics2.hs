{-# LANGUAGE BangPatterns, CPP, MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables #-}

import qualified Control.Exception as C

-- ATTENTION: Do not introduce line breaks in import declarations as these
-- are not recognized!
import Data.Char (chr, ord)
import GHC.Exts (Double (D#), Double#, (==##), (<=##), negateDouble#)
import GHC.Exts (Char (C#), Char#, eqChar#, leChar#)
import System.IO

import CurryException
import KiCS2Debug     (internalError)
import FailInfo       (customFail)
import PrimTypes

#if __GLASGOW_HASKELL__ > 706
import GHC.Exts (isTrue#)
#endif

-- #endimport - do not remove this line!

#if !(__GLASGOW_HASKELL__ > 706)
isTrue# :: Bool -> Bool
{-# INLINE isTrue# #-}
isTrue# x = x
#endif

-- -----------------------------------------------------------------------------
-- Type aliases for compatibility (the frontend no longer generates these,
-- therefore they are redeclared here as some runtime modules rely on them)
-- -----------------------------------------------------------------------------

type C_String = OP_List C_Char

type C_Success = C_Bool

-- -----------------------------------------------------------------------------
-- Int representation
-- -----------------------------------------------------------------------------

-- BEGIN GENERATED FROM PrimTypes.curry
data C_Int
     = C_Int Integer
     | C_CurryInt BinInt
     | Choice_C_Int Cover ID C_Int C_Int
     | Choices_C_Int Cover ID ([C_Int])
     | Fail_C_Int Cover FailInfo
     | Guard_C_Int Cover Constraints C_Int

instance Show C_Int where
  showsPrec d (Choice_C_Int cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Int cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Int cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Int _ _) = showChar '!'
  showsPrec d (C_Int x1) = shows x1
  showsPrec d (C_CurryInt x1) = case ((\x _ _ -> x) $## x1) (error "Show C_Int: nesting depth used") emptyCs of
    Choice_BinInt _ _ _ _ -> shows x1
    Choices_BinInt _ _ _  -> shows x1
    Fail_BinInt _ _       -> shows x1
    Guard_BinInt _ _ _    -> shows x1
    gnfBinInt             -> shows (curryint2primint gnfBinInt)

instance Read C_Int where
  readsPrec d s = map readInt (readsPrec d s) where readInt (i, s) = (C_Int i, s)

instance NonDet C_Int where
  choiceCons = Choice_C_Int
  choicesCons = Choices_C_Int
  failCons = Fail_C_Int
  guardCons = Guard_C_Int
  try (Choice_C_Int cd i x y) = tryChoice cd i x y
  try (Choices_C_Int cd i xs) = tryChoices cd i xs
  try (Fail_C_Int cd info) = Fail cd info
  try (Guard_C_Int cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Int cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Int cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Int cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Int _  i _) = error ("Prelude.Int.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Int cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Int cd cs e) = f cd cs e
  match _ _ _ _ _ f x = f x

instance Generable C_Int where
  generate s cd = Choices_C_Int cd (freeID [1] s) [C_CurryInt (generate (leftSupply s) cd)]

instance NormalForm C_Int where
  ($!!) cont x@(C_Int _) cd cs = cont x cd cs
  ($!!) cont (C_CurryInt x1) cd cs = ((\y1 -> cont (C_CurryInt y1)) $!! x1) cd cs
  ($!!) cont (Choice_C_Int d i x y) cd cs = nfChoice cont d i x y cd cs
  ($!!) cont (Choices_C_Int d i xs) cd cs = nfChoices cont d i xs cd cs
  ($!!) cont (Guard_C_Int d c x) cd cs = guardCons d c ((cont $!! x) cd $! (addCs c cs))
  ($!!) _ (Fail_C_Int cd info) _ _ = failCons cd info
  ($##) cont x@(C_Int _) cd cs = cont x cd cs
  ($##) cont (C_CurryInt x1) cd cs = ((\y1 -> cont (C_CurryInt y1)) $## x1) cd cs
  ($##) cont (Choice_C_Int d i x y) cd cs = gnfChoice cont d i x y cd cs
  ($##) cont (Choices_C_Int d i xs) cd cs = gnfChoices cont d i xs cd cs
  ($##) cont (Guard_C_Int d c x) cd cs = guardCons d c ((cont $## x) cd $! (addCs c cs))
  ($##) _ (Fail_C_Int d info) _ _ = failCons d info
  searchNF search cont x@(C_Int _) = cont x
  searchNF search cont (C_CurryInt x1) = search (\y1 -> cont (C_CurryInt y1)) x1
  searchNF _ _ x = error ("Prelude.Int.searchNF: no constructor: " ++ (show x))

instance Unifiable C_Int where
  (=.=) (C_Int      x1) (C_Int      y1) cd _  = if x1 == y1 then C_True else Fail_C_Bool cd defFailInfo
  (=.=) (C_Int      x1) (C_CurryInt y1) cd cs = ((primint2curryint x1) =:= y1) cd cs
  (=.=) (C_CurryInt x1) (C_Int      y1) cd cs = (x1 =:= (primint2curryint y1)) cd cs
  (=.=) (C_CurryInt x1) (C_CurryInt y1) cd cs = (x1 =:= y1) cd cs
  (=.=) _               _               cd _  = Fail_C_Bool cd defFailInfo
  (=.<=) (C_Int      x1) (C_Int      y1) cd _ = if x1 == y1 then C_True else Fail_C_Bool cd defFailInfo
  (=.<=) (C_Int      x1) (C_CurryInt y1) cd cs = ((primint2curryint x1) =:<= y1) cd cs
  (=.<=) (C_CurryInt x1) (C_Int      y1) cd cs = (x1 =:<= (primint2curryint y1)) cd cs
  (=.<=) (C_CurryInt x1) (C_CurryInt y1) cd cs = (x1 =:<= y1) cd cs
  (=.<=) _ _ cd _= Fail_C_Bool cd defFailInfo
  bind cd i (C_Int      x2) = (i :=: ChooseN 0 1) : bind cd (leftID i) (primint2curryint x2)
  bind cd i (C_CurryInt x2) = (i :=: ChooseN 0 1) : bind cd (leftID i) x2
  bind cd i (Choice_C_Int d j l r) = [(ConstraintChoice d j (bind cd i l) (bind cd i r))]
  bind cd i (Choices_C_Int d j@(FreeID _ _) xs) = bindOrNarrow cd i d j xs
  bind cd i (Choices_C_Int d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (bind cd i) xs))]
  bind _  _ c@(Choices_C_Int _ i@(ChoiceID _) _) = error ("Prelude.Int.bind: Choices with ChoiceID: " ++ (show c))
  bind _ _ (Fail_C_Int _ info) = [Unsolvable info]
  bind cd  i (Guard_C_Int _ cs e) = getConstrList cs ++ (bind cd i e)
  lazyBind cd i (C_Int      x2) = [i :=: ChooseN 0 1, leftID i :=: LazyBind (lazyBind cd (leftID i) (primint2curryint x2))]
  lazyBind cd i (C_CurryInt x2) = [i :=: ChooseN 0 1, leftID i :=: LazyBind (lazyBind cd (leftID i) x2)]
  lazyBind cd i (Choice_C_Int d j l r) = [(ConstraintChoice d j (lazyBind cd i l) (lazyBind cd i r))]
  lazyBind cd i (Choices_C_Int d j@(FreeID _ _) xs) = lazyBindOrNarrow cd i d j xs
  lazyBind cd i (Choices_C_Int d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (lazyBind cd i) xs))]
  lazyBind _  _ c@(Choices_C_Int _ i@(ChoiceID _) _) = error ("Prelude.Int.lazyBind: Choices with ChoiceID: " ++ (show c))
  lazyBind _  _ (Fail_C_Int _ info) = [Unsolvable info]
  lazyBind cd i (Guard_C_Int _ cs e) = getConstrList cs ++ [(i :=: (LazyBind (lazyBind cd i e)))]

instance Curry C_Int
-- END GENERATED FROM PrimTypes.curry

d_C_prim_eqInt :: C_Int -> C_Int -> Cover -> ConstStore -> C_Bool
d_C_prim_eqInt (Choice_C_Int d i x y) z cd cs =
  narrow d i ((x `d_C_prim_eqInt` z) cd cs) ((y `d_C_prim_eqInt` z) cd cs)
d_C_prim_eqInt (Choices_C_Int d i xs) y cd cs =
  narrows cs d i (\x -> (x `d_C_prim_eqInt` y) cd cs) xs
d_C_prim_eqInt (Guard_C_Int d c x) y cd cs =
  guardCons d c ((x `d_C_prim_eqInt` y) cd $! (addCs c cs))
d_C_prim_eqInt (Fail_C_Int d info) _ _ _ = failCons d info
d_C_prim_eqInt z (Choice_C_Int d i x y) cd cs =
  narrow d i ((z `d_C_prim_eqInt` x) cd cs) ((z `d_C_prim_eqInt` y) cd cs)
d_C_prim_eqInt y (Choices_C_Int d i xs) cd cs =
  narrows cs d i (\x -> (y `d_C_prim_eqInt` x) cd cs) xs
d_C_prim_eqInt y (Guard_C_Int d c x) cd cs =
  guardCons d c ((y `d_C_prim_eqInt` x) cd $! (addCs c cs))
d_C_prim_eqInt _ (Fail_C_Int d info) _ _ = failCons d info
d_C_prim_eqInt (C_Int      x1) (C_Int      y1) _ _ = toCurry (x1 == y1)
d_C_prim_eqInt (C_Int      x1) (C_CurryInt y1) cd cs =
  ((primint2curryint x1) `d_C_prim_eqBinInt` y1) cd cs
d_C_prim_eqInt (C_CurryInt x1) (C_Int      y1) cd cs =
  (x1 `d_C_prim_eqBinInt` (primint2curryint y1)) cd cs
d_C_prim_eqInt (C_CurryInt x1) (C_CurryInt y1) cd cs =
  (x1 `d_C_prim_eqBinInt` y1) cd cs

d_C_prim_ltEqInt :: C_Int -> C_Int -> Cover -> ConstStore -> C_Bool
d_C_prim_ltEqInt (Choice_C_Int d i x y) z cd cs =
  narrow d i ((x `d_C_prim_ltEqInt` z) cd cs) ((y `d_C_prim_ltEqInt` z) cd cs)
d_C_prim_ltEqInt (Choices_C_Int d i xs) y cd cs =
  narrows cs d i (\x -> (x `d_C_prim_ltEqInt` y) cd cs) xs
d_C_prim_ltEqInt (Guard_C_Int d c x) y cd cs =
  guardCons d c ((x `d_C_prim_ltEqInt` y) cd $! (addCs c cs))
d_C_prim_ltEqInt (Fail_C_Int d info) _ _ _ = failCons d info
d_C_prim_ltEqInt z (Choice_C_Int d i x y) cd cs =
  narrow d i ((z `d_C_prim_ltEqInt` x) cd cs) ((z `d_C_prim_ltEqInt` y) cd cs)
d_C_prim_ltEqInt y (Choices_C_Int d i xs) cd cs =
  narrows cs d i (\x -> (y `d_C_prim_ltEqInt` x) cd cs) xs
d_C_prim_ltEqInt y (Guard_C_Int d c x) cd cs =
  guardCons d c ((y `d_C_prim_ltEqInt` x) cd $! (addCs c cs))
d_C_prim_ltEqInt _ (Fail_C_Int d info) _ _ = failCons d info
d_C_prim_ltEqInt (C_Int      x1) (C_Int      y1) _ _ = toCurry (x1 <= y1)
d_C_prim_ltEqInt (C_Int      x1) (C_CurryInt y1) cd cs =
  ((primint2curryint x1) `d_C_lteqInteger` y1) cd cs
d_C_prim_ltEqInt (C_CurryInt x1) (C_Int      y1) cd cs =
  (x1 `d_C_lteqInteger` (primint2curryint y1)) cd cs
d_C_prim_ltEqInt (C_CurryInt x1) (C_CurryInt y1) cd cs =
  (x1 `d_C_lteqInteger` y1) cd cs

external_d_C_eqInt :: C_Int -> C_Int -> Cover -> ConstStore -> C_Bool
external_d_C_eqInt  = d_C_prim_eqInt

external_d_C_ltEqInt :: C_Int -> C_Int -> Cover -> ConstStore -> C_Bool
external_d_C_ltEqInt  = d_C_prim_ltEqInt

primint2curryint :: Integer -> BinInt
primint2curryint n
  | n <  0    = Neg (primint2currynat (negate n))
  | n == 0    = Zero
  | otherwise = Pos (primint2currynat n)

primint2currynat :: Integer -> Nat
primint2currynat n
  | n == 1           = IHi
  | (n `rem` 2) == 0 = O (primint2currynat (n `quot` 2))
  | otherwise        = I (primint2currynat (n `quot` 2))

curryint2primint :: BinInt -> Integer
curryint2primint Zero    = 0
curryint2primint (Pos n) = currynat2primint n
curryint2primint (Neg n) = negate (currynat2primint n)
curryint2primint int     = error ("KiCS2 error: Prelude.curryint2primint: no ground term, but " ++ show int)

currynat2primint :: Nat -> Integer
currynat2primint IHi   = 1
currynat2primint (O n) = 2 * currynat2primint n
currynat2primint (I n) = 2 * currynat2primint n + 1
currynat2primint nat   = error ("KiCS2 error: Prelude.currynat2primint: no ground term, but " ++ show nat)

-- -----------------------------------------------------------------------------
-- Float representation
-- -----------------------------------------------------------------------------

-- BEGIN GENERATED FROM PrimTypes.curry

data C_Float
     = C_Float Double#
     | Choice_C_Float Cover ID C_Float C_Float
     | Choices_C_Float Cover ID ([C_Float])
     | Fail_C_Float Cover FailInfo
     | Guard_C_Float Cover (Constraints) C_Float

instance Show C_Float where
  showsPrec d (Choice_C_Float cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Float cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Float cd c e) = showsGuard d cd c e
  showsPrec d (Fail_C_Float _ _) = showChar '!'
  showsPrec d (C_Float x1) = shows (D# x1)

instance Read C_Float where
  readsPrec d s = map readFloat (readsPrec d s) where readFloat (D# d, s) = (C_Float d, s)

instance NonDet C_Float where
  choiceCons = Choice_C_Float
  choicesCons = Choices_C_Float
  failCons = Fail_C_Float
  guardCons = Guard_C_Float
  try (Choice_C_Float cd i x y) = tryChoice cd i x y
  try (Choices_C_Float cd i xs) = tryChoices cd i xs
  try (Fail_C_Float cd info) = Fail cd info
  try (Guard_C_Float cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Float cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Float cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Float cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Float cd i@(ChoiceID _) _) = error ("Prelude.Float.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Float cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Float cd cs e) = f cd cs e
  match _ _ _ _ _ f x = f x

instance Generable C_Float where
  generate = error "No generator for C_Float"

instance NormalForm C_Float where
  ($!!) cont x@(C_Float _) cd cs = cont x cd cs
  ($!!) cont (Choice_C_Float d i x y) cd cs = nfChoice cont d i x y cd cs
  ($!!) cont (Choices_C_Float d i xs) cd cs = nfChoices cont d i xs cd cs
  ($!!) cont (Guard_C_Float d c x) cd cs = guardCons d c ((cont $!! x) cd $! (addCs c cs))
  ($!!) _ (Fail_C_Float d info) _ _ = failCons d info
  ($##) cont x@(C_Float _) cd cs = cont x cd cs
  ($##) cont (Choice_C_Float d i x y) cd cs = gnfChoice cont d i x y cd cs
  ($##) cont (Choices_C_Float d i xs) cd cs = gnfChoices cont d i xs cd cs
  ($##) cont (Guard_C_Float d c x) cd cs = guardCons d c ((cont $## x) cd $! (addCs c cs))
  ($##) _ (Fail_C_Float d info) _ _ = failCons d info
  searchNF search cont x@(C_Float _) = cont x
  searchNF _ _ x = error ("Prelude.Float.searchNF: no constructor: " ++ (show x))

instance Unifiable C_Float where
  (=.=) (C_Float x1) (C_Float y1) cd _  = if isTrue# (x1 ==## y1) then C_True else Fail_C_Bool cd defFailInfo
  (=.<=) (C_Float x1) (C_Float y1) cd _  = if isTrue# (x1 ==## y1) then C_True else Fail_C_Bool cd defFailInfo
  bind cd i (Choice_C_Float d j l r) = [(ConstraintChoice d j (bind cd i l) (bind cd i r))]
  bind cd i (Choices_C_Float d j@(FreeID _ _) xs) = bindOrNarrow cd i d j xs
  bind cd i (Choices_C_Float d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (bind cd i) xs))]
  bind _  _ c@(Choices_C_Float _ i _) = error ("Prelude.Float.bind: Choices with ChoiceID: " ++ (show c))
  bind _  _ (Fail_C_Float _ info) = [Unsolvable info]
  bind cd i (Guard_C_Float _ cs e) = getConstrList cs ++ (bind cd i e)
  lazyBind cd i (Choice_C_Float d j l r) = [(ConstraintChoice d j (lazyBind cd i l) (lazyBind cd i r))]
  lazyBind cd i (Choices_C_Float d j@(FreeID _ _) xs) = lazyBindOrNarrow cd i d j xs
  lazyBind cd i (Choices_C_Float d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (lazyBind cd i) xs))]
  lazyBind _  _ c@(Choices_C_Float _ i _) = error ("Prelude.Float.lazyBind: Choices with ChoiceID: " ++ (show c))
  lazyBind _  _ (Fail_C_Float _ info) = [Unsolvable info]
  lazyBind cd  i (Guard_C_Float _ cs e) = getConstrList cs ++ [(i :=: (LazyBind (lazyBind cd i e)))]

instance Curry C_Float
-- END GENERATED FROM PrimTypes.curry

d_C_prim_eqFloat :: C_Float -> C_Float -> Cover -> ConstStore -> C_Bool
d_C_prim_eqFloat (Choice_C_Float d i x y) z cd cs =
  narrow d i ((x `d_C_prim_eqFloat` z) cd cs) ((y `d_C_prim_eqFloat` z) cd cs)
d_C_prim_eqFloat (Choices_C_Float d i xs) y cd cs =
  narrows cs d i (\x -> (x `d_C_prim_eqFloat` y) cd cs) xs
d_C_prim_eqFloat (Guard_C_Float d c x) y cd cs =
  guardCons d c ((x `d_C_prim_eqFloat` y) cd  $! (addCs c cs))
d_C_prim_eqFloat (Fail_C_Float d info) _ _ _= failCons d info
d_C_prim_eqFloat z (Choice_C_Float d i x y) cd cs =
  narrow d i ((z `d_C_prim_eqFloat` x) cd cs) ((z `d_C_prim_eqFloat` y) cd cs)
d_C_prim_eqFloat y (Choices_C_Float d i xs) cd cs =
  narrows cs d i (\x -> (y `d_C_prim_eqFloat` x) cd cs) xs
d_C_prim_eqFloat y (Guard_C_Float d c x) cd cs =
  guardCons d c ((y `d_C_prim_eqFloat` x) cd  $! (addCs c cs))
d_C_prim_eqFloat _ (Fail_C_Float d info) _ _ = failCons d info
d_C_prim_eqFloat (C_Float x1) (C_Float y1) _ _ = toCurry (isTrue# (x1 ==## y1))

d_C_prim_ltEqFloat :: C_Float -> C_Float -> Cover -> ConstStore -> C_Bool
d_C_prim_ltEqFloat (Choice_C_Float d i x y) z cd cs =
  narrow d i ((x `d_C_prim_ltEqFloat` z) cd cs) ((y `d_C_prim_ltEqFloat` z) cd cs)
d_C_prim_ltEqFloat (Choices_C_Float d i xs) y cd cs =
  narrows cs d i (\x -> (x `d_C_prim_ltEqFloat` y) cd cs) xs
d_C_prim_ltEqFloat (Guard_C_Float d c x) y cd cs =
  guardCons d c ((x `d_C_prim_ltEqFloat` y) cd $! (addCs c cs))
d_C_prim_ltEqFloat (Fail_C_Float d info) _ _ _ = failCons d info
d_C_prim_ltEqFloat z (Choice_C_Float d i x y) cd cs =
  narrow d i ((z `d_C_prim_ltEqFloat` x) cd cs) ((z `d_C_prim_ltEqFloat` y) cd cs)
d_C_prim_ltEqFloat y (Choices_C_Float d i xs) cd cs =
  narrows cs d i (\x -> (y `d_C_prim_ltEqFloat` x) cd cs) xs
d_C_prim_ltEqFloat y (Guard_C_Float d c x) cd cs =
  guardCons d c ((y `d_C_prim_ltEqFloat` x) cd $! (addCs c cs))
d_C_prim_ltEqFloat _ (Fail_C_Float d info) _ _ = failCons d info
d_C_prim_ltEqFloat (C_Float x1) (C_Float y1) _ _ = toCurry (isTrue# (x1 <=## y1))

external_d_C_eqFloat :: C_Float -> C_Float -> Cover -> ConstStore -> C_Bool
external_d_C_eqFloat  = d_C_prim_eqFloat

external_d_C_ltEqFloat :: C_Float -> C_Float -> Cover -> ConstStore -> C_Bool
external_d_C_ltEqFloat  = d_C_prim_ltEqFloat

-- ---------------------------------------------------------------------------
-- Char
-- ---------------------------------------------------------------------------

-- BEGIN GENERATED FROM PrimTypes.curry

data C_Char
     = C_Char Char#
     | CurryChar BinInt
     | Choice_C_Char Cover ID C_Char C_Char
     | Choices_C_Char Cover ID ([C_Char])
     | Fail_C_Char Cover FailInfo
     | Guard_C_Char Cover (Constraints) C_Char

instance Show C_Char where
  showsPrec d (Choice_C_Char cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Char cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Char cd c e) = showsGuard d d c e
  showsPrec d (Fail_C_Char _ _) = showChar '!'
  showsPrec d (C_Char x1) = showString (show (C# x1))
  showsPrec d (CurryChar x1) = case ((\x _ _ -> x) $## x1) (error "Show C_Char: nesting depth used") emptyCs of
    Choice_BinInt _ _ _ _ -> showString "chr " . shows x1
    Choices_BinInt _ _ _  -> showString "chr " . shows x1
    Fail_BinInt _ _       -> shows x1
    Guard_BinInt _ _ _    -> shows x1
    gnfBinInt             -> shows (C# (curryChar2primChar gnfBinInt))

  showList cs | all isPrimChar cs' = showList     (map convert cs')
              | otherwise          = showCharList cs'
   where
    cs' = map gnfCurryChar cs

    gnfCurryChar :: C_Char -> C_Char
    gnfCurryChar (CurryChar x1) = case ((\x _ _ -> x) $## x1) (error "gnfCurryChar: nesting depth used") emptyCs of
      Choice_BinInt _ _ _ _ -> CurryChar x1
      Choices_BinInt _ _ _  -> CurryChar x1
      Fail_BinInt _ _       -> CurryChar x1
      Guard_BinInt _ _ _    -> CurryChar x1
      gnfBinInt             -> C_Char (curryChar2primChar gnfBinInt)
    gnfCurryChar c              = c

    isPrimChar (C_Char _) = True
    isPrimChar _          = False

    convert (C_Char c) = C# c

    showCharList []     = showString "[]"
    showCharList (x:xs) = showChar '[' . shows x . showRest xs
      where
        showRest []     = showChar ']'
        showRest (y:ys) = showChar ',' . shows y . showRest ys

instance Read C_Char where
  readsPrec d s = map readChar (readsPrec d s) where readChar (C# c, s) = (C_Char c, s)

  readList s = map readString (readList s) where readString (cs, s) = (map (\(C# c) -> C_Char c) cs, s)

instance NonDet C_Char where
  choiceCons = Choice_C_Char
  choicesCons = Choices_C_Char
  failCons = Fail_C_Char
  guardCons = Guard_C_Char
  try (Choice_C_Char cd i x y) = tryChoice cd i x y
  try (Choices_C_Char cd i xs) = tryChoices cd i xs
  try (Fail_C_Char cd info) = Fail cd info
  try (Guard_C_Char cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Char cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Char cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Char cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Char cd i _) = error ("Prelude.Char.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Char cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Char cd cs e) = f cd cs e
  match _ _ _ _ _ f x = f x

instance Generable C_Char where
  generate s cd = Choices_C_Char cd (freeID [1] s)
                    [CurryChar (generateNNBinInt (leftSupply s) cd)]
   where
    -- generate only non-negative ord values for characters:
    generateNNBinInt s c =
      Choices_BinInt c (freeID [1, 0, 1] s)
        [Fail_BinInt c (customFail "no negative ord values for characters"),
         Zero, Pos (generate (leftSupply s) c)]

instance NormalForm C_Char where
  ($!!) cont x@(C_Char _) cd cs = cont x cd cs
  ($!!) cont (CurryChar x) cd cs = ((cont . CurryChar) $!! x) cd cs
  ($!!) cont (Choice_C_Char d i x y) cd cs = nfChoice cont d i x y cd cs
  ($!!) cont (Choices_C_Char d i xs) cd cs = nfChoices cont d i xs cd cs
  ($!!) cont (Guard_C_Char d c x) cd cs = guardCons d c ((cont $!! x) cd $! (addCs c cs))
  ($!!) _ (Fail_C_Char d info) _ _ = failCons d info
  ($##) cont x@(C_Char _) cd cs = cont x cd cs
  ($##) cont (CurryChar x) cd cs = ((cont . CurryChar) $## x) cd cs
  ($##) cont (Choice_C_Char d i x y) cd cs = gnfChoice cont d i x y cd cs
  ($##) cont (Choices_C_Char d i xs) cd cs = gnfChoices cont d i xs cd cs
  ($##) cont (Guard_C_Char d c x) cd cs = guardCons d c ((cont $## x) cd $! (addCs c cs))
  ($##) _ (Fail_C_Char d info) _ _ = failCons d info
  searchNF search cont c@(C_Char _) = cont c
  searchNF search cont (CurryChar x) = search (cont . CurryChar) x
  searchNF _ _ x = error ("Prelude.Char.searchNF: no constructor: " ++ (show x))

instance Unifiable C_Char where
  (=.=) (C_Char       x1) (C_Char      x2) cd _ | isTrue# (x1 `eqChar#` x2) = C_True
                                                | otherwise                 = Fail_C_Bool cd defFailInfo
  (=.=) (C_Char       x1) (CurryChar x2)   cd cs = (primChar2CurryChar x1 =:= x2) cd cs
  (=.=) (CurryChar  x1) (C_Char      x2)   cd cs = (x1 =:= primChar2CurryChar x2) cd cs
  (=.=) (CurryChar x1)    (CurryChar   x2) cd cs = (x1 =:= x2) cd cs
  (=.=) _                 _                cd _  = Fail_C_Bool cd  defFailInfo
  (=.<=) (C_Char       x1) (C_Char      x2) cd _ | isTrue# (x1 `eqChar#` x2) = C_True
                                                 | otherwise                 = Fail_C_Bool cd defFailInfo
  (=.<=) (C_Char       x1) (CurryChar x2)   cd cs = (primChar2CurryChar x1 =:<= x2) cd cs
  (=.<=) (CurryChar  x1) (C_Char      x2)   cd cs = (x1 =:<= primChar2CurryChar x2) cd cs
  (=.<=) (CurryChar x1)    (CurryChar   x2) cd cs = (x1 =:<= x2) cd cs
  (=.<=) _                 _                cd _  = Fail_C_Bool cd defFailInfo
  bind cd i (C_Char    x) = (i :=: ChooseN 0 1) : bind cd (leftID i) (primChar2CurryChar x)
  bind cd i (CurryChar x) = (i :=: ChooseN 0 1) : bind cd (leftID i) x
  bind cd i (Choice_C_Char d j l r) = [(ConstraintChoice d j (bind cd i l) (bind cd i r))]
  bind cd i (Choices_C_Char d j@(FreeID _ _) xs) = bindOrNarrow cd i d j xs
  bind cd i (Choices_C_Char d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (bind cd i) xs))]
  bind _  _ c@(Choices_C_Char _ i _) = error ("Prelude.Char.bind: Choices with ChoiceID: " ++ (show c))
  bind _  _ (Fail_C_Char _ info) = [Unsolvable info]
  bind cd  i (Guard_C_Char _ cs e) = getConstrList cs ++ (bind cd i e)
  lazyBind cd i (C_Char    x) = [i :=: ChooseN 0 1, leftID i :=: LazyBind (lazyBind cd (leftID i) (primChar2CurryChar x))]
  lazyBind cd i (CurryChar x) = [i :=: ChooseN 0 1, leftID i :=: LazyBind (lazyBind cd (leftID i) x)]
  lazyBind cd i (Choice_C_Char d j l r) = [(ConstraintChoice d j (lazyBind cd i l) (lazyBind cd i r))]
  lazyBind cd i (Choices_C_Char d j@(FreeID _ _) xs) = lazyBindOrNarrow cd i d j xs
  lazyBind cd i (Choices_C_Char d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (lazyBind cd i) xs))]
  lazyBind _  _ c@(Choices_C_Char _ i _) = error ("Prelude.Char.lazyBind: Choices with ChoiceID: " ++ (show c))
  lazyBind _  _ (Fail_C_Char _ info) = [Unsolvable info]
  lazyBind cd i (Guard_C_Char _ cs e) = getConstrList cs ++ [(i :=: (LazyBind (lazyBind cd i e)))]

instance Curry C_Char
-- END GENERATED FROM PrimTypes.curry

d_C_prim_eqChar :: C_Char -> C_Char -> Cover -> ConstStore -> C_Bool
d_C_prim_eqChar (Choice_C_Char d i x y) z cd cs =
  narrow d i ((x `d_C_prim_eqChar` z) cd cs) ((y `d_C_prim_eqChar` z) cd cs)
d_C_prim_eqChar (Choices_C_Char d i xs) y cd cs =
  narrows cs d i (\x -> (x `d_C_prim_eqChar` y) cd cs) xs
d_C_prim_eqChar (Guard_C_Char d c x) y cd cs =
  guardCons d c ((x `d_C_prim_eqChar` y) cd $! (addCs c cs))
d_C_prim_eqChar (Fail_C_Char d info) _ _ _ = failCons d info
d_C_prim_eqChar z (Choice_C_Char d i x y) cd cs =
  narrow d i ((z `d_C_prim_eqChar` x) cd cs) ((z `d_C_prim_eqChar` y) cd cs)
d_C_prim_eqChar y (Choices_C_Char d i xs) cd cs =
  narrows cs d i (\x -> (y `d_C_prim_eqChar` x) cd cs) xs
d_C_prim_eqChar y (Guard_C_Char d c x) cd cs =
  guardCons d c ((y `d_C_prim_eqChar` x) cd $! (addCs c cs))
d_C_prim_eqChar _ (Fail_C_Char d info) _ _ = failCons d info
d_C_prim_eqChar (C_Char x1) (C_Char y1) _ _ = toCurry (isTrue# (x1 `eqChar#` y1))
d_C_prim_eqChar (C_Char      x1) (CurryChar y1) cd cs =
  ((primChar2CurryChar x1) `d_C_prim_eqBinInt` y1) cd cs
d_C_prim_eqChar (CurryChar x1) (C_Char      y1) cd cs =
  (x1 `d_C_prim_eqBinInt` (primChar2CurryChar y1)) cd cs
d_C_prim_eqChar (CurryChar x1) (CurryChar y1) cd cs =
  (x1 `d_C_prim_eqBinInt` y1) cd cs

d_C_prim_ltEqChar :: C_Char -> C_Char -> Cover -> ConstStore -> C_Bool
d_C_prim_ltEqChar (Choice_C_Char d i x y) z cd cs =
  narrow d i ((x `d_C_prim_ltEqChar` z) cd cs) ((y `d_C_prim_ltEqChar` z) cd cs)
d_C_prim_ltEqChar (Choices_C_Char d i xs) y cd cs =
  narrows cs d i (\x -> (x `d_C_prim_ltEqChar` y) cd cs) xs
d_C_prim_ltEqChar (Guard_C_Char d c x) y cd cs =
  guardCons d c ((x `d_C_prim_ltEqChar` y) cd $! (addCs c cs))
d_C_prim_ltEqChar (Fail_C_Char d info) _ _ _ = failCons d info
d_C_prim_ltEqChar z (Choice_C_Char d i x y) cd cs =
  narrow d i ((z `d_C_prim_ltEqChar` x) cd cs) ((z `d_C_prim_ltEqChar` y) cd cs)
d_C_prim_ltEqChar y (Choices_C_Char d i xs) cd cs =
  narrows cs d i (\x -> (y `d_C_prim_ltEqChar` x) cd cs) xs
d_C_prim_ltEqChar y (Guard_C_Char d c x) cd cs =
  guardCons d c ((y `d_C_prim_ltEqChar` x) cd $! (addCs c cs))
d_C_prim_ltEqChar _ (Fail_C_Char d info) _ _ = failCons d info
d_C_prim_ltEqChar (C_Char x1) (C_Char y1) _ _ = toCurry (isTrue# (x1 `leChar#` y1))
d_C_prim_ltEqChar (C_Char      x1) (CurryChar y1) cd cs = ((primChar2CurryChar x1) `d_C_lteqInteger` y1) cd cs
d_C_prim_ltEqChar (CurryChar x1) (C_Char      y1) cd cs = (x1 `d_C_lteqInteger` (primChar2CurryChar y1)) cd cs
d_C_prim_ltEqChar (CurryChar x1) (CurryChar y1) cd cs = (x1 `d_C_lteqInteger` y1) cd cs

external_d_C_eqChar :: C_Char -> C_Char -> Cover -> ConstStore -> C_Bool
external_d_C_eqChar  = d_C_prim_eqChar

external_d_C_ltEqChar :: C_Char -> C_Char -> Cover -> ConstStore -> C_Bool
external_d_C_ltEqChar  = d_C_prim_ltEqChar

primChar2primint :: Char# -> Integer
primChar2primint c = toInteger (ord (C# c))

primint2primChar :: Integer -> Char#
primint2primChar c = char2primChar (chr (fromInteger c))
  where char2primChar (C# c) = c

primChar2CurryChar :: Char# -> BinInt
primChar2CurryChar c = primint2curryint (primChar2primint c)

curryChar2primChar :: BinInt -> Char#
curryChar2primChar c = primint2primChar (curryint2primint c)

-- ---------------------------------------------------------------------------
-- Conversion from and to primitive Haskell types
-- ---------------------------------------------------------------------------

instance ConvertCurryHaskell C_Int Integer where
  toCurry i = C_Int i

  fromCurry (C_Int i)      = i
  fromCurry (C_CurryInt i) = curryint2primint i
  fromCurry _              = error "KiCS2 error: Int data with no ground term"

instance ConvertCurryHaskell C_Int Int where
  toCurry i = toCurry (toInteger i)

  fromCurry i = fromInteger (fromCurry i)

instance ConvertCurryHaskell C_Float Double where
  toCurry (D# d) = C_Float d

  fromCurry (C_Float d) = D# d
  fromCurry _           = error "KiCS2 error: Float data with no ground term"

instance ConvertCurryHaskell C_Char Char where
  toCurry (C# c) = C_Char c

  fromCurry (C_Char    c) = C# c
  fromCurry (CurryChar c) = C# (curryChar2primChar c)
  fromCurry _             = error "KiCS2 error: Char data with no ground term"

instance (ConvertCurryHaskell ct ht) =>
         ConvertCurryHaskell (OP_List ct) [ht] where
  toCurry []     = OP_List
  toCurry (c:cs) = OP_Cons (toCurry c) (toCurry cs)

  fromCurry OP_List        = []
  fromCurry (OP_Cons c cs) = fromCurry c : fromCurry cs
  fromCurry _              = error "KiCS2 error: List data with no ground term"

instance ConvertCurryHaskell C_Bool Bool where
  toCurry True  = C_True
  toCurry False = C_False

  fromCurry C_True  = True
  fromCurry C_False = False
  fromCurry _       = error "KiCS2 error: Bool data with no ground term"

instance ConvertCurryHaskell OP_Unit () where
  toCurry ()  = OP_Unit

  fromCurry OP_Unit = ()
  fromCurry _       = error "KiCS2 error: Unit data with no ground term"

instance (ConvertCurryHaskell ct1 ht1, ConvertCurryHaskell ct2 ht2) =>
         ConvertCurryHaskell (OP_Tuple2 ct1 ct2) (ht1,ht2) where
  toCurry (x1,x2)  = OP_Tuple2 (toCurry x1) (toCurry x2)

  fromCurry (OP_Tuple2 x1 x2) = (fromCurry x1, fromCurry x2)
  fromCurry _       = error "KiCS2 error: Pair data with no ground term"

instance (ConvertCurryHaskell ct1 ht1, ConvertCurryHaskell ct2 ht2,
          ConvertCurryHaskell ct3 ht3) =>
         ConvertCurryHaskell (OP_Tuple3 ct1 ct2 ct3) (ht1,ht2,ht3) where
  toCurry (x1,x2,x3)  = OP_Tuple3 (toCurry x1) (toCurry x2) (toCurry x3)

  fromCurry (OP_Tuple3 x1 x2 x3) = (fromCurry x1, fromCurry x2, fromCurry x3)
  fromCurry _       = error "KiCS2 error: Tuple3 data with no ground term occurred"

instance ConvertCurryHaskell ct ht =>
         ConvertCurryHaskell (C_Maybe ct) (Maybe ht) where
  toCurry Nothing  = C_Nothing
  toCurry (Just x) = C_Just (toCurry x)

  fromCurry C_Nothing  = Nothing
  fromCurry (C_Just x) = Just (fromCurry x)
  fromCurry _          = error "KiCS2 error: Maybe data with no ground term occurred"

toCurryString :: String -> OP_List C_Char
toCurryString = toCurry

-- -----------------------------------------------------------------------------
-- Auxiliary operations for showing lists
-- -----------------------------------------------------------------------------

showsPrec4CurryList :: Curry a => Int -> OP_List a -> ShowS
showsPrec4CurryList d cl =
  if isStandardCurryList cl
  then showsPrec d (clist2hlist cl)
  else showChar '(' . showsPrecRaw d cl . showChar ')'
 where
  isStandardCurryList OP_List = True
  isStandardCurryList (OP_Cons _ xs) = isStandardCurryList xs
  isStandardCurryList _ = False

  clist2hlist OP_List        = []
  clist2hlist (OP_Cons x xs) = x : clist2hlist xs

  showsPrecRaw d (Choice_OP_List cd i x y) = showsChoice d cd i x y
  showsPrecRaw d (Choices_OP_List cd i xs) = showsChoices d cd i xs
  showsPrecRaw d (Guard_OP_List    cd c e) = showsGuard d cd c e
  showsPrecRaw d (Fail_OP_List        _ _) = showChar '!'
  showsPrecRaw d OP_List                   = showString "[]"
  showsPrecRaw d (OP_Cons            x xs) = showParen (d > 5)
    (showsPrec 6 x . showChar ':' . showsPrecRaw 5 xs)

-- -----------------------------------------------------------------------------
-- Primitive operations: General
-- -----------------------------------------------------------------------------

external_d_C_prim_show :: Show a => a -> Cover -> ConstStore -> C_String
external_d_C_prim_show a _ _ = toCurry (show a)

external_d_C_prim_showCharLiteral :: C_Char -> Cover -> ConstStore -> C_String
external_d_C_prim_showCharLiteral a _ _ = toCurry (show a)

external_d_C_prim_showIntLiteral :: C_Int -> Cover -> ConstStore -> C_String
external_d_C_prim_showIntLiteral a _ _ = toCurry (show a)

external_d_C_prim_showFloatLiteral :: C_Float -> Cover -> ConstStore -> C_String
external_d_C_prim_showFloatLiteral a _ _ = toCurry (show a)

external_d_C_prim_showStringLiteral :: C_String -> Cover -> ConstStore -> C_String
external_d_C_prim_showStringLiteral a _ _ = toCurry (show a)

external_d_C_prim_readNatLiteral :: C_String -> Cover -> ConstStore -> OP_List (OP_Tuple2 C_Int C_String)
external_d_C_prim_readNatLiteral s _ _ = toCurry (reads (fromCurry s) :: [(Integer, String)])

external_d_C_prim_readFloatLiteral :: C_String -> Cover -> ConstStore -> OP_List (OP_Tuple2 C_Float C_String)
external_d_C_prim_readFloatLiteral s _ _ = toCurry (reads (fromCurry s) :: [(Double, String)])

external_d_C_prim_readCharLiteral :: C_String -> Cover -> ConstStore -> OP_List (OP_Tuple2 C_Char C_String)
external_d_C_prim_readCharLiteral s _ _ = toCurry (reads (fromCurry s) :: [(Char, String)])

external_d_C_prim_readStringLiteral :: C_String -> Cover -> ConstStore -> OP_List (OP_Tuple2 C_String C_String)
external_d_C_prim_readStringLiteral s _ _ = toCurry (reads (fromCurry s) :: [(String, String)])

external_d_OP_eq_colon_eq :: Unifiable a => a -> a -> Cover -> ConstStore -> C_Bool
external_d_OP_eq_colon_eq = (=:=)

external_nd_OP_eq_colon_eq :: (NonDet a, Unifiable a) => a -> a -> IDSupply -> Cover -> ConstStore -> C_Bool
external_nd_OP_eq_colon_eq x1 x2 _ = x1 =:= x2

external_d_OP_eq_colon_lt_eq :: Curry a => a -> a -> Cover -> ConstStore -> C_Bool
external_d_OP_eq_colon_lt_eq = (=:<=)

external_nd_OP_eq_colon_lt_eq :: (NonDet a, Curry a) => a -> a -> IDSupply -> Cover -> ConstStore -> C_Bool
external_nd_OP_eq_colon_lt_eq x1 x2 _ = x1 =:<= x2

external_d_C_failed :: NonDet a => Cover -> ConstStore -> a
external_d_C_failed cd _ = failCons cd (customFail "Call to function `failed'")

external_d_C_cond :: Curry a => C_Bool -> a -> Cover -> ConstStore -> a
external_d_C_cond succ a cd cs = ((\_ _ _ -> a) `d_OP_dollar_hash` succ) cd cs

external_d_OP_amp :: C_Bool -> C_Bool -> Cover -> ConstStore -> C_Bool
external_d_OP_amp = (&)

external_d_C_ensureNotFree :: Curry a => a -> Cover -> ConstStore -> a
external_d_C_ensureNotFree x cd cs = case try x of
  Choice   d i a b  -> choiceCons d i (external_d_C_ensureNotFree a cd cs)
                                      (external_d_C_ensureNotFree b cd cs)
  Narrowed d  i xs -> choicesCons d i
                      (map (\x -> external_d_C_ensureNotFree x cd cs) xs)
  Free     d  i xs -> narrows cs d i
                      (\x -> external_d_C_ensureNotFree x cd cs) xs
  Guard    d   c e -> guardCons d c
                      (external_d_C_ensureNotFree e cd $! (addCs c cs))
  _                -> x

external_d_OP_dollar_bang :: (NonDet a, NonDet b)
  => (a -> Cover -> ConstStore -> b) -> a -> Cover -> ConstStore -> b
external_d_OP_dollar_bang = d_dollar_bang

external_nd_OP_dollar_bang :: (NonDet a, NonDet b)
  => (Func a b) -> a -> IDSupply -> Cover -> ConstStore -> b
external_nd_OP_dollar_bang = nd_dollar_bang

external_d_OP_dollar_bang_bang :: (NormalForm a, NonDet b)
  => (a -> Cover -> ConstStore -> b) -> a -> Cover -> ConstStore -> b
external_d_OP_dollar_bang_bang = ($!!)

external_nd_OP_dollar_bang_bang :: (NormalForm a, NonDet b)
  => Func a b -> a -> IDSupply -> Cover -> ConstStore -> b
external_nd_OP_dollar_bang_bang f x s cd cs
  = ((\y cd1 cs1-> nd_apply f y s cd1 cs1) $!! x) cd cs

external_d_OP_dollar_hash_hash :: (NormalForm a, NonDet b)
  => (a -> Cover -> ConstStore -> b) -> a -> Cover -> ConstStore -> b
external_d_OP_dollar_hash_hash = ($##)

external_nd_OP_dollar_hash_hash :: (NormalForm a, NonDet b)
  => Func a b -> a -> IDSupply -> Cover -> ConstStore -> b
external_nd_OP_dollar_hash_hash f x s cd cs
  = ((\y cd1 cs1 -> nd_apply f y s cd1 cs1) $## x) cd cs

external_d_C_apply :: (a -> Cover -> ConstStore -> b) -> a
  -> Cover -> ConstStore -> b
external_d_C_apply = d_apply

external_nd_C_apply :: NonDet b => Func a b -> a
  -> IDSupply -> Cover -> ConstStore -> b
external_nd_C_apply = nd_apply

-- -----------------------------------------------------------------------------
-- Primitive operations: Characters
-- -----------------------------------------------------------------------------

external_d_C_prim_ord :: C_Char -> Cover -> ConstStore -> C_Int
external_d_C_prim_ord (C_Char c)    _ _ = C_Int (primChar2primint c)
external_d_C_prim_ord (CurryChar c) _ _ = C_CurryInt c

external_d_C_prim_chr :: C_Int -> Cover -> ConstStore -> C_Char
external_d_C_prim_chr (C_Int i)      _ _ = C_Char (primint2primChar i)
external_d_C_prim_chr (C_CurryInt i) _ _ = CurryChar i

-- -----------------------------------------------------------------------------
-- Primitive operations: Arithmetics
-- -----------------------------------------------------------------------------

external_d_C_plusInt :: C_Int -> C_Int -> Cover -> ConstStore -> C_Int
external_d_C_plusInt (C_Int      x) (C_Int      y) _  _  = C_Int (x + y)
external_d_C_plusInt (C_Int      x) (C_CurryInt y) cd cs
  = C_CurryInt (((primint2curryint x) `d_OP_plus_hash` y) cd cs)
external_d_C_plusInt (C_CurryInt x) (C_Int      y) cd cs
  = C_CurryInt ((x `d_OP_plus_hash` (primint2curryint y)) cd cs)
external_d_C_plusInt (C_CurryInt x) (C_CurryInt y) cd cs
  = C_CurryInt ((x `d_OP_plus_hash` y) cd cs)
external_d_C_plusInt x              y              cd cs
  = ((\a cd1 cs1 -> ((\b cd2 cs2 -> ((a `external_d_C_plusInt` b) cd2 cs2))
    `d_OP_dollar_hash` y) cd1 cs1) `d_OP_dollar_hash` x) cd cs

external_d_C_minusInt :: C_Int -> C_Int -> Cover -> ConstStore -> C_Int
external_d_C_minusInt (C_Int      x) (C_Int      y) _  _  = C_Int (x - y)
external_d_C_minusInt (C_Int      x) (C_CurryInt y) cd cs
  = C_CurryInt (((primint2curryint x) `d_OP_minus_hash` y) cd cs)
external_d_C_minusInt (C_CurryInt x) (C_Int y)      cd cs
  = C_CurryInt ((x `d_OP_minus_hash` (primint2curryint y)) cd cs)
external_d_C_minusInt (C_CurryInt x) (C_CurryInt y) cd cs
  = C_CurryInt ((x `d_OP_minus_hash` y) cd cs)
external_d_C_minusInt x              y              cd cs
  = ((\a cd1 cs1 -> ((\b cd2 cs2 -> ((a `external_d_C_minusInt` b) cd2 cs2 ))
    `d_OP_dollar_hash` y) cd1 cs1) `d_OP_dollar_hash` x) cd cs

external_d_C_timesInt :: C_Int -> C_Int -> Cover -> ConstStore -> C_Int
external_d_C_timesInt (C_Int      x) (C_Int      y) _  _  = C_Int (x * y)
external_d_C_timesInt (C_Int      x) (C_CurryInt y) cd cs
  = C_CurryInt (((primint2curryint x) `d_OP_star_hash` y) cd cs)
external_d_C_timesInt (C_CurryInt x) (C_Int      y) cd cs
  = C_CurryInt ((x `d_OP_star_hash` (primint2curryint y)) cd cs)
external_d_C_timesInt (C_CurryInt x) (C_CurryInt y) cd cs
  = C_CurryInt ((x `d_OP_star_hash` y) cd cs)
external_d_C_timesInt x              y              cd cs
  = ((\a cd1 cs1 -> ((\b cd2 cs2 -> ((a `external_d_C_timesInt` b) cd2 cs2))
    `d_OP_dollar_hash` y) cd1 cs1) `d_OP_dollar_hash` x) cd cs

external_d_C_quotInt :: C_Int -> C_Int -> Cover -> ConstStore -> C_Int
external_d_C_quotInt (C_Int      x) (C_Int      y) cd _
  | y == 0    = Fail_C_Int cd (customFail "Division by Zero")
  | otherwise = C_Int (x `quot` y)
external_d_C_quotInt (C_Int      x) (C_CurryInt y) cd cs
  = C_CurryInt (((primint2curryint x) `d_C_quotInteger` y) cd cs)
external_d_C_quotInt (C_CurryInt x) (C_Int      y) cd cs
  = C_CurryInt ((x `d_C_quotInteger` (primint2curryint y)) cd cs)
external_d_C_quotInt (C_CurryInt x) (C_CurryInt y) cd cs
  = C_CurryInt ((x `d_C_quotInteger` y) cd cs)
external_d_C_quotInt x              y              cd cs
  = ((\a cd1 cs1 -> ((\b cd2 cs2 -> ((a `external_d_C_quotInt` b) cd2 cs2 ))
    `d_OP_dollar_hash` y) cd1 cs1) `d_OP_dollar_hash` x) cd cs

external_d_C_remInt :: C_Int -> C_Int -> Cover -> ConstStore -> C_Int
external_d_C_remInt (C_Int      x) (C_Int      y) cd _
  | y == 0    = Fail_C_Int cd (customFail "Division by Zero")
  | otherwise = C_Int (x `rem` y)
external_d_C_remInt (C_Int      x) (C_CurryInt y) cd cs
  = C_CurryInt (((primint2curryint x) `d_C_remInteger` y) cd cs)
external_d_C_remInt (C_CurryInt x) (C_Int      y) cd cs
  = C_CurryInt ((x `d_C_remInteger` (primint2curryint y)) cd cs)
external_d_C_remInt (C_CurryInt x) (C_CurryInt y) cd cs
  = C_CurryInt ((x `d_C_remInteger` y) cd cs)
external_d_C_remInt x              y              cd cs
  = ((\a cd1 cs1 -> ((\b cd2 cs2 -> ((a `external_d_C_remInt` b) cd2 cs2))
    `d_OP_dollar_hash` y) cd1 cs1) `d_OP_dollar_hash` x) cd cs

external_d_C_quotRem_ :: C_Int -> C_Int
  -> Cover -> ConstStore -> OP_Tuple2 C_Int C_Int
external_d_C_quotRem_ (C_Int      x) (C_Int      y) cd _
  | y == 0    = Fail_OP_Tuple2 cd (customFail "Division by Zero")
  | otherwise = OP_Tuple2 (C_Int (x `quot` y)) (C_Int (x `rem` y))
external_d_C_quotRem_ (C_Int      x) (C_CurryInt y) cd cs
  = (mkIntTuple `d_dollar_bang` (((primint2curryint x) `d_C_quotRemInteger` y) cd cs)) cd cs
external_d_C_quotRem_ (C_CurryInt x) (C_Int      y) cd cs
  = (mkIntTuple `d_dollar_bang` ((x `d_C_quotRemInteger` (primint2curryint y)) cd cs)) cd cs
external_d_C_quotRem_ (C_CurryInt x) (C_CurryInt y) cd cs
  = (mkIntTuple `d_dollar_bang` ((x `d_C_quotRemInteger` y) cd cs)) cd cs
external_d_C_quotRem_ x              y              cd cs
  = ((\a cd1 cs1 -> ((\b cd2 cs2 -> ((a `external_d_C_quotRem_` b) cd2 cs2))
    `d_OP_dollar_hash` y) cd1 cs1) `d_OP_dollar_hash` x) cd cs

external_d_C_divInt :: C_Int -> C_Int -> Cover -> ConstStore -> C_Int
external_d_C_divInt (C_Int      x) (C_Int      y) cd _
  | y == 0    = Fail_C_Int cd (customFail "Division by Zero")
  | otherwise = C_Int (x `div` y)
external_d_C_divInt (C_Int      x) (C_CurryInt y) cd cs
  = C_CurryInt (((primint2curryint x) `d_C_divInteger` y) cd cs)
external_d_C_divInt (C_CurryInt x) (C_Int      y) cd cs
  = C_CurryInt ((x `d_C_divInteger` (primint2curryint y)) cd cs)
external_d_C_divInt (C_CurryInt x) (C_CurryInt y) cd cs
  = C_CurryInt ((x `d_C_divInteger` y) cd cs)
external_d_C_divInt x              y              cd cs
  = ((\a cd1 cs1-> ((\b cd2 cs2-> ((a `external_d_C_divInt` b) cd2 cs2))
    `d_OP_dollar_hash` y) cd1 cs1) `d_OP_dollar_hash` x) cd cs

external_d_C_modInt :: C_Int -> C_Int -> Cover -> ConstStore -> C_Int
external_d_C_modInt (C_Int      x) (C_Int      y) cd _
  | y == 0    = Fail_C_Int cd (customFail "Division by Zero")
  | otherwise = C_Int (x `mod` y)
external_d_C_modInt (C_Int      x) (C_CurryInt y) cd cs
  = C_CurryInt (((primint2curryint x) `d_C_modInteger` y) cd cs)
external_d_C_modInt (C_CurryInt x) (C_Int      y) cd cs
  = C_CurryInt ((x `d_C_modInteger` (primint2curryint y)) cd cs)
external_d_C_modInt (C_CurryInt x) (C_CurryInt y) cd cs
  = C_CurryInt ((x `d_C_modInteger` y) cd cs)
external_d_C_modInt x              y              cd cs
  = ((\a cd1 cs1 -> ((\b cd2 cs2 -> ((a `external_d_C_modInt` b)) cd2 cs2)
    `d_OP_dollar_hash` y) cd1 cs1) `d_OP_dollar_hash` x) cd cs

external_d_C_divMod_ :: C_Int -> C_Int ->  Cover -> ConstStore -> OP_Tuple2 C_Int C_Int
external_d_C_divMod_ (C_Int      x) (C_Int      y) cd _
  | y == 0    = Fail_OP_Tuple2 cd (customFail "Division by Zero")
  | otherwise = OP_Tuple2 (C_Int (x `div` y)) (C_Int (x `mod` y))
external_d_C_divMod_ (C_Int      x) (C_CurryInt y) cd cs
  = (mkIntTuple `d_OP_dollar_hash` (((primint2curryint x) `d_C_divModInteger` y) cd cs)) cd cs
external_d_C_divMod_ (C_CurryInt x) (C_Int      y) cd cs
  = (mkIntTuple `d_OP_dollar_hash` ((x `d_C_divModInteger` (primint2curryint y)) cd cs)) cd cs
external_d_C_divMod_ (C_CurryInt x) (C_CurryInt y) cd cs
  = (mkIntTuple `d_OP_dollar_hash` ((x `d_C_divModInteger` y) cd cs)) cd cs
external_d_C_divMod_ x              y              cd cs
  = ((\a cd1 cs1 -> ((\b cd2 cs2 -> ((a `external_d_C_divMod_` b) cd2 cs2 ))
    `d_OP_dollar_hash` y) cd1 cs1) `d_OP_dollar_hash` x) cd cs

mkIntTuple :: OP_Tuple2 BinInt BinInt -> Cover -> ConstStore -> OP_Tuple2 C_Int C_Int
mkIntTuple (OP_Tuple2 d m) _ _ = OP_Tuple2 (C_CurryInt d) (C_CurryInt m)

external_d_C_negateFloat :: C_Float -> Cover -> ConstStore -> C_Float
external_d_C_negateFloat (C_Float x) _  _  = C_Float (negateDouble# x)
external_d_C_negateFloat x           cd cs
  = (external_d_C_negateFloat `d_OP_dollar_hash` x) cd cs

external_d_C_prim_truncateFloat :: C_Float -> Cover -> ConstStore -> C_Int
external_d_C_prim_truncateFloat (C_Float x) _  _  = C_Int (truncate (D# x))
external_d_C_prim_truncateFloat x           cd cs
  = (external_d_C_prim_truncateFloat `d_OP_dollar_hash` x) cd cs

external_d_C_prim_roundFloat :: C_Float -> Cover -> ConstStore -> C_Int
external_d_C_prim_roundFloat (C_Float x) _  _  = C_Int (round (D# x))
external_d_C_prim_roundFloat x           cd cs
  = (external_d_C_prim_roundFloat `d_OP_dollar_hash` x) cd cs

external_d_C_prim_logFloat :: C_Float -> Cover -> ConstStore -> C_Float
external_d_C_prim_logFloat x _ _ =
  toCurry ((log (fromCurry x)) :: Double)

external_d_C_prim_expFloat :: C_Float -> Cover -> ConstStore -> C_Float
external_d_C_prim_expFloat x _ _ =
  toCurry ((exp (fromCurry x)) :: Double)

external_d_C_prim_sqrtFloat :: C_Float -> Cover -> ConstStore -> C_Float
external_d_C_prim_sqrtFloat x _ _ =
  toCurry ((sqrt (fromCurry x)) :: Double)

external_d_C_prim_cosFloat :: C_Float -> Cover -> ConstStore -> C_Float
external_d_C_prim_cosFloat x _ _ =
  toCurry ((cos (fromCurry x)) :: Double)

external_d_C_prim_sinFloat :: C_Float -> Cover -> ConstStore -> C_Float
external_d_C_prim_sinFloat x _ _ =
  toCurry ((sin (fromCurry x)) :: Double)

external_d_C_prim_tanFloat :: C_Float -> Cover -> ConstStore -> C_Float
external_d_C_prim_tanFloat x _ _ =
  toCurry ((tan (fromCurry x)) :: Double)

external_d_C_prim_asinFloat :: C_Float -> Cover -> ConstStore -> C_Float
external_d_C_prim_asinFloat x _ _ =
  toCurry ((asin (fromCurry x)) :: Double)

external_d_C_prim_acosFloat :: C_Float -> Cover -> ConstStore -> C_Float
external_d_C_prim_acosFloat x _ _ =
  toCurry ((acos (fromCurry x)) :: Double)

external_d_C_prim_atanFloat :: C_Float -> Cover -> ConstStore -> C_Float
external_d_C_prim_atanFloat x _ _ =
  toCurry ((atan (fromCurry x)) :: Double)

external_d_C_prim_sinhFloat :: C_Float -> Cover -> ConstStore -> C_Float
external_d_C_prim_sinhFloat x _ _ =
  toCurry ((sinh (fromCurry x)) :: Double)

external_d_C_prim_coshFloat :: C_Float -> Cover -> ConstStore -> C_Float
external_d_C_prim_coshFloat x _ _ =
  toCurry ((cosh (fromCurry x)) :: Double)

external_d_C_prim_tanhFloat :: C_Float -> Cover -> ConstStore -> C_Float
external_d_C_prim_tanhFloat x _ _ =
  toCurry ((tanh (fromCurry x)) :: Double)

external_d_C_prim_asinhFloat :: C_Float -> Cover -> ConstStore -> C_Float
external_d_C_prim_asinhFloat x _ _ =
  toCurry ((asinh (fromCurry x)) :: Double)

external_d_C_prim_acoshFloat :: C_Float -> Cover -> ConstStore -> C_Float
external_d_C_prim_acoshFloat x _ _ =
  toCurry ((acosh (fromCurry x)) :: Double)

external_d_C_prim_atanhFloat :: C_Float -> Cover -> ConstStore -> C_Float
external_d_C_prim_atanhFloat x _ _ =
  toCurry ((atanh (fromCurry x)) :: Double)

external_d_C_prim_plusFloat :: C_Float -> C_Float -> Cover -> ConstStore -> C_Float
external_d_C_prim_plusFloat y x _ _ =
  toCurry ((fromCurry x + fromCurry y) :: Double)

external_d_C_prim_minusFloat :: C_Float -> C_Float -> Cover -> ConstStore -> C_Float
external_d_C_prim_minusFloat y x _ _ =
  toCurry ((fromCurry x - fromCurry y) :: Double)

external_d_C_prim_timesFloat :: C_Float -> C_Float -> Cover -> ConstStore -> C_Float
external_d_C_prim_timesFloat y x _ _ =
  toCurry ((fromCurry x * fromCurry y) :: Double)

external_d_C_prim_divFloat :: C_Float -> C_Float -> Cover -> ConstStore -> C_Float
external_d_C_prim_divFloat y x _ _ =
  toCurry ((fromCurry x / fromCurry y) :: Double)

external_d_C_prim_intToFloat :: C_Int -> Cover -> ConstStore -> C_Float
external_d_C_prim_intToFloat x _ _ = toCurry (fromInteger (fromCurry x) :: Double)

-- -----------------------------------------------------------------------------
-- Primitive operations: IO stuff
-- -----------------------------------------------------------------------------

external_d_C_returnIO :: a -> Cover -> ConstStore -> C_IO a
external_d_C_returnIO a _ _ = fromIO (return a)

external_d_C_prim_putChar :: C_Char -> Cover -> ConstStore -> C_IO OP_Unit
external_d_C_prim_putChar c _ _ = toCurry putChar c

external_d_C_getChar :: Cover -> ConstStore -> C_IO C_Char
external_d_C_getChar _ _ = toCurry getChar

external_d_C_prim_readFile :: C_String -> Cover -> ConstStore -> C_IO C_String
external_d_C_prim_readFile s _ _ = toCurry readFile s

-- TODO: Problem: s is not evaluated to enable lazy IO and therefore could
-- be non-deterministic
external_d_C_prim_writeFile :: C_String -> C_String
  -> Cover -> ConstStore -> C_IO OP_Unit
external_d_C_prim_writeFile s1 s2 _ _ = toCurry writeFile s1 s2

-- TODO: Problem: s is not evaluated to enable lazy IO and therefore could
-- be non-deterministic
external_d_C_prim_appendFile :: C_String -> C_String
  -> Cover -> ConstStore -> C_IO OP_Unit
external_d_C_prim_appendFile s1 s2 _ _ = toCurry appendFile s1 s2

external_d_C_bindIO :: (Curry t0, Curry t1)
  => C_IO t0 -> (t0 -> Cover -> ConstStore -> C_IO t1)
  -> Cover -> ConstStore -> C_IO t1
external_d_C_bindIO m f cd cs = C_IO $ do
  res <- searchIO errSupply cd cs m
  case res of
    Left err -> return (Left (traceFail ("Prelude.>>=") [show m, show f] err))
    Right x  -> do
      cs1 <- lookupGlobalCs
      let cs2 = combineCs cs cs1
      searchIO errSupply cd cs2 (f x cd cs2)
  where errSupply = internalError "Prelude.(>>=): ID supply used"

-- TODO: Investigate if `cs` and `cs'` are in a subset relation
-- in either direction.
external_nd_C_bindIO :: (Curry t0, Curry t1)
  => C_IO t0 -> Func t0 (C_IO t1)
  -> IDSupply -> Cover -> ConstStore -> C_IO t1
external_nd_C_bindIO m f _ _ cs = HO_C_IO $ \s cd cs' -> do
  let cs1 = combineCs cs' cs
  res <- searchIO (leftSupply s) cd cs1 m
  case res of
    Left err -> return (Left (traceFail ("Prelude.>>=") [show m, show f] err))
    Right x  -> do
      cs2 <- lookupGlobalCs
      let cs3 = combineCs cs1 cs2
          s'  = rightSupply s
      searchIO (leftSupply s') cd cs3 (nd_apply f x (rightSupply s') cd cs3)

-- -----------------------------------------------------------------------------
-- Primitive operations: Exception handling
-- -----------------------------------------------------------------------------

instance ConvertCurryHaskell C_IOError CurryException where
  toCurry (IOException     s) = C_IOError     (toCurry s)
  toCurry (UserException   s) = C_UserError   (toCurry s)
  toCurry (FailException   s) = C_FailError   (toCurry s)
  toCurry (NondetException s) = C_NondetError (toCurry s)

  fromCurry (C_IOError     s) = IOException     $ fromCurry s
  fromCurry (C_UserError   s) = UserException   $ fromCurry s
  fromCurry (C_FailError   s) = FailException   $ fromCurry s
  fromCurry (C_NondetError s) = NondetException $ fromCurry s
  fromCurry _                 = internalError "non-deterministic IOError"

external_d_C_prim_error :: C_String -> Cover -> ConstStore -> a
external_d_C_prim_error s _ _ = C.throw $ UserException (fromCurry s)

external_d_C_prim_ioError :: C_IOError -> Cover -> ConstStore -> C_IO a
external_d_C_prim_ioError e _ _ = C.throw $ (fromCurry e :: CurryException)

external_d_C_catch :: C_IO a -> (C_IOError -> Cover -> ConstStore -> C_IO a)
  -> Cover -> ConstStore -> C_IO a
external_d_C_catch act hndl cd cs =
  fromIO $ C.catches (toIO errSupply1 cd cs act)
                     (exceptionHandlers errSupply2 cd cs (nd hndl))
  where
  errSupply1 = internalError "Prelude.catch: ID supply 1 used"
  errSupply2 = internalError "Prelude.catch: ID supply 2 used"

external_nd_C_catch :: C_IO a -> Func C_IOError (C_IO a)
  -> IDSupply -> Cover -> ConstStore -> C_IO a
external_nd_C_catch act hndl _ _ cs = HO_C_IO $ \s cd cs' -> do
  let cs1 = combineCs cs' cs
  res <- C.catches (toIO (leftSupply s) cd cs1 act)
                   (exceptionHandlers (rightSupply s) cd cs1 (nd_apply hndl))
  return (Right res)

exceptionHandlers :: IDSupply -> Cover -> ConstStore -> (C_IOError -> IDSupply -> Cover -> ConstStore -> C_IO a) -> [C.Handler a]
exceptionHandlers s cd cs hndl =
  [ C.Handler (\ (e :: CurryException) -> toIO (leftSupply s) cd cs (hndl (toCurry         e) (rightSupply s) cd cs))
  , C.Handler (\ (e ::  C.IOException) -> toIO (leftSupply s) cd cs (hndl (fromIOException e) (rightSupply s) cd cs))
  ] where fromIOException = toCurry . IOException . show

-- -----------------------------------------------------------------------------
-- Functions on Integer and Nat added from PrimTypes
-- -----------------------------------------------------------------------------

d_C_cmpNat :: Nat -> Nat -> Cover -> ConstStore -> C_Ordering
d_C_cmpNat x1 x2 cd cs = case x1 of
  IHi -> d_C__casept_33 x2 cd cs
  O x5 -> d_C__casept_32 x5 x2 cd cs
  I x9 -> d_C__casept_30 x9 x2 cd cs
  Choice_Nat d i l r -> narrow d i (d_C_cmpNat l x2 cd cs) (d_C_cmpNat r x2 cd
    cs)
  Choices_Nat d i xs -> narrows cs d i (\z -> d_C_cmpNat z x2 cd cs) xs
  Guard_Nat d c e -> guardCons d c (d_C_cmpNat e x2 cd $! addCs c cs)
  Fail_Nat d info -> failCons d (traceFail "Prelude.cmpNat" [show x1, show x2]
    info)
  _ -> failCons cd (consFail "Prelude.cmpNat" (showCons x1))

d_C_succNat :: Nat -> Cover -> ConstStore -> Nat
d_C_succNat x1 cd cs = case x1 of
  IHi -> O IHi
  O x2 -> I x2
  I x3 -> O (d_C_succNat x3 cd cs)
  Choice_Nat d i l r -> narrow d i (d_C_succNat l cd cs) (d_C_succNat r cd cs)
  Choices_Nat d i xs -> narrows cs d i (\z -> d_C_succNat z cd cs) xs
  Guard_Nat d c e -> guardCons d c (d_C_succNat e cd $! addCs c cs)
  Fail_Nat d info -> failCons d (traceFail "Prelude.succ" [show x1] info)
  _ -> failCons cd (consFail "Prelude.succ" (showCons x1))

d_C_predNat :: Nat -> Cover -> ConstStore -> Nat
d_C_predNat x1 cd cs = case x1 of
  IHi -> d_C_failed cd cs
  O x2 -> d_C__casept_28 x2 cd cs
  I x5 -> O x5
  Choice_Nat d i l r -> narrow d i (d_C_predNat l cd cs) (d_C_predNat r cd cs)
  Choices_Nat d i xs -> narrows cs d i (\z -> d_C_predNat z cd cs) xs
  Guard_Nat d c e -> guardCons d c (d_C_predNat e cd $! addCs c cs)
  Fail_Nat d info -> failCons d (traceFail "Prelude.pred" [show x1] info)
  _ -> failCons cd (consFail "Prelude.pred" (showCons x1))

d_OP_plus_caret :: Nat -> Nat -> Cover -> ConstStore -> Nat
d_OP_plus_caret x1 x2 cd cs = case x1 of
  IHi -> d_C_succNat x2 cd cs
  O x3 -> d_C__casept_27 x3 x2 cd cs
  I x6 -> d_C__casept_26 x6 x2 cd cs
  Choice_Nat d i l r -> narrow d i (d_OP_plus_caret l x2 cd cs)
    (d_OP_plus_caret r x2 cd cs)
  Choices_Nat d i xs -> narrows cs d i (\z -> d_OP_plus_caret z x2 cd cs) xs
  Guard_Nat d c e -> guardCons d c (d_OP_plus_caret e x2 cd $! addCs c cs)
  Fail_Nat d info -> failCons d (traceFail "Prelude.+^" [show x1, show x2] info)
  _ -> failCons cd (consFail "Prelude.+^" (showCons x1))

d_OP_minus_caret :: Nat -> Nat -> Cover -> ConstStore -> BinInt
d_OP_minus_caret x1 x2 cd cs = case x1 of
  IHi -> d_C_inc (Neg x2) cd cs
  O x3 -> d_C__casept_25 x3 x1 x2 cd cs
  I x6 -> d_C__casept_24 x6 x2 cd cs
  Choice_Nat d i l r -> narrow d i (d_OP_minus_caret l x2 cd cs)
    (d_OP_minus_caret r x2 cd cs)
  Choices_Nat d i xs -> narrows cs d i (\z -> d_OP_minus_caret z x2 cd cs) xs
  Guard_Nat d c e -> guardCons d c (d_OP_minus_caret e x2 cd $! addCs c cs)
  Fail_Nat d info -> failCons d (traceFail "Prelude.-^" [show x1, show x2] info)
  _ -> failCons cd (consFail "Prelude.-^" (showCons x1))

d_C_mult2 :: BinInt -> Cover -> ConstStore -> BinInt
d_C_mult2 x1 cd cs = case x1 of
  Pos x2 -> Pos (O x2)
  Zero -> Zero
  Neg x3 -> Neg (O x3)
  Choice_BinInt d i l r -> narrow d i (d_C_mult2 l cd cs) (d_C_mult2 r cd cs)
  Choices_BinInt d i xs -> narrows cs d i (\z -> d_C_mult2 z cd cs) xs
  Guard_BinInt d c e -> guardCons d c (d_C_mult2 e cd $! addCs c cs)
  Fail_BinInt d info -> failCons d (traceFail "Prelude.mult2" [show x1] info)
  _ -> failCons cd (consFail "Prelude.mult2" (showCons x1))

d_OP_star_caret :: Nat -> Nat -> Cover -> ConstStore -> Nat
d_OP_star_caret x1 x2 cd cs = case x1 of
  IHi -> x2
  O x3 -> O (d_OP_star_caret x3 x2 cd cs)
  I x4 -> d_OP_plus_caret x2 (O (d_OP_star_caret x4 x2 cd cs)) cd cs
  Choice_Nat d i l r -> narrow d i (d_OP_star_caret l x2 cd cs)
    (d_OP_star_caret r x2 cd cs)
  Choices_Nat d i xs -> narrows cs d i (\z -> d_OP_star_caret z x2 cd cs) xs
  Guard_Nat d c e -> guardCons d c (d_OP_star_caret e x2 cd $! addCs c cs)
  Fail_Nat d info -> failCons d (traceFail "Prelude.*^" [show x1, show x2] info)
  _ -> failCons cd (consFail "Prelude.*^" (showCons x1))

d_C_div2 :: Nat -> Cover -> ConstStore -> Nat
d_C_div2 x1 cd cs = case x1 of
  IHi -> d_C_failed cd cs
  O x2 -> x2
  I x3 -> x3
  Choice_Nat d i l r -> narrow d i (d_C_div2 l cd cs) (d_C_div2 r cd cs)
  Choices_Nat d i xs -> narrows cs d i (\z -> d_C_div2 z cd cs) xs
  Guard_Nat d c e -> guardCons d c (d_C_div2 e cd $! addCs c cs)
  Fail_Nat d info -> failCons d (traceFail "Prelude.div2" [show x1] info)
  _ -> failCons cd (consFail "Prelude.div2" (showCons x1))

d_C_mod2 :: Nat -> Cover -> ConstStore -> BinInt
d_C_mod2 x1 cd cs = case x1 of
  IHi -> Pos IHi
  O x2 -> Zero
  I x3 -> Pos IHi
  Choice_Nat d i l r -> narrow d i (d_C_mod2 l cd cs) (d_C_mod2 r cd cs)
  Choices_Nat d i xs -> narrows cs d i (\z -> d_C_mod2 z cd cs) xs
  Guard_Nat d c e -> guardCons d c (d_C_mod2 e cd $! addCs c cs)
  Fail_Nat d info -> failCons d (traceFail "Prelude.mod2" [show x1] info)
  _ -> failCons cd (consFail "Prelude.mod2" (showCons x1))

d_C_quotRemNat :: Nat -> Nat -> Cover -> ConstStore -> OP_Tuple2 BinInt BinInt
d_C_quotRemNat x1 x2 cd cs = d_C__casept_23 x2 x1 (d_C_prim_eqNat x2
  IHi cd cs) cd cs

d_OP_quotRemNat_dot_shift_dot_104 :: Nat -> Nat -> Cover -> ConstStore -> Nat
d_OP_quotRemNat_dot_shift_dot_104 x1 x2 cd cs = case x1 of
  IHi -> d_C_error (toCurryString
    "quotRemNat.shift: IHi") cd cs
  O x3 -> O x2
  I x4 -> I x2
  Choice_Nat d i l r -> narrow d i (d_OP_quotRemNat_dot_shift_dot_104 l x2 cd
    cs) (d_OP_quotRemNat_dot_shift_dot_104 r x2 cd cs)
  Choices_Nat d i xs -> narrows cs d i (\z ->
    d_OP_quotRemNat_dot_shift_dot_104 z x2 cd cs) xs
  Guard_Nat d c e -> guardCons d c (d_OP_quotRemNat_dot_shift_dot_104 e x2
    cd $! addCs c cs)
  Fail_Nat d info -> failCons d (traceFail "Prelude.quotRemNat.shift.104" [show x1
    , show x2] info)
  _ -> failCons cd (consFail "Prelude.quotRemNat.shift.104" (showCons x1))

d_C_lteqInteger :: BinInt -> BinInt -> Cover -> ConstStore
  -> C_Bool
d_C_lteqInteger x1 x2 cd cs =
  d_C_not (d_OP_eq_eq d_OP_uscore_inst_hash_Prelude_dot_Eq_hash_Prelude_dot_Ordering
    cd cs (d_C_cmpInteger x1 x2 cd cs) cd cs C_GT cd cs) cd cs

d_C_cmpInteger :: BinInt -> BinInt -> Cover -> ConstStore -> C_Ordering
d_C_cmpInteger x1 x2 cd cs = case x1 of
  Zero -> d_C__casept_14 x2 cd cs
  Pos x5 -> d_C__casept_13 x5 x2 cd cs
  Neg x8 -> d_C__casept_12 x8 x2 cd cs
  Choice_BinInt d i l r -> narrow d i (d_C_cmpInteger l x2 cd cs)
    (d_C_cmpInteger r x2 cd cs)
  Choices_BinInt d i xs -> narrows cs d i (\z -> d_C_cmpInteger z x2 cd cs) xs
  Guard_BinInt d c e -> guardCons d c (d_C_cmpInteger e x2 cd $! addCs c cs)
  Fail_BinInt d info -> failCons d (traceFail "Prelude.cmpInteger" [show x1, show
    x2] info)
  _ -> failCons cd (consFail "Prelude.cmpInteger" (showCons x1))

d_C_neg :: BinInt -> Cover -> ConstStore -> BinInt
d_C_neg x1 cd cs = case x1 of
  Zero -> Zero
  Pos x2 -> Neg x2
  Neg x3 -> Pos x3
  Choice_BinInt d i l r -> narrow d i (d_C_neg l cd cs) (d_C_neg r cd cs)
  Choices_BinInt d i xs -> narrows cs d i (\z -> d_C_neg z cd cs) xs
  Guard_BinInt d c e -> guardCons d c (d_C_neg e cd $! addCs c cs)
  Fail_BinInt d info -> failCons d (traceFail "Prelude.neg" [show x1] info)
  _ -> failCons cd (consFail "Prelude.neg" (showCons x1))

d_C_inc :: BinInt -> Cover -> ConstStore -> BinInt
d_C_inc x1 cd cs = case x1 of
  Zero -> Pos IHi
  Pos x2 -> Pos (d_C_succNat x2 cd cs)
  Neg x3 -> d_C__casept_11 x3 cd cs
  Choice_BinInt d i l r -> narrow d i (d_C_inc l cd cs) (d_C_inc r cd cs)
  Choices_BinInt d i xs -> narrows cs d i (\z -> d_C_inc z cd cs) xs
  Guard_BinInt d c e -> guardCons d c (d_C_inc e cd $! addCs c cs)
  Fail_BinInt d info -> failCons d (traceFail "Prelude.inc" [show x1] info)
  _ -> failCons cd (consFail "Prelude.inc" (showCons x1))

d_C_dec :: BinInt -> Cover -> ConstStore -> BinInt
d_C_dec x1 cd cs = case x1 of
  Zero -> Neg IHi
  Pos x2 -> d_C__casept_10 x2 cd cs
  Neg x5 -> Neg (d_C_succNat x5 cd cs)
  Choice_BinInt d i l r -> narrow d i (d_C_dec l cd cs) (d_C_dec r cd cs)
  Choices_BinInt d i xs -> narrows cs d i (\z -> d_C_dec z cd cs) xs
  Guard_BinInt d c e -> guardCons d c (d_C_dec e cd $! addCs c cs)
  Fail_BinInt d info -> failCons d (traceFail "Prelude.dec" [show x1] info)
  _ -> failCons cd (consFail "Prelude.dec" (showCons x1))

d_OP_plus_hash :: BinInt -> BinInt -> Cover -> ConstStore -> BinInt
d_OP_plus_hash x1 x2 cd cs = case x1 of
  Zero -> x2
  Pos x3 -> d_C__casept_9 x3 x1 x2 cd cs
  Neg x6 -> d_C__casept_8 x6 x1 x2 cd cs
  Choice_BinInt d i l r -> narrow d i (d_OP_plus_hash l x2 cd cs)
    (d_OP_plus_hash r x2 cd cs)
  Choices_BinInt d i xs -> narrows cs d i (\z -> d_OP_plus_hash z x2 cd cs) xs
  Guard_BinInt d c e -> guardCons d c (d_OP_plus_hash e x2 cd $! addCs c cs)
  Fail_BinInt d info -> failCons d (traceFail "Prelude.+#" [show x1, show x2]
    info)
  _ -> failCons cd (consFail "Prelude.+#" (showCons x1))

d_OP_minus_hash :: BinInt -> BinInt -> Cover -> ConstStore -> BinInt
d_OP_minus_hash x1 x2 cd cs = case x2 of
  Zero -> x1
  Pos x3 -> d_OP_plus_hash x1 (Neg x3) cd cs
  Neg x4 -> d_OP_plus_hash x1 (Pos x4) cd cs
  Choice_BinInt d i l r -> narrow d i (d_OP_minus_hash x1 l cd cs)
    (d_OP_minus_hash x1 r cd cs)
  Choices_BinInt d i xs -> narrows cs d i (\z -> d_OP_minus_hash x1 z cd cs)
    xs
  Guard_BinInt d c e -> guardCons d c (d_OP_minus_hash x1 e cd $! addCs c cs)
  Fail_BinInt d info -> failCons d (traceFail "Prelude.-#" [show x1, show x2]
    info)
  _ -> failCons cd (consFail "Prelude.-#" (showCons x2))

d_OP_star_hash :: BinInt -> BinInt -> Cover -> ConstStore -> BinInt
d_OP_star_hash x1 x2 cd cs = case x1 of
  Zero -> Zero
  Pos x3 -> d_C__casept_7 x3 x2 cd cs
  Neg x6 -> d_C__casept_6 x6 x2 cd cs
  Choice_BinInt d i l r -> narrow d i (d_OP_star_hash l x2 cd cs)
    (d_OP_star_hash r x2 cd cs)
  Choices_BinInt d i xs -> narrows cs d i (\z -> d_OP_star_hash z x2 cd cs) xs
  Guard_BinInt d c e -> guardCons d c (d_OP_star_hash e x2 cd $! addCs c cs)
  Fail_BinInt d info -> failCons d (traceFail "Prelude.*#" [show x1, show x2]
    info)
  _ -> failCons cd (consFail "Prelude.*#" (showCons x1))

d_C_quotRemInteger :: BinInt -> BinInt -> Cover -> ConstStore
  -> OP_Tuple2 BinInt BinInt
d_C_quotRemInteger x1 x2 cd cs = case x2 of
  Zero -> d_C_failed cd cs
  Pos x3 -> d_C__casept_5 x3 x1 cd cs
  Neg x9 -> d_C__casept_4 x9 x1 cd cs
  Choice_BinInt d i l r -> narrow d i (d_C_quotRemInteger x1 l cd cs)
    (d_C_quotRemInteger x1 r cd cs)
  Choices_BinInt d i xs -> narrows cs d i (\z -> d_C_quotRemInteger x1 z cd
    cs) xs
  Guard_BinInt d c e -> guardCons d c (d_C_quotRemInteger x1 e cd $! addCs c
    cs)
  Fail_BinInt d info -> failCons d (traceFail "Prelude.quotRemInteger" [show x1
    , show x2] info)
  _ -> failCons cd (consFail "Prelude.quotRemInteger" (showCons x2))

d_OP_quotRemInteger_dot_uscore_hash_selFP2_hash_d :: OP_Tuple2
  BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_quotRemInteger_dot_uscore_hash_selFP2_hash_d x1 cd cs = case x1 of
  OP_Tuple2 x2 x3 -> x2
  Choice_OP_Tuple2 d i l r -> narrow d i
    (d_OP_quotRemInteger_dot_uscore_hash_selFP2_hash_d l cd cs)
    (d_OP_quotRemInteger_dot_uscore_hash_selFP2_hash_d r cd cs)
  Choices_OP_Tuple2 d i xs -> narrows cs d i (\z ->
    d_OP_quotRemInteger_dot_uscore_hash_selFP2_hash_d z cd cs) xs
  Guard_OP_Tuple2 d c e -> guardCons d c
    (d_OP_quotRemInteger_dot_uscore_hash_selFP2_hash_d e cd $! addCs c cs)
  Fail_OP_Tuple2 d info -> failCons d (traceFail
    "Prelude.quotRemInteger._#selFP2#d" [show x1] info)
  _ -> failCons cd (consFail "Prelude.quotRemInteger._#selFP2#d" (showCons x1))

d_OP_quotRemInteger_dot_uscore_hash_selFP3_hash_m :: OP_Tuple2
  BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_quotRemInteger_dot_uscore_hash_selFP3_hash_m x1 cd cs = case x1 of
  OP_Tuple2 x2 x3 -> x3
  Choice_OP_Tuple2 d i l r -> narrow d i
    (d_OP_quotRemInteger_dot_uscore_hash_selFP3_hash_m l cd cs)
    (d_OP_quotRemInteger_dot_uscore_hash_selFP3_hash_m r cd cs)
  Choices_OP_Tuple2 d i xs -> narrows cs d i (\z ->
    d_OP_quotRemInteger_dot_uscore_hash_selFP3_hash_m z cd cs) xs
  Guard_OP_Tuple2 d c e -> guardCons d c
    (d_OP_quotRemInteger_dot_uscore_hash_selFP3_hash_m e cd $! addCs c cs)
  Fail_OP_Tuple2 d info -> failCons d (traceFail
    "Prelude.quotRemInteger._#selFP3#m" [show x1] info)
  _ -> failCons cd (consFail "Prelude.quotRemInteger._#selFP3#m" (showCons x1))

d_OP_quotRemInteger_dot_uscore_hash_selFP5_hash_d :: OP_Tuple2
  BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_quotRemInteger_dot_uscore_hash_selFP5_hash_d x1 cd cs = case x1 of
  OP_Tuple2 x2 x3 -> x2
  Choice_OP_Tuple2 d i l r -> narrow d i
    (d_OP_quotRemInteger_dot_uscore_hash_selFP5_hash_d l cd cs)
    (d_OP_quotRemInteger_dot_uscore_hash_selFP5_hash_d r cd cs)
  Choices_OP_Tuple2 d i xs -> narrows cs d i (\z ->
    d_OP_quotRemInteger_dot_uscore_hash_selFP5_hash_d z cd cs) xs
  Guard_OP_Tuple2 d c e -> guardCons d c
    (d_OP_quotRemInteger_dot_uscore_hash_selFP5_hash_d e cd $! addCs c cs)
  Fail_OP_Tuple2 d info -> failCons d (traceFail
    "Prelude.quotRemInteger._#selFP5#d" [show x1] info)
  _ -> failCons cd (consFail "Prelude.quotRemInteger._#selFP5#d" (showCons x1))

d_OP_quotRemInteger_dot_uscore_hash_selFP6_hash_m :: OP_Tuple2
  BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_quotRemInteger_dot_uscore_hash_selFP6_hash_m x1 cd cs = case x1 of
  OP_Tuple2 x2 x3 -> x3
  Choice_OP_Tuple2 d i l r -> narrow d i
    (d_OP_quotRemInteger_dot_uscore_hash_selFP6_hash_m l cd cs)
    (d_OP_quotRemInteger_dot_uscore_hash_selFP6_hash_m r cd cs)
  Choices_OP_Tuple2 d i xs -> narrows cs d i (\z ->
    d_OP_quotRemInteger_dot_uscore_hash_selFP6_hash_m z cd cs) xs
  Guard_OP_Tuple2 d c e -> guardCons d c
    (d_OP_quotRemInteger_dot_uscore_hash_selFP6_hash_m e cd $! addCs c cs)
  Fail_OP_Tuple2 d info -> failCons d (traceFail
    "Prelude.quotRemInteger._#selFP6#m" [show x1] info)
  _ -> failCons cd (consFail "Prelude.quotRemInteger._#selFP6#m" (showCons x1))

d_OP_quotRemInteger_dot_uscore_hash_selFP8_hash_d :: OP_Tuple2
  BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_quotRemInteger_dot_uscore_hash_selFP8_hash_d x1 cd cs = case x1 of
  OP_Tuple2 x2 x3 -> x2
  Choice_OP_Tuple2 d i l r -> narrow d i
    (d_OP_quotRemInteger_dot_uscore_hash_selFP8_hash_d l cd cs)
    (d_OP_quotRemInteger_dot_uscore_hash_selFP8_hash_d r cd cs)
  Choices_OP_Tuple2 d i xs -> narrows cs d i (\z ->
    d_OP_quotRemInteger_dot_uscore_hash_selFP8_hash_d z cd cs) xs
  Guard_OP_Tuple2 d c e -> guardCons d c
    (d_OP_quotRemInteger_dot_uscore_hash_selFP8_hash_d e cd $! addCs c cs)
  Fail_OP_Tuple2 d info -> failCons d (traceFail
    "Prelude.quotRemInteger._#selFP8#d" [show x1] info)
  _ -> failCons cd (consFail "Prelude.quotRemInteger._#selFP8#d" (showCons x1))

d_OP_quotRemInteger_dot_uscore_hash_selFP9_hash_m :: OP_Tuple2
  BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_quotRemInteger_dot_uscore_hash_selFP9_hash_m x1 cd cs = case x1 of
  OP_Tuple2 x2 x3 -> x3
  Choice_OP_Tuple2 d i l r -> narrow d i
    (d_OP_quotRemInteger_dot_uscore_hash_selFP9_hash_m l cd cs)
    (d_OP_quotRemInteger_dot_uscore_hash_selFP9_hash_m r cd cs)
  Choices_OP_Tuple2 d i xs -> narrows cs d i (\z ->
    d_OP_quotRemInteger_dot_uscore_hash_selFP9_hash_m z cd cs) xs
  Guard_OP_Tuple2 d c e -> guardCons d c
    (d_OP_quotRemInteger_dot_uscore_hash_selFP9_hash_m e cd $! addCs c cs)
  Fail_OP_Tuple2 d info -> failCons d (traceFail
    "Prelude.quotRemInteger._#selFP9#m" [show x1] info)
  _ -> failCons cd (consFail "Prelude.quotRemInteger._#selFP9#m" (showCons x1))

d_C_divModInteger :: BinInt -> BinInt -> Cover -> ConstStore
  -> OP_Tuple2 BinInt BinInt
d_C_divModInteger x1 x2 cd cs = case x2 of
  Zero -> d_C_failed cd cs
  Pos x3 -> d_C__casept_3 x3 x1 cd cs
  Neg x12 -> d_C__casept_1 x12 x1 cd cs
  Choice_BinInt d i l r -> narrow d i (d_C_divModInteger x1 l cd cs)
    (d_C_divModInteger x1 r cd cs)
  Choices_BinInt d i xs -> narrows cs d i (\z -> d_C_divModInteger x1 z cd cs)
    xs
  Guard_BinInt d c e -> guardCons d c (d_C_divModInteger x1 e cd $! addCs c
    cs)
  Fail_BinInt d info -> failCons d (traceFail "Prelude.divModInteger" [show x1
    , show x2] info)
  _ -> failCons cd (consFail "Prelude.divModInteger" (showCons x2))

d_OP_divModInteger_dot_uscore_hash_selFP11_hash_d :: OP_Tuple2
  BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_divModInteger_dot_uscore_hash_selFP11_hash_d x1 cd cs = case x1 of
  OP_Tuple2 x2 x3 -> x2
  Choice_OP_Tuple2 d i l r -> narrow d i
    (d_OP_divModInteger_dot_uscore_hash_selFP11_hash_d l cd cs)
    (d_OP_divModInteger_dot_uscore_hash_selFP11_hash_d r cd cs)
  Choices_OP_Tuple2 d i xs -> narrows cs d i (\z ->
    d_OP_divModInteger_dot_uscore_hash_selFP11_hash_d z cd cs) xs
  Guard_OP_Tuple2 d c e -> guardCons d c
    (d_OP_divModInteger_dot_uscore_hash_selFP11_hash_d e cd $! addCs c cs)
  Fail_OP_Tuple2 d info -> failCons d (traceFail
    "Prelude.divModInteger._#selFP11#d" [show x1] info)
  _ -> failCons cd (consFail "Prelude.divModInteger._#selFP11#d" (showCons x1))

d_OP_divModInteger_dot_uscore_hash_selFP12_hash_m :: OP_Tuple2
  BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_divModInteger_dot_uscore_hash_selFP12_hash_m x1 cd cs = case x1 of
  OP_Tuple2 x2 x3 -> x3
  Choice_OP_Tuple2 d i l r -> narrow d i
    (d_OP_divModInteger_dot_uscore_hash_selFP12_hash_m l cd cs)
    (d_OP_divModInteger_dot_uscore_hash_selFP12_hash_m r cd cs)
  Choices_OP_Tuple2 d i xs -> narrows cs d i (\z ->
    d_OP_divModInteger_dot_uscore_hash_selFP12_hash_m z cd cs) xs
  Guard_OP_Tuple2 d c e -> guardCons d c
    (d_OP_divModInteger_dot_uscore_hash_selFP12_hash_m e cd $! addCs c cs)
  Fail_OP_Tuple2 d info -> failCons d (traceFail
    "Prelude.divModInteger._#selFP12#m" [show x1] info)
  _ -> failCons cd (consFail "Prelude.divModInteger._#selFP12#m" (showCons x1))

d_OP_divModInteger_dot_uscore_hash_selFP14_hash_d :: OP_Tuple2
  BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_divModInteger_dot_uscore_hash_selFP14_hash_d x1 cd cs = case x1 of
  OP_Tuple2 x2 x3 -> x2
  Choice_OP_Tuple2 d i l r -> narrow d i
    (d_OP_divModInteger_dot_uscore_hash_selFP14_hash_d l cd cs)
    (d_OP_divModInteger_dot_uscore_hash_selFP14_hash_d r cd cs)
  Choices_OP_Tuple2 d i xs -> narrows cs d i (\z ->
    d_OP_divModInteger_dot_uscore_hash_selFP14_hash_d z cd cs) xs
  Guard_OP_Tuple2 d c e -> guardCons d c
    (d_OP_divModInteger_dot_uscore_hash_selFP14_hash_d e cd $! addCs c cs)
  Fail_OP_Tuple2 d info -> failCons d (traceFail
    "Prelude.divModInteger._#selFP14#d" [show x1] info)
  _ -> failCons cd (consFail "Prelude.divModInteger._#selFP14#d" (showCons x1))

d_OP_divModInteger_dot_uscore_hash_selFP15_hash_m :: OP_Tuple2
  BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_divModInteger_dot_uscore_hash_selFP15_hash_m x1 cd cs = case x1 of
  OP_Tuple2 x2 x3 -> x3
  Choice_OP_Tuple2 d i l r -> narrow d i
    (d_OP_divModInteger_dot_uscore_hash_selFP15_hash_m l cd cs)
    (d_OP_divModInteger_dot_uscore_hash_selFP15_hash_m r cd cs)
  Choices_OP_Tuple2 d i xs -> narrows cs d i (\z ->
    d_OP_divModInteger_dot_uscore_hash_selFP15_hash_m z cd cs) xs
  Guard_OP_Tuple2 d c e -> guardCons d c
    (d_OP_divModInteger_dot_uscore_hash_selFP15_hash_m e cd $! addCs c cs)
  Fail_OP_Tuple2 d info -> failCons d (traceFail
    "Prelude.divModInteger._#selFP15#m" [show x1] info)
  _ -> failCons cd (consFail "Prelude.divModInteger._#selFP15#m" (showCons x1))

d_OP_divModInteger_dot_uscore_hash_selFP17_hash_d :: OP_Tuple2
  BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_divModInteger_dot_uscore_hash_selFP17_hash_d x1 cd cs = case x1 of
  OP_Tuple2 x2 x3 -> x2
  Choice_OP_Tuple2 d i l r -> narrow d i
    (d_OP_divModInteger_dot_uscore_hash_selFP17_hash_d l cd cs)
    (d_OP_divModInteger_dot_uscore_hash_selFP17_hash_d r cd cs)
  Choices_OP_Tuple2 d i xs -> narrows cs d i (\z ->
    d_OP_divModInteger_dot_uscore_hash_selFP17_hash_d z cd cs) xs
  Guard_OP_Tuple2 d c e -> guardCons d c
    (d_OP_divModInteger_dot_uscore_hash_selFP17_hash_d e cd $! addCs c cs)
  Fail_OP_Tuple2 d info -> failCons d (traceFail
    "Prelude.divModInteger._#selFP17#d" [show x1] info)
  _ -> failCons cd (consFail "Prelude.divModInteger._#selFP17#d" (showCons x1))

d_OP_divModInteger_dot_uscore_hash_selFP18_hash_m :: OP_Tuple2
  BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_divModInteger_dot_uscore_hash_selFP18_hash_m x1 cd cs = case x1 of
  OP_Tuple2 x2 x3 -> x3
  Choice_OP_Tuple2 d i l r -> narrow d i
    (d_OP_divModInteger_dot_uscore_hash_selFP18_hash_m l cd cs)
    (d_OP_divModInteger_dot_uscore_hash_selFP18_hash_m r cd cs)
  Choices_OP_Tuple2 d i xs -> narrows cs d i (\z ->
    d_OP_divModInteger_dot_uscore_hash_selFP18_hash_m z cd cs) xs
  Guard_OP_Tuple2 d c e -> guardCons d c
    (d_OP_divModInteger_dot_uscore_hash_selFP18_hash_m e cd $! addCs c cs)
  Fail_OP_Tuple2 d info -> failCons d (traceFail
    "Prelude.divModInteger._#selFP18#m" [show x1] info)
  _ -> failCons cd (consFail "Prelude.divModInteger._#selFP18#m" (showCons x1))

d_C_divInteger :: BinInt -> BinInt -> Cover -> ConstStore -> BinInt
d_C_divInteger x1 x2 cd cs = d_C_fst (d_C_divModInteger x1 x2 cd
  cs) cd cs

d_C_modInteger :: BinInt -> BinInt -> Cover -> ConstStore -> BinInt
d_C_modInteger x1 x2 cd cs = d_C_snd (d_C_divModInteger x1 x2 cd
  cs) cd cs

d_C_quotInteger :: BinInt -> BinInt -> Cover -> ConstStore -> BinInt
d_C_quotInteger x1 x2 cd cs = d_C_fst (d_C_quotRemInteger x1 x2 cd
  cs) cd cs

d_C_remInteger :: BinInt -> BinInt -> Cover -> ConstStore -> BinInt
d_C_remInteger x1 x2 cd cs = d_C_snd (d_C_quotRemInteger x1 x2 cd
  cs) cd cs

d_C__casept_1 :: Nat -> BinInt -> Cover -> ConstStore
  -> OP_Tuple2 BinInt BinInt
d_C__casept_1 x12 x1 cd cs = case x1 of
  Zero -> OP_Tuple2 Zero Zero
  Pos x13 -> let x14 = d_C_quotRemNat x13 x12 cd cs
                 x15 = d_OP_divModInteger_dot_uscore_hash_selFP14_hash_d x14
                   cd cs
                 x16 = d_OP_divModInteger_dot_uscore_hash_selFP15_hash_m x14
                   cd cs
                 x17 = OP_Tuple2 (d_C_neg (d_C_inc x15 cd cs) cd
                   cs) (d_OP_minus_hash x16 (Pos x12) cd cs)
    in d_C__casept_0 x17 x15 x16 cd cs
  Neg x20 -> let x21 = d_C_quotRemNat x20 x12 cd cs
                 x22 = d_OP_divModInteger_dot_uscore_hash_selFP17_hash_d x21
                   cd cs
                 x23 = d_OP_divModInteger_dot_uscore_hash_selFP18_hash_m x21
                   cd cs
    in OP_Tuple2 x22 (d_C_neg x23 cd cs)
  Choice_BinInt d i l r -> narrow d i (d_C__casept_1 x12 l cd cs) (d_C__casept_1
    x12 r cd cs)
  Choices_BinInt d i xs -> narrows cs d i (\z -> d_C__casept_1 x12 z cd cs) xs
  Guard_BinInt d c e -> guardCons d c (d_C__casept_1 x12 e cd $! addCs c cs)
  Fail_BinInt d info -> failCons d (traceFail "Prelude._casept_1" [show x12, show
    x1] info)
  _ -> failCons cd (consFail "Prelude._casept_1" (showCons x1))

d_C__casept_0 :: OP_Tuple2 BinInt BinInt -> BinInt -> BinInt
  -> Cover -> ConstStore -> OP_Tuple2 BinInt BinInt
d_C__casept_0 x17 x15 x16 cd cs = case x16 of
  Zero -> OP_Tuple2 (d_C_neg x15 cd cs) x16
  Neg x18 -> x17
  Pos x19 -> x17
  Choice_BinInt d i l r -> narrow d i (d_C__casept_0 x17 x15 l cd cs)
    (d_C__casept_0 x17 x15 r cd cs)
  Choices_BinInt d i xs -> narrows cs d i (\z -> d_C__casept_0 x17 x15 z cd cs)
    xs
  Guard_BinInt d c e -> guardCons d c (d_C__casept_0 x17 x15 e cd $! addCs c cs)
  Fail_BinInt d info -> failCons d (traceFail "Prelude._casept_0" [show x17, show
    x15, show x16] info)
  _ -> failCons cd (consFail "Prelude._casept_0" (showCons x16))

d_C__casept_3 :: Nat -> BinInt -> Cover -> ConstStore
  -> OP_Tuple2 BinInt BinInt
d_C__casept_3 x3 x1 cd cs = case x1 of
  Zero -> OP_Tuple2 Zero Zero
  Pos x4 -> d_C_quotRemNat x4 x3 cd cs
  Neg x5 -> let x6 = d_C_quotRemNat x5 x3 cd cs
                x7 = d_OP_divModInteger_dot_uscore_hash_selFP11_hash_d x6 cd
                  cs
                x8 = d_OP_divModInteger_dot_uscore_hash_selFP12_hash_m x6 cd
                  cs
                x9 = OP_Tuple2 (d_C_neg (d_C_inc x7 cd cs) cd
                  cs) (d_OP_minus_hash (Pos x3) x8 cd cs)
    in d_C__casept_2 x9 x7 x8 cd cs
  Choice_BinInt d i l r -> narrow d i (d_C__casept_3 x3 l cd cs) (d_C__casept_3 x3
    r cd cs)
  Choices_BinInt d i xs -> narrows cs d i (\z -> d_C__casept_3 x3 z cd cs) xs
  Guard_BinInt d c e -> guardCons d c (d_C__casept_3 x3 e cd $! addCs c cs)
  Fail_BinInt d info -> failCons d (traceFail "Prelude._casept_3" [show x3, show x1]
    info)
  _ -> failCons cd (consFail "Prelude._casept_3" (showCons x1))

d_C__casept_2 :: OP_Tuple2 BinInt BinInt -> BinInt -> BinInt
  -> Cover -> ConstStore -> OP_Tuple2 BinInt BinInt
d_C__casept_2 x9 x7 x8 cd cs = case x8 of
  Zero -> OP_Tuple2 (d_C_neg x7 cd cs) x8
  Neg x10 -> x9
  Pos x11 -> x9
  Choice_BinInt d i l r -> narrow d i (d_C__casept_2 x9 x7 l cd cs) (d_C__casept_2
    x9 x7 r cd cs)
  Choices_BinInt d i xs -> narrows cs d i (\z -> d_C__casept_2 x9 x7 z cd cs) xs
  Guard_BinInt d c e -> guardCons d c (d_C__casept_2 x9 x7 e cd $! addCs c cs)
  Fail_BinInt d info -> failCons d (traceFail "Prelude._casept_2" [show x9, show x7
    , show x8] info)
  _ -> failCons cd (consFail "Prelude._casept_2" (showCons x8))

d_C__casept_4 :: Nat -> BinInt -> Cover -> ConstStore
  -> OP_Tuple2 BinInt BinInt
d_C__casept_4 x9 x1 cd cs = case x1 of
  Zero -> OP_Tuple2 Zero Zero
  Pos x10 -> let x11 = d_C_quotRemNat x10 x9 cd cs
                 x12 = d_OP_quotRemInteger_dot_uscore_hash_selFP5_hash_d x11
                   cd cs
                 x13 = d_OP_quotRemInteger_dot_uscore_hash_selFP6_hash_m x11
                   cd cs
    in OP_Tuple2 (d_C_neg x12 cd cs) x13
  Neg x14 -> let x15 = d_C_quotRemNat x14 x9 cd cs
                 x16 = d_OP_quotRemInteger_dot_uscore_hash_selFP8_hash_d x15
                   cd cs
                 x17 = d_OP_quotRemInteger_dot_uscore_hash_selFP9_hash_m x15
                   cd cs
    in OP_Tuple2 x16 (d_C_neg x17 cd cs)
  Choice_BinInt d i l r -> narrow d i (d_C__casept_4 x9 l cd cs) (d_C__casept_4 x9
    r cd cs)
  Choices_BinInt d i xs -> narrows cs d i (\z -> d_C__casept_4 x9 z cd cs) xs
  Guard_BinInt d c e -> guardCons d c (d_C__casept_4 x9 e cd $! addCs c cs)
  Fail_BinInt d info -> failCons d (traceFail "Prelude._casept_4" [show x9, show x1]
    info)
  _ -> failCons cd (consFail "Prelude._casept_4" (showCons x1))

d_C__casept_5 :: Nat -> BinInt -> Cover -> ConstStore
  -> OP_Tuple2 BinInt BinInt
d_C__casept_5 x3 x1 cd cs = case x1 of
  Zero -> OP_Tuple2 Zero Zero
  Pos x4 -> d_C_quotRemNat x4 x3 cd cs
  Neg x5 -> let x6 = d_C_quotRemNat x5 x3 cd cs
                x7 = d_OP_quotRemInteger_dot_uscore_hash_selFP2_hash_d x6 cd
                  cs
                x8 = d_OP_quotRemInteger_dot_uscore_hash_selFP3_hash_m x6 cd
                  cs
    in OP_Tuple2 (d_C_neg x7 cd cs) (d_C_neg x8 cd cs)
  Choice_BinInt d i l r -> narrow d i (d_C__casept_5 x3 l cd cs) (d_C__casept_5 x3
    r cd cs)
  Choices_BinInt d i xs -> narrows cs d i (\z -> d_C__casept_5 x3 z cd cs) xs
  Guard_BinInt d c e -> guardCons d c (d_C__casept_5 x3 e cd $! addCs c cs)
  Fail_BinInt d info -> failCons d (traceFail "Prelude._casept_5" [show x3, show x1]
    info)
  _ -> failCons cd (consFail "Prelude._casept_5" (showCons x1))

d_C__casept_6 :: Nat -> BinInt -> Cover -> ConstStore -> BinInt
d_C__casept_6 x6 x2 cd cs = case x2 of
  Zero -> Zero
  Pos x7 -> Neg (d_OP_star_caret x6 x7 cd cs)
  Neg x8 -> Pos (d_OP_star_caret x6 x8 cd cs)
  Choice_BinInt d i l r -> narrow d i (d_C__casept_6 x6 l cd cs) (d_C__casept_6 x6
    r cd cs)
  Choices_BinInt d i xs -> narrows cs d i (\z -> d_C__casept_6 x6 z cd cs) xs
  Guard_BinInt d c e -> guardCons d c (d_C__casept_6 x6 e cd $! addCs c cs)
  Fail_BinInt d info -> failCons d (traceFail "Prelude._casept_6" [show x6, show x2]
    info)
  _ -> failCons cd (consFail "Prelude._casept_6" (showCons x2))

d_C__casept_7 :: Nat -> BinInt -> Cover -> ConstStore -> BinInt
d_C__casept_7 x3 x2 cd cs = case x2 of
  Zero -> Zero
  Pos x4 -> Pos (d_OP_star_caret x3 x4 cd cs)
  Neg x5 -> Neg (d_OP_star_caret x3 x5 cd cs)
  Choice_BinInt d i l r -> narrow d i (d_C__casept_7 x3 l cd cs) (d_C__casept_7 x3
    r cd cs)
  Choices_BinInt d i xs -> narrows cs d i (\z -> d_C__casept_7 x3 z cd cs) xs
  Guard_BinInt d c e -> guardCons d c (d_C__casept_7 x3 e cd $! addCs c cs)
  Fail_BinInt d info -> failCons d (traceFail "Prelude._casept_7" [show x3, show x2]
    info)
  _ -> failCons cd (consFail "Prelude._casept_7" (showCons x2))

d_C__casept_8 :: Nat -> BinInt -> BinInt -> Cover -> ConstStore -> BinInt
d_C__casept_8 x6 x1 x2 cd cs = case x2 of
  Zero -> x1
  Pos x7 -> d_OP_minus_caret x7 x6 cd cs
  Neg x8 -> Neg (d_OP_plus_caret x6 x8 cd cs)
  Choice_BinInt d i l r -> narrow d i (d_C__casept_8 x6 x1 l cd cs) (d_C__casept_8
    x6 x1 r cd cs)
  Choices_BinInt d i xs -> narrows cs d i (\z -> d_C__casept_8 x6 x1 z cd cs) xs
  Guard_BinInt d c e -> guardCons d c (d_C__casept_8 x6 x1 e cd $! addCs c cs)
  Fail_BinInt d info -> failCons d (traceFail "Prelude._casept_8" [show x6, show x1
    , show x2] info)
  _ -> failCons cd (consFail "Prelude._casept_8" (showCons x2))

d_C__casept_9 :: Nat -> BinInt -> BinInt -> Cover -> ConstStore -> BinInt
d_C__casept_9 x3 x1 x2 cd cs = case x2 of
  Zero -> x1
  Pos x4 -> Pos (d_OP_plus_caret x3 x4 cd cs)
  Neg x5 -> d_OP_minus_caret x3 x5 cd cs
  Choice_BinInt d i l r -> narrow d i (d_C__casept_9 x3 x1 l cd cs) (d_C__casept_9
    x3 x1 r cd cs)
  Choices_BinInt d i xs -> narrows cs d i (\z -> d_C__casept_9 x3 x1 z cd cs) xs
  Guard_BinInt d c e -> guardCons d c (d_C__casept_9 x3 x1 e cd $! addCs c cs)
  Fail_BinInt d info -> failCons d (traceFail "Prelude._casept_9" [show x3, show x1
    , show x2] info)
  _ -> failCons cd (consFail "Prelude._casept_9" (showCons x2))

d_C__casept_10 :: Nat -> Cover -> ConstStore -> BinInt
d_C__casept_10 x2 cd cs = case x2 of
  IHi -> Zero
  O x3 -> Pos (d_C_predNat (O x3) cd cs)
  I x4 -> Pos (O x4)
  Choice_Nat d i l r -> narrow d i (d_C__casept_10 l cd cs) (d_C__casept_10 r cd
    cs)
  Choices_Nat d i xs -> narrows cs d i (\z -> d_C__casept_10 z cd cs) xs
  Guard_Nat d c e -> guardCons d c (d_C__casept_10 e cd $! addCs c cs)
  Fail_Nat d info -> failCons d (traceFail "Prelude._casept_10" [show x2] info)
  _ -> failCons cd (consFail "Prelude._casept_10" (showCons x2))

d_C__casept_11 :: Nat -> Cover -> ConstStore -> BinInt
d_C__casept_11 x3 cd cs = case x3 of
  IHi -> Zero
  O x4 -> Neg (d_C_predNat (O x4) cd cs)
  I x5 -> Neg (O x5)
  Choice_Nat d i l r -> narrow d i (d_C__casept_11 l cd cs) (d_C__casept_11 r cd
    cs)
  Choices_Nat d i xs -> narrows cs d i (\z -> d_C__casept_11 z cd cs) xs
  Guard_Nat d c e -> guardCons d c (d_C__casept_11 e cd $! addCs c cs)
  Fail_Nat d info -> failCons d (traceFail "Prelude._casept_11" [show x3] info)
  _ -> failCons cd (consFail "Prelude._casept_11" (showCons x3))

d_C__casept_12 :: Nat -> BinInt -> Cover -> ConstStore -> C_Ordering
d_C__casept_12 x8 x2 cd cs = case x2 of
  Zero -> C_LT
  Pos x9 -> C_LT
  Neg x10 -> d_C_cmpNat x10 x8 cd cs
  Choice_BinInt d i l r -> narrow d i (d_C__casept_12 x8 l cd cs) (d_C__casept_12
    x8 r cd cs)
  Choices_BinInt d i xs -> narrows cs d i (\z -> d_C__casept_12 x8 z cd cs) xs
  Guard_BinInt d c e -> guardCons d c (d_C__casept_12 x8 e cd $! addCs c cs)
  Fail_BinInt d info -> failCons d (traceFail "Prelude._casept_12" [show x8, show
    x2] info)
  _ -> failCons cd (consFail "Prelude._casept_12" (showCons x2))

d_C__casept_13 :: Nat -> BinInt -> Cover -> ConstStore -> C_Ordering
d_C__casept_13 x5 x2 cd cs = case x2 of
  Zero -> C_GT
  Pos x6 -> d_C_cmpNat x5 x6 cd cs
  Neg x7 -> C_GT
  Choice_BinInt d i l r -> narrow d i (d_C__casept_13 x5 l cd cs) (d_C__casept_13
    x5 r cd cs)
  Choices_BinInt d i xs -> narrows cs d i (\z -> d_C__casept_13 x5 z cd cs) xs
  Guard_BinInt d c e -> guardCons d c (d_C__casept_13 x5 e cd $! addCs c cs)
  Fail_BinInt d info -> failCons d (traceFail "Prelude._casept_13" [show x5, show
    x2] info)
  _ -> failCons cd (consFail "Prelude._casept_13" (showCons x2))

d_C__casept_14 :: BinInt -> Cover -> ConstStore -> C_Ordering
d_C__casept_14 x2 cd cs = case x2 of
  Zero -> C_EQ
  Pos x3 -> C_LT
  Neg x4 -> C_GT
  Choice_BinInt d i l r -> narrow d i (d_C__casept_14 l cd cs) (d_C__casept_14 r
    cd cs)
  Choices_BinInt d i xs -> narrows cs d i (\z -> d_C__casept_14 z cd cs) xs
  Guard_BinInt d c e -> guardCons d c (d_C__casept_14 e cd $! addCs c cs)
  Fail_BinInt d info -> failCons d (traceFail "Prelude._casept_14" [show x2] info)
  _ -> failCons cd (consFail "Prelude._casept_14" (showCons x2))

d_C__casept_23 :: Nat -> Nat -> C_Bool -> Cover -> ConstStore
  -> OP_Tuple2 BinInt BinInt
d_C__casept_23 x2 x1 x3 cd cs = case x3 of
  C_True -> OP_Tuple2 (Pos x1) Zero
  C_False -> d_C__casept_22 x1 x2 (d_C_prim_eqNat x1 IHi
    cd cs) cd cs
  Choice_C_Bool d i l r -> narrow d i (d_C__casept_23 x2 x1 l cd cs)
    (d_C__casept_23 x2 x1 r cd cs)
  Choices_C_Bool d i xs -> narrows cs d i (\z -> d_C__casept_23 x2
    x1 z cd cs) xs
  Guard_C_Bool d c e -> guardCons d c (d_C__casept_23 x2 x1 e cd $!
    addCs c cs)
  Fail_C_Bool d info -> failCons d (traceFail "Prelude._casept_23" [show
    x2, show x1, show x3] info)
  _ -> failCons cd (consFail "Prelude._casept_23" (showCons x3))

d_C__casept_22 :: Nat -> Nat -> C_Bool -> Cover -> ConstStore
  -> OP_Tuple2 BinInt BinInt
d_C__casept_22 x1 x2 x3 cd cs = case x3 of
  C_True -> OP_Tuple2 Zero (Pos IHi)
  C_False -> d_C__casept_21 x2 x1 (d_C_otherwise cd
    cs) cd cs
  Choice_C_Bool d i l r -> narrow d i (d_C__casept_22 x1 x2 l cd cs)
    (d_C__casept_22 x1 x2 r cd cs)
  Choices_C_Bool d i xs -> narrows cs d i (\z -> d_C__casept_22 x1
    x2 z cd cs) xs
  Guard_C_Bool d c e -> guardCons d c (d_C__casept_22 x1 x2 e cd $!
    addCs c cs)
  Fail_C_Bool d info -> failCons d (traceFail "Prelude._casept_22" [show
    x1, show x2, show x3] info)
  _ -> failCons cd (consFail "Prelude._casept_22" (showCons x3))

d_C__casept_21 :: Nat -> Nat -> C_Bool -> Cover -> ConstStore
  -> OP_Tuple2 BinInt BinInt
d_C__casept_21 x2 x1 x3 cd cs = case x3 of
  C_True -> d_C__casept_20 x2 x1 (d_C_cmpNat x1 x2 cd cs) cd cs
  C_False -> d_C_failed cd cs
  Choice_C_Bool d i l r -> narrow d i (d_C__casept_21 x2 x1 l cd cs)
    (d_C__casept_21 x2 x1 r cd cs)
  Choices_C_Bool d i xs -> narrows cs d i (\z -> d_C__casept_21 x2
    x1 z cd cs) xs
  Guard_C_Bool d c e -> guardCons d c (d_C__casept_21 x2 x1 e cd $!
    addCs c cs)
  Fail_C_Bool d info -> failCons d (traceFail "Prelude._casept_21" [show
    x2, show x1, show x3] info)
  _ -> failCons cd (consFail "Prelude._casept_21" (showCons x3))

d_C__casept_20 :: Nat -> Nat -> C_Ordering -> Cover
  -> ConstStore -> OP_Tuple2 BinInt BinInt
d_C__casept_20 x2 x1 x3 cd cs = case x3 of
  C_EQ -> OP_Tuple2 (Pos IHi) Zero
  C_LT -> OP_Tuple2 Zero (Pos x1)
  C_GT -> d_C__casept_19 x2 x1 (d_C_quotRemNat (d_C_div2 x1 cd cs)
    x2 cd cs) cd cs
  Choice_C_Ordering d i l r -> narrow d i (d_C__casept_20 x2 x1 l cd
    cs) (d_C__casept_20 x2 x1 r cd cs)
  Choices_C_Ordering d i xs -> narrows cs d i (\z -> d_C__casept_20
    x2 x1 z cd cs) xs
  Guard_C_Ordering d c e -> guardCons d c (d_C__casept_20 x2 x1 e
    cd $! addCs c cs)
  Fail_C_Ordering d info -> failCons d (traceFail "Prelude._casept_20"
    [show x2, show x1, show x3] info)
  _ -> failCons cd (consFail "Prelude._casept_20" (showCons x3))

d_C__casept_19 :: Nat -> Nat -> OP_Tuple2 BinInt BinInt
  -> Cover -> ConstStore -> OP_Tuple2 BinInt BinInt
d_C__casept_19 x2 x1 x5 cd cs = case x5 of
  OP_Tuple2 x3 x4 -> d_C__casept_18 x4 x2 x1 x3 cd cs
  Choice_OP_Tuple2 d i l r -> narrow d i (d_C__casept_19 x2 x1 l cd
    cs) (d_C__casept_19 x2 x1 r cd cs)
  Choices_OP_Tuple2 d i xs -> narrows cs d i (\z -> d_C__casept_19
    x2 x1 z cd cs) xs
  Guard_OP_Tuple2 d c e -> guardCons d c (d_C__casept_19 x2 x1 e
    cd $! addCs c cs)
  Fail_OP_Tuple2 d info -> failCons d (traceFail "Prelude._casept_19"
    [show x2, show x1, show x5] info)
  _ -> failCons cd (consFail "Prelude._casept_19" (showCons x5))

d_C__casept_18 :: BinInt -> Nat -> Nat -> BinInt -> Cover -> ConstStore
  -> OP_Tuple2 BinInt BinInt
d_C__casept_18 x4 x2 x1 x3 cd cs = case x3 of
  Neg x5 -> d_C_error (toCurryString
    "quotRemNat: negative quotient") cd cs
  Zero -> OP_Tuple2 (Pos IHi) (d_OP_minus_caret x1 x2 cd cs)
  Pos x6 -> d_C__casept_17 x2 x1 x6 x4 cd cs
  Choice_BinInt d i l r -> narrow d i (d_C__casept_18 x4 x2 x1 l cd cs)
    (d_C__casept_18 x4 x2 x1 r cd cs)
  Choices_BinInt d i xs -> narrows cs d i (\z -> d_C__casept_18 x4 x2 x1 z cd
    cs) xs
  Guard_BinInt d c e -> guardCons d c (d_C__casept_18 x4 x2 x1 e cd $! addCs c
    cs)
  Fail_BinInt d info -> failCons d (traceFail "Prelude._casept_18" [show x4, show x2
    , show x1, show x3] info)
  _ -> failCons cd (consFail "Prelude._casept_18" (showCons x3))

d_C__casept_17 :: Nat -> Nat -> Nat -> BinInt -> Cover -> ConstStore
  -> OP_Tuple2 BinInt BinInt
d_C__casept_17 x2 x1 x6 x4 cd cs = case x4 of
  Neg x7 -> d_C_error (toCurryString
    "quotRemNat: negative remainder") cd cs
  Zero -> OP_Tuple2 (Pos (O x6)) (d_C_mod2 x1 cd cs)
  Pos x8 -> d_C__casept_16 x2 x8 x1 x6 (d_C_quotRemNat
    (d_OP_quotRemNat_dot_shift_dot_104 x1 x8 cd cs) x2 cd cs) cd cs
  Choice_BinInt d i l r -> narrow d i (d_C__casept_17 x2 x1 x6 l cd cs)
    (d_C__casept_17 x2 x1 x6 r cd cs)
  Choices_BinInt d i xs -> narrows cs d i (\z -> d_C__casept_17 x2 x1 x6 z cd
    cs) xs
  Guard_BinInt d c e -> guardCons d c (d_C__casept_17 x2 x1 x6 e cd $! addCs c
    cs)
  Fail_BinInt d info -> failCons d (traceFail "Prelude._casept_17" [show x2, show x1
    , show x6, show x4] info)
  _ -> failCons cd (consFail "Prelude._casept_17" (showCons x4))

d_C__casept_16 :: Nat -> Nat -> Nat -> Nat -> OP_Tuple2
  BinInt BinInt -> Cover -> ConstStore -> OP_Tuple2 BinInt
  BinInt
d_C__casept_16 x2 x8 x1 x6 x11 cd cs = case x11 of
  OP_Tuple2 x9 x10 -> d_C__casept_15 x10 x6 x9 cd cs
  Choice_OP_Tuple2 d i l r -> narrow d i (d_C__casept_16 x2 x8 x1 x6
    l cd cs) (d_C__casept_16 x2 x8 x1 x6 r cd cs)
  Choices_OP_Tuple2 d i xs -> narrows cs d i (\z -> d_C__casept_16
    x2 x8 x1 x6 z cd cs) xs
  Guard_OP_Tuple2 d c e -> guardCons d c (d_C__casept_16 x2 x8 x1 x6
    e cd $! addCs c cs)
  Fail_OP_Tuple2 d info -> failCons d (traceFail "Prelude._casept_16"
    [show x2, show x8, show x1, show x6, show x11] info)
  _ -> failCons cd (consFail "Prelude._casept_16" (showCons x11))

d_C__casept_15 :: BinInt -> Nat -> BinInt -> Cover -> ConstStore
  -> OP_Tuple2 BinInt BinInt
d_C__casept_15 x10 x6 x9 cd cs = case x9 of
  Neg x11 -> d_C_error (toCurryString
    "quotRemNat: negative quotient") cd cs
  Zero -> OP_Tuple2 (Pos (O x6)) x10
  Pos x12 -> OP_Tuple2 (Pos (d_OP_plus_caret (O x6) x12 cd
    cs)) x10
  Choice_BinInt d i l r -> narrow d i (d_C__casept_15 x10 x6 l cd cs)
    (d_C__casept_15 x10 x6 r cd cs)
  Choices_BinInt d i xs -> narrows cs d i (\z -> d_C__casept_15 x10 x6 z cd cs)
    xs
  Guard_BinInt d c e -> guardCons d c (d_C__casept_15 x10 x6 e cd $! addCs c cs)
  Fail_BinInt d info -> failCons d (traceFail "Prelude._casept_15" [show x10, show
    x6, show x9] info)
  _ -> failCons cd (consFail "Prelude._casept_15" (showCons x9))

d_C__casept_24 :: Nat -> Nat -> Cover -> ConstStore -> BinInt
d_C__casept_24 x6 x2 cd cs = case x2 of
  IHi -> Pos (O x6)
  O x7 -> d_C_inc (d_C_mult2 (d_OP_minus_caret x6 x7 cd cs) cd cs) cd cs
  I x8 -> d_C_mult2 (d_OP_minus_caret x6 x8 cd cs) cd cs
  Choice_Nat d i l r -> narrow d i (d_C__casept_24 x6 l cd cs) (d_C__casept_24 x6
    r cd cs)
  Choices_Nat d i xs -> narrows cs d i (\z -> d_C__casept_24 x6 z cd cs) xs
  Guard_Nat d c e -> guardCons d c (d_C__casept_24 x6 e cd $! addCs c cs)
  Fail_Nat d info -> failCons d (traceFail "Prelude._casept_24" [show x6, show x2]
    info)
  _ -> failCons cd (consFail "Prelude._casept_24" (showCons x2))

d_C__casept_25 :: Nat -> Nat -> Nat -> Cover -> ConstStore -> BinInt
d_C__casept_25 x3 x1 x2 cd cs = case x2 of
  IHi -> Pos (d_C_predNat x1 cd cs)
  O x4 -> d_C_mult2 (d_OP_minus_caret x3 x4 cd cs) cd cs
  I x5 -> d_C_dec (d_C_mult2 (d_OP_minus_caret x3 x5 cd cs) cd cs) cd cs
  Choice_Nat d i l r -> narrow d i (d_C__casept_25 x3 x1 l cd cs) (d_C__casept_25
    x3 x1 r cd cs)
  Choices_Nat d i xs -> narrows cs d i (\z -> d_C__casept_25 x3 x1 z cd cs) xs
  Guard_Nat d c e -> guardCons d c (d_C__casept_25 x3 x1 e cd $! addCs c cs)
  Fail_Nat d info -> failCons d (traceFail "Prelude._casept_25" [show x3, show x1
    , show x2] info)
  _ -> failCons cd (consFail "Prelude._casept_25" (showCons x2))

d_C__casept_26 :: Nat -> Nat -> Cover -> ConstStore -> Nat
d_C__casept_26 x6 x2 cd cs = case x2 of
  IHi -> O (d_C_succNat x6 cd cs)
  O x7 -> I (d_OP_plus_caret x6 x7 cd cs)
  I x8 -> O (d_OP_plus_caret (d_C_succNat x6 cd cs) x8 cd cs)
  Choice_Nat d i l r -> narrow d i (d_C__casept_26 x6 l cd cs) (d_C__casept_26 x6
    r cd cs)
  Choices_Nat d i xs -> narrows cs d i (\z -> d_C__casept_26 x6 z cd cs) xs
  Guard_Nat d c e -> guardCons d c (d_C__casept_26 x6 e cd $! addCs c cs)
  Fail_Nat d info -> failCons d (traceFail "Prelude._casept_26" [show x6, show x2]
    info)
  _ -> failCons cd (consFail "Prelude._casept_26" (showCons x2))

d_C__casept_27 :: Nat -> Nat -> Cover -> ConstStore -> Nat
d_C__casept_27 x3 x2 cd cs = case x2 of
  IHi -> I x3
  O x4 -> O (d_OP_plus_caret x3 x4 cd cs)
  I x5 -> I (d_OP_plus_caret x3 x5 cd cs)
  Choice_Nat d i l r -> narrow d i (d_C__casept_27 x3 l cd cs) (d_C__casept_27 x3
    r cd cs)
  Choices_Nat d i xs -> narrows cs d i (\z -> d_C__casept_27 x3 z cd cs) xs
  Guard_Nat d c e -> guardCons d c (d_C__casept_27 x3 e cd $! addCs c cs)
  Fail_Nat d info -> failCons d (traceFail "Prelude._casept_27" [show x3, show x2]
    info)
  _ -> failCons cd (consFail "Prelude._casept_27" (showCons x2))

d_C__casept_28 :: Nat -> Cover -> ConstStore -> Nat
d_C__casept_28 x2 cd cs = case x2 of
  IHi -> IHi
  O x3 -> I (d_C_predNat x2 cd cs)
  I x4 -> I (O x4)
  Choice_Nat d i l r -> narrow d i (d_C__casept_28 l cd cs) (d_C__casept_28 r cd
    cs)
  Choices_Nat d i xs -> narrows cs d i (\z -> d_C__casept_28 z cd cs) xs
  Guard_Nat d c e -> guardCons d c (d_C__casept_28 e cd $! addCs c cs)
  Fail_Nat d info -> failCons d (traceFail "Prelude._casept_28" [show x2] info)
  _ -> failCons cd (consFail "Prelude._casept_28" (showCons x2))

d_C__casept_30 :: Nat -> Nat -> Cover -> ConstStore -> C_Ordering
d_C__casept_30 x9 x2 cd cs = case x2 of
  IHi -> C_GT
  O x10 -> let x11 = d_C_cmpNat x9 x10 cd cs in d_C__casept_29 x11 cd cs
  I x12 -> d_C_cmpNat x9 x12 cd cs
  Choice_Nat d i l r -> narrow d i (d_C__casept_30 x9 l cd cs) (d_C__casept_30 x9
    r cd cs)
  Choices_Nat d i xs -> narrows cs d i (\z -> d_C__casept_30 x9 z cd cs) xs
  Guard_Nat d c e -> guardCons d c (d_C__casept_30 x9 e cd $! addCs c cs)
  Fail_Nat d info -> failCons d (traceFail "Prelude._casept_30" [show x9, show x2]
    info)
  _ -> failCons cd (consFail "Prelude._casept_30" (showCons x2))

d_C__casept_29 :: C_Ordering -> Cover -> ConstStore -> C_Ordering
d_C__casept_29 x11 cd cs = case x11 of
  C_EQ -> C_GT
  C_LT -> x11
  C_GT -> x11
  Choice_C_Ordering d i l r -> narrow d i (d_C__casept_29 l cd cs)
    (d_C__casept_29 r cd cs)
  Choices_C_Ordering d i xs -> narrows cs d i (\z -> d_C__casept_29
    z cd cs) xs
  Guard_C_Ordering d c e -> guardCons d c (d_C__casept_29 e cd $!
    addCs c cs)
  Fail_C_Ordering d info -> failCons d (traceFail "Prelude._casept_29"
    [show x11] info)
  _ -> failCons cd (consFail "Prelude._casept_29" (showCons x11))

d_C__casept_32 :: Nat -> Nat -> Cover -> ConstStore -> C_Ordering
d_C__casept_32 x5 x2 cd cs = case x2 of
  IHi -> C_GT
  O x6 -> d_C_cmpNat x5 x6 cd cs
  I x7 -> let x8 = d_C_cmpNat x5 x7 cd cs in d_C__casept_31 x8 cd cs
  Choice_Nat d i l r -> narrow d i (d_C__casept_32 x5 l cd cs) (d_C__casept_32 x5
    r cd cs)
  Choices_Nat d i xs -> narrows cs d i (\z -> d_C__casept_32 x5 z cd cs) xs
  Guard_Nat d c e -> guardCons d c (d_C__casept_32 x5 e cd $! addCs c cs)
  Fail_Nat d info -> failCons d (traceFail "Prelude._casept_32" [show x5, show x2]
    info)
  _ -> failCons cd (consFail "Prelude._casept_32" (showCons x2))

d_C__casept_31 :: C_Ordering -> Cover -> ConstStore -> C_Ordering
d_C__casept_31 x8 cd cs = case x8 of
  C_EQ -> C_LT
  C_LT -> x8
  C_GT -> x8
  Choice_C_Ordering d i l r -> narrow d i (d_C__casept_31 l cd cs)
    (d_C__casept_31 r cd cs)
  Choices_C_Ordering d i xs -> narrows cs d i (\z -> d_C__casept_31
    z cd cs) xs
  Guard_C_Ordering d c e -> guardCons d c (d_C__casept_31 e cd $!
    addCs c cs)
  Fail_C_Ordering d info -> failCons d (traceFail "Prelude._casept_31"
    [show x8] info)
  _ -> failCons cd (consFail "Prelude._casept_31" (showCons x8))

d_C__casept_33 :: Nat -> Cover -> ConstStore -> C_Ordering
d_C__casept_33 x2 cd cs = case x2 of
  IHi -> C_EQ
  O x3 -> C_LT
  I x4 -> C_LT
  Choice_Nat d i l r -> narrow d i (d_C__casept_33 l cd cs) (d_C__casept_33 r cd
    cs)
  Choices_Nat d i xs -> narrows cs d i (\z -> d_C__casept_33 z cd cs) xs
  Guard_Nat d c e -> guardCons d c (d_C__casept_33 e cd $! addCs c cs)
  Fail_Nat d info -> failCons d (traceFail "Prelude._casept_33" [show x2] info)
  _ -> failCons cd (consFail "Prelude._casept_33" (showCons x2))
