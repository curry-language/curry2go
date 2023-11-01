{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
module Prelude (Apply_Det, Apply_ND) where
import qualified Prelude as P
import qualified Control.Monad as P
import qualified Control.Monad.State as P
import qualified Control.Exception as P
import qualified Data.List as P
import qualified GHC.IO.Exception as P
import qualified GHC.Magic as P
import qualified System.IO.Unsafe as P
import qualified Data.SBV as SBV
import BasicDefinitions
import Prelude ((.), ($), ($!), (+), (-), (*), (/), (==), (<=),(>>=))

-- -----------------------------------------------------------------------------
-- higher-order representation
-- -----------------------------------------------------------------------------

type Apply_Det (a :: k1 -> k2) (b :: k1) = (a b)

type Apply_ND (a :: k1 -> k2) (b :: k1) = (a b)

apply_Det# :: (a -> b) -> a -> b
apply_Det# = ($)

apply_ND# :: Curry (LiftedFunc (LiftedFunc a b) (LiftedFunc a b))
apply_ND# = returnFunc (returnFunc . app)

-- -----------------------------------------------------------------------------
-- Int representation
-- -----------------------------------------------------------------------------

type Int_Det# = P.Integer

type Int_ND# = P.Integer

eqInt_Det# :: Int_Det -> Int_Det -> Bool_Det
eqInt_Det# = liftForeign2 (==)

eqInt_ND# :: Curry (LiftedFunc Int_ND (LiftedFunc Int_ND Bool_ND))
eqInt_ND# = primitive2Bool (SBV..==) (==)

ltEqInt_Det# :: Int_Det -> Int_Det -> Bool_Det
ltEqInt_Det# = liftForeign2 (<=)

ltEqInt_ND# :: Curry (LiftedFunc Int_ND (LiftedFunc Int_ND Bool_ND))
ltEqInt_ND# = primitive2Bool (SBV..<=) (<=)

-- -----------------------------------------------------------------------------
-- Float representation
-- -----------------------------------------------------------------------------

type Float_Det# = P.Double

type Float_ND# = P.Double

eqFloat_Det# :: Float_Det -> Float_Det -> Bool_Det
eqFloat_Det# = liftForeign2 (==)

eqFloat_ND# :: Curry (LiftedFunc Float_ND (LiftedFunc Float_ND Bool_ND))
eqFloat_ND# = primitive2Bool (SBV..==) (==)

ltEqFloat_Det# :: Float_Det -> Float_Det -> Bool_Det
ltEqFloat_Det# = liftForeign2 (<=)

ltEqFloat_ND# :: Curry (LiftedFunc Float_ND (LiftedFunc Float_ND Bool_ND))
ltEqFloat_ND# = primitive2Bool (SBV..<=) (<=)

-- ---------------------------------------------------------------------------
-- Char representation
-- ---------------------------------------------------------------------------

type Char_Det# = P.Char

type Char_ND# = P.Char

eqChar_Det# :: Char_Det -> Char_Det -> Bool_Det
eqChar_Det# = liftForeign2 (==)

eqChar_ND# :: Curry (LiftedFunc Char_ND (LiftedFunc Char_ND Bool_ND))
eqChar_ND# = primitive2Bool (SBV..==) (==)

ltEqChar_Det# :: Char_Det -> Char_Det -> Bool_Det
ltEqChar_Det# = liftForeign2 (<=)

ltEqChar_ND# :: Curry (LiftedFunc Char_ND (LiftedFunc Char_ND Bool_ND))
ltEqChar_ND# = primitive2Bool (SBV..<=) (<=)

-- ---------------------------------------------------------------------------
-- IO representation
-- ---------------------------------------------------------------------------

type IO_Det# = P.IO

type IO_ND# = P.IO

-- ---------------------------------------------------------------------------
-- Function representation
-- ---------------------------------------------------------------------------

type CArrow_Det# = (->)

type CArrow_ND# = BasicDefinitions.LiftedFunc

-- -----------------------------------------------------------------------------
-- ShowFree for Lists, Tuples and Strings (overlaps!)
-- -----------------------------------------------------------------------------

data ListInfo = FullList | FreeElem | FreeList
  deriving (P.Eq, P.Ord)

instance Curryable a => ShowFree (CList_ND a) where
  showsFreePrec _ CList_ND s = BasicDefinitions.showsStringCurry "[]" s
  showsFreePrec _ xs       s = (P.fst s,) $ do
    (ys, b) <- gatherContents (`BasicDefinitions.showFree` P.fst s) (`BasicDefinitions.showFree` P.fst s) P.id xs
    case b of
      FullList -> P.snd $ BasicDefinitions.showsStringCurry ("["  P.++ P.intercalate "," ys P.++ "]") s
      FreeElem -> P.snd $ BasicDefinitions.showsStringCurry ("["  P.++ P.intercalate "," ys P.++ "]") s
      FreeList -> P.snd $ BasicDefinitions.showsStringCurry (          P.intercalate ":" ys         ) s

instance {-# OVERLAPS #-} ShowFree (CList_ND Char_ND) where
  showsFreePrec _ CList_ND s = BasicDefinitions.showsStringCurry "\"\"" s
  showsFreePrec _ xs       s = (P.fst s,) $ do
    (ys, b) <- gatherContents (\c -> P.return [c]) (P.return . P.show) (\s -> "'" P.++ P.show (P.head s) P.++ "'") xs
    case b of
      FullList -> P.snd $ BasicDefinitions.showsStringCurry ("\"" P.++ P.concat ys          P.++ "\"") s
      FreeElem -> P.snd $ BasicDefinitions.showsStringCurry ("["  P.++ P.intercalate "," ys P.++ "]" ) s
      FreeList -> P.snd $ BasicDefinitions.showsStringCurry (          P.intercalate ":" ys          ) s

-- first arg: use this to show the element if it is a full list
-- second arg: use this to show the element if there is a free element/list
-- third arg: use this to convert an arg produced by the first arg to one by the second arg.
gatherContents :: Curryable a => (a -> Curry P.String) -> (a -> Curry P.String) -> (P.String -> P.String)
               -> CList_ND a -> BasicDefinitions.Curry ([P.String], ListInfo)
gatherContents _ _ _ CList_ND = P.return ([], FullList)
gatherContents _ _ _ (CListFlat# CList_Det) = P.return ([], FullList)
gatherContents f g h (CListFlat# (CCons_Det x xs)) = gatherContents f g h (CCons_ND (fromHaskell x) (fromHaskell xs))
gatherContents f g h (CCons_ND x xs) = BasicDefinitions.Curry $ do
  c <- deref x
  rest <- deref xs
  unCurry (case rest of
    BasicDefinitions.Var _ i -> case c of
      BasicDefinitions.Var _ j ->
        P.return (["_" P.++ P.show j, "_" P.++ P.show i], FreeList)
      BasicDefinitions.Val u -> do
        e <- g u
        P.return ([e, "_" P.++ P.show i], FreeList)
    BasicDefinitions.Val v -> do
      (ys, b) <- gatherContents f g h v
      case c of
        BasicDefinitions.Var _ j -> case b of
          FullList -> do
            let ys' = P.fmap h ys
            P.return (("_" P.++ P.show j) : ys', FreeElem)
          _        ->
            P.return (("_" P.++ P.show j) : ys , b)
        BasicDefinitions.Val u -> case b of
          FullList -> do
            e <- f u
            P.return (e : ys, b)
          _        -> do
            e <- g u
            P.return (e : ys, b))

instance UnitDispatchable CUnit_ND where
  unitDispatch = IsUnit

instance UnitDispatchable CUnit_Det where
  unitDispatch = IsUnit

instance ShowFree CUnit_ND where
  showsFreePrec _ CUnit_ND = BasicDefinitions.showsStringCurry "()"
  showsFreePrec _ (CUnitFlat# CUnit_Det) = BasicDefinitions.showsStringCurry "()"

instance (Curryable x, Curryable y) => ShowFree (CTuple2_ND x y) where
  showsFreePrec _ (CTuple2_ND x y) =
    showsStringCurry ")" .
    showsFreePrecCurry 0 y .
    showsStringCurry "," .
    showsFreePrecCurry 0 x .
    showsStringCurry "("
  showsFreePrec p' (CTuple2Flat# (CTuple2_Det x y)) = showsFreePrec p' (CTuple2_ND (fromHaskell x) (fromHaskell y))

instance (Curryable x, Curryable y, Curryable z) => ShowFree (CTuple3_ND x y z) where
  showsFreePrec _ (CTuple3_ND x y z) =
    showsStringCurry ")" .
    showsFreePrecCurry 0 z .
    showsStringCurry "," .
    showsFreePrecCurry 0 y .
    showsStringCurry "," .
    showsFreePrecCurry 0 x .
    showsStringCurry "("
  showsFreePrec p' (CTuple3Flat# (CTuple3_Det x y z)) = showsFreePrec p' (CTuple3_ND (fromHaskell x) (fromHaskell y) (fromHaskell z))

instance (Curryable x, Curryable y, Curryable z, Curryable w) => ShowFree (CTuple4_ND x y z w) where
  showsFreePrec _ (CTuple4_ND x y z w) =
    showsStringCurry ")" .
    showsFreePrecCurry 0 w .
    showsStringCurry "," .
    showsFreePrecCurry 0 z .
    showsStringCurry "," .
    showsFreePrecCurry 0 y .
    showsStringCurry "," .
    showsFreePrecCurry 0 x .
    showsStringCurry "("
  showsFreePrec p' (CTuple4Flat# (CTuple4_Det x y z w)) = showsFreePrec p' (CTuple4_ND (fromHaskell x) (fromHaskell y) (fromHaskell z) (fromHaskell w))

instance (Curryable x, Curryable y, Curryable z, Curryable w, Curryable v) => ShowFree (CTuple5_ND x y z w v) where
  showsFreePrec _ (CTuple5_ND x y z w t) =
    showsStringCurry ")" .
    showsFreePrecCurry 0 t .
    showsStringCurry "," .
    showsFreePrecCurry 0 w .
    showsStringCurry "," .
    showsFreePrecCurry 0 z .
    showsStringCurry "," .
    showsFreePrecCurry 0 y .
    showsStringCurry "," .
    showsFreePrecCurry 0 x .
    showsStringCurry "("
  showsFreePrec p' (CTuple5Flat# (CTuple5_Det x y z w t)) = showsFreePrec p' (CTuple5_ND (fromHaskell x) (fromHaskell y) (fromHaskell z) (fromHaskell w) (fromHaskell t))

instance (Curryable x, Curryable y, Curryable z, Curryable w, Curryable v, Curryable u) => ShowFree (CTuple6_ND x y z w v u) where
  showsFreePrec _ (CTuple6_ND x y z w t s) =
    showsStringCurry ")" .
    showsFreePrecCurry 0 s .
    showsStringCurry "," .
    showsFreePrecCurry 0 t .
    showsStringCurry "," .
    showsFreePrecCurry 0 w .
    showsStringCurry "," .
    showsFreePrecCurry 0 z .
    showsStringCurry "," .
    showsFreePrecCurry 0 y .
    showsStringCurry "," .
    showsFreePrecCurry 0 x .
    showsStringCurry "("
  showsFreePrec p' (CTuple6Flat# (CTuple6_Det x y z w t s)) = showsFreePrec p' (CTuple6_ND (fromHaskell x) (fromHaskell y) (fromHaskell z) (fromHaskell w) (fromHaskell t) (fromHaskell s))

instance (Curryable x, Curryable y, Curryable z, Curryable w, Curryable v, Curryable u, Curryable t) => ShowFree (CTuple7_ND x y z w v u t) where
  showsFreePrec _ (CTuple7_ND x y z w t s r) =
    showsStringCurry ")" .
    showsFreePrecCurry 0 r .
    showsStringCurry "," .
    showsFreePrecCurry 0 s .
    showsStringCurry "," .
    showsFreePrecCurry 0 t .
    showsStringCurry "," .
    showsFreePrecCurry 0 w .
    showsStringCurry "," .
    showsFreePrecCurry 0 z .
    showsStringCurry "," .
    showsFreePrecCurry 0 y .
    showsStringCurry "," .
    showsFreePrecCurry 0 x .
    showsStringCurry "("
  showsFreePrec p' (CTuple7Flat# (CTuple7_Det x y z w t s r)) = showsFreePrec p' (CTuple7_ND (fromHaskell x) (fromHaskell y) (fromHaskell z) (fromHaskell w) (fromHaskell t) (fromHaskell s) (fromHaskell r))

instance (Curryable x, Curryable y, Curryable z, Curryable w, Curryable v, Curryable u, Curryable t, Curryable s) => ShowFree (CTuple8_ND x y z w v u t s) where
  showsFreePrec _ (CTuple8_ND x y z w t s r q) =
    showsStringCurry ")" .
    showsFreePrecCurry 0 q .
    showsStringCurry "," .
    showsFreePrecCurry 0 r .
    showsStringCurry "," .
    showsFreePrecCurry 0 s .
    showsStringCurry "," .
    showsFreePrecCurry 0 t .
    showsStringCurry "," .
    showsFreePrecCurry 0 w .
    showsStringCurry "," .
    showsFreePrecCurry 0 z .
    showsStringCurry "," .
    showsFreePrecCurry 0 y .
    showsStringCurry "," .
    showsFreePrecCurry 0 x .
    showsStringCurry "("
  showsFreePrec p' (CTuple8Flat# (CTuple8_Det x y z w t s r q)) = showsFreePrec p' (CTuple8_ND (fromHaskell x) (fromHaskell y) (fromHaskell z) (fromHaskell w) (fromHaskell t) (fromHaskell s) (fromHaskell r) (fromHaskell q))

instance (Curryable x, Curryable y, Curryable z, Curryable w, Curryable v, Curryable u, Curryable t, Curryable s, Curryable r) => ShowFree (CTuple9_ND x y z w v u t s r) where
  showsFreePrec _ (CTuple9_ND x y z w t s r q p) =
    showsStringCurry ")" .
    showsFreePrecCurry 0 p .
    showsStringCurry "," .
    showsFreePrecCurry 0 q .
    showsStringCurry "," .
    showsFreePrecCurry 0 r .
    showsStringCurry "," .
    showsFreePrecCurry 0 s .
    showsStringCurry "," .
    showsFreePrecCurry 0 t .
    showsStringCurry "," .
    showsFreePrecCurry 0 w .
    showsStringCurry "," .
    showsFreePrecCurry 0 z .
    showsStringCurry "," .
    showsFreePrecCurry 0 y .
    showsStringCurry "," .
    showsFreePrecCurry 0 x .
    showsStringCurry "("
  showsFreePrec p' (CTuple9Flat# (CTuple9_Det x y z w t s r q p)) = showsFreePrec p' (CTuple9_ND (fromHaskell x) (fromHaskell y) (fromHaskell z) (fromHaskell w) (fromHaskell t) (fromHaskell s) (fromHaskell r) (fromHaskell q) (fromHaskell p))

instance (Curryable x, Curryable y, Curryable z, Curryable w, Curryable v, Curryable u, Curryable t, Curryable s, Curryable r, Curryable q) => ShowFree (CTuple10_ND x y z w v u t s r q) where
  showsFreePrec _ (CTuple10_ND x y z w t s r q p o) =
    showsStringCurry ")" .
    showsFreePrecCurry 0 o .
    showsStringCurry "," .
    showsFreePrecCurry 0 p .
    showsStringCurry "," .
    showsFreePrecCurry 0 q .
    showsStringCurry "," .
    showsFreePrecCurry 0 r .
    showsStringCurry "," .
    showsFreePrecCurry 0 s .
    showsStringCurry "," .
    showsFreePrecCurry 0 t .
    showsStringCurry "," .
    showsFreePrecCurry 0 w .
    showsStringCurry "," .
    showsFreePrecCurry 0 z .
    showsStringCurry "," .
    showsFreePrecCurry 0 y .
    showsStringCurry "," .
    showsFreePrecCurry 0 x .
    showsStringCurry "("
  showsFreePrec p' (CTuple10Flat# (CTuple10_Det x y z w t s r q p o)) = showsFreePrec p' (CTuple10_ND (fromHaskell x) (fromHaskell y) (fromHaskell z) (fromHaskell w) (fromHaskell t) (fromHaskell s) (fromHaskell r) (fromHaskell q) (fromHaskell p) (fromHaskell o))

instance (Curryable x, Curryable y, Curryable z, Curryable w, Curryable v, Curryable u, Curryable t, Curryable s, Curryable r, Curryable q, Curryable p) => ShowFree (CTuple11_ND x y z w v u t s r q p) where
  showsFreePrec _ (CTuple11_ND x y z w t s r q p o n) =
    showsStringCurry ")" .
    showsFreePrecCurry 0 n .
    showsStringCurry "," .
    showsFreePrecCurry 0 o .
    showsStringCurry "," .
    showsFreePrecCurry 0 p .
    showsStringCurry "," .
    showsFreePrecCurry 0 q .
    showsStringCurry "," .
    showsFreePrecCurry 0 r .
    showsStringCurry "," .
    showsFreePrecCurry 0 s .
    showsStringCurry "," .
    showsFreePrecCurry 0 t .
    showsStringCurry "," .
    showsFreePrecCurry 0 w .
    showsStringCurry "," .
    showsFreePrecCurry 0 z .
    showsStringCurry "," .
    showsFreePrecCurry 0 y .
    showsStringCurry "," .
    showsFreePrecCurry 0 x .
    showsStringCurry "("
  showsFreePrec p' (CTuple11Flat# (CTuple11_Det x y z w t s r q p o n)) = showsFreePrec p' (CTuple11_ND (fromHaskell x) (fromHaskell y) (fromHaskell z) (fromHaskell w) (fromHaskell t) (fromHaskell s) (fromHaskell r) (fromHaskell q) (fromHaskell p) (fromHaskell o) (fromHaskell n))

instance (Curryable x, Curryable y, Curryable z, Curryable w, Curryable v, Curryable u, Curryable t, Curryable s, Curryable r, Curryable q, Curryable p, Curryable o) => ShowFree (CTuple12_ND x y z w v u t s r q p o) where
  showsFreePrec _ (CTuple12_ND x y z w t s r q p o n m) =
    showsStringCurry ")" .
    showsFreePrecCurry 0 m .
    showsStringCurry "," .
    showsFreePrecCurry 0 n .
    showsStringCurry "," .
    showsFreePrecCurry 0 o .
    showsStringCurry "," .
    showsFreePrecCurry 0 p .
    showsStringCurry "," .
    showsFreePrecCurry 0 q .
    showsStringCurry "," .
    showsFreePrecCurry 0 r .
    showsStringCurry "," .
    showsFreePrecCurry 0 s .
    showsStringCurry "," .
    showsFreePrecCurry 0 t .
    showsStringCurry "," .
    showsFreePrecCurry 0 w .
    showsStringCurry "," .
    showsFreePrecCurry 0 z .
    showsStringCurry "," .
    showsFreePrecCurry 0 y .
    showsStringCurry "," .
    showsFreePrecCurry 0 x .
    showsStringCurry "("
  showsFreePrec p' (CTuple12Flat# (CTuple12_Det x y z w t s r q p o n m)) = showsFreePrec p' (CTuple12_ND (fromHaskell x) (fromHaskell y) (fromHaskell z) (fromHaskell w) (fromHaskell t) (fromHaskell s) (fromHaskell r) (fromHaskell q) (fromHaskell p) (fromHaskell o) (fromHaskell n) (fromHaskell m))

instance (Curryable x, Curryable y, Curryable z, Curryable w, Curryable v, Curryable u, Curryable t, Curryable s, Curryable r, Curryable q, Curryable p, Curryable o, Curryable n) => ShowFree (CTuple13_ND x y z w v u t s r q p o n) where
  showsFreePrec _ (CTuple13_ND x y z w t s r q p o n m l) =
    showsStringCurry ")" .
    showsFreePrecCurry 0 l .
    showsStringCurry "," .
    showsFreePrecCurry 0 m .
    showsStringCurry "," .
    showsFreePrecCurry 0 n .
    showsStringCurry "," .
    showsFreePrecCurry 0 o .
    showsStringCurry "," .
    showsFreePrecCurry 0 p .
    showsStringCurry "," .
    showsFreePrecCurry 0 q .
    showsStringCurry "," .
    showsFreePrecCurry 0 r .
    showsStringCurry "," .
    showsFreePrecCurry 0 s .
    showsStringCurry "," .
    showsFreePrecCurry 0 t .
    showsStringCurry "," .
    showsFreePrecCurry 0 w .
    showsStringCurry "," .
    showsFreePrecCurry 0 z .
    showsStringCurry "," .
    showsFreePrecCurry 0 y .
    showsStringCurry "," .
    showsFreePrecCurry 0 x .
    showsStringCurry "("
  showsFreePrec p' (CTuple13Flat# (CTuple13_Det x y z w t s r q p o n m l)) = showsFreePrec p' (CTuple13_ND (fromHaskell x) (fromHaskell y) (fromHaskell z) (fromHaskell w) (fromHaskell t) (fromHaskell s) (fromHaskell r) (fromHaskell q) (fromHaskell p) (fromHaskell o) (fromHaskell n) (fromHaskell m) (fromHaskell l))

instance (Curryable x, Curryable y, Curryable z, Curryable w, Curryable v, Curryable u, Curryable t, Curryable s, Curryable r, Curryable q, Curryable p, Curryable o, Curryable n, Curryable m) => ShowFree (CTuple14_ND x y z w v u t s r q p o n m) where
  showsFreePrec _ (CTuple14_ND x y z w t s r q p o n m l k) =
    showsStringCurry ")" .
    showsFreePrecCurry 0 k .
    showsStringCurry "," .
    showsFreePrecCurry 0 l .
    showsStringCurry "," .
    showsFreePrecCurry 0 m .
    showsStringCurry "," .
    showsFreePrecCurry 0 n .
    showsStringCurry "," .
    showsFreePrecCurry 0 o .
    showsStringCurry "," .
    showsFreePrecCurry 0 p .
    showsStringCurry "," .
    showsFreePrecCurry 0 q .
    showsStringCurry "," .
    showsFreePrecCurry 0 r .
    showsStringCurry "," .
    showsFreePrecCurry 0 s .
    showsStringCurry "," .
    showsFreePrecCurry 0 t .
    showsStringCurry "," .
    showsFreePrecCurry 0 w .
    showsStringCurry "," .
    showsFreePrecCurry 0 z .
    showsStringCurry "," .
    showsFreePrecCurry 0 y .
    showsStringCurry "," .
    showsFreePrecCurry 0 x .
    showsStringCurry "("
  showsFreePrec p' (CTuple14Flat# (CTuple14_Det x y z w t s r q p o n m l k)) = showsFreePrec p' (CTuple14_ND (fromHaskell x) (fromHaskell y) (fromHaskell z) (fromHaskell w) (fromHaskell t) (fromHaskell s) (fromHaskell r) (fromHaskell q) (fromHaskell p) (fromHaskell o) (fromHaskell n) (fromHaskell m) (fromHaskell l) (fromHaskell k))

instance (Curryable x, Curryable y, Curryable z, Curryable w, Curryable v, Curryable u, Curryable t, Curryable s, Curryable r, Curryable q, Curryable p, Curryable o, Curryable n, Curryable m, Curryable l) => ShowFree (CTuple15_ND x y z w v u t s r q p o n m l) where
  showsFreePrec _ (CTuple15_ND x y z w t s r q p o n m l k j) =
    showsStringCurry ")" .
    showsFreePrecCurry 0 j .
    showsStringCurry "," .
    showsFreePrecCurry 0 k .
    showsStringCurry "," .
    showsFreePrecCurry 0 l .
    showsStringCurry "," .
    showsFreePrecCurry 0 m .
    showsStringCurry "," .
    showsFreePrecCurry 0 n .
    showsStringCurry "," .
    showsFreePrecCurry 0 o .
    showsStringCurry "," .
    showsFreePrecCurry 0 p .
    showsStringCurry "," .
    showsFreePrecCurry 0 q .
    showsStringCurry "," .
    showsFreePrecCurry 0 r .
    showsStringCurry "," .
    showsFreePrecCurry 0 s .
    showsStringCurry "," .
    showsFreePrecCurry 0 t .
    showsStringCurry "," .
    showsFreePrecCurry 0 w .
    showsStringCurry "," .
    showsFreePrecCurry 0 z .
    showsStringCurry "," .
    showsFreePrecCurry 0 y .
    showsStringCurry "," .
    showsFreePrecCurry 0 x .
    showsStringCurry "("
  showsFreePrec p' (CTuple15Flat# (CTuple15_Det x y z w t s r q p o n m l k j)) = showsFreePrec p' (CTuple15_ND (fromHaskell x) (fromHaskell y) (fromHaskell z) (fromHaskell w) (fromHaskell t) (fromHaskell s) (fromHaskell r) (fromHaskell q) (fromHaskell p) (fromHaskell o) (fromHaskell n) (fromHaskell m) (fromHaskell l) (fromHaskell k) (fromHaskell j))

-- -----------------------------------------------------------------------------
-- Foreign Conversion
-- -----------------------------------------------------------------------------

instance ForeignType a => ForeignType (CList_Det a) where
  type Foreign (CList_Det a) = [Foreign a]
  toForeign CList_Det = []
  toForeign (CCons_Det x xs) = toForeign x : toForeign xs
  fromForeign [] = CList_Det
  fromForeign (x:xs) = CCons_Det (fromForeign x) (fromForeign xs)

instance (ForeignType a, ForeignType b) => ForeignType (CTuple2_Det a b) where
  type Foreign (CTuple2_Det a b) = (Foreign a, Foreign b)
  toForeign (CTuple2_Det a b) = (toForeign a, toForeign b)
  fromForeign (a, b) = CTuple2_Det (fromForeign a) (fromForeign b)

instance ForeignType Bool_Det where
  type Foreign Bool_Det = P.Bool
  toForeign False_Det = P.False
  toForeign True_Det = P.True
  fromForeign P.False = False_Det
  fromForeign P.True = True_Det

instance ForeignType CUnit_Det where
  type Foreign CUnit_Det = ()
  toForeign CUnit_Det = ()
  fromForeign () = CUnit_Det

-- -----------------------------------------------------------------------------
-- Primitive operations: General
-- -----------------------------------------------------------------------------

amp_Det# :: Bool_Det -> Bool_Det -> Bool_Det
amp_Det# True_Det  True_Det  = True_Det
amp_Det# False_Det False_Det = False_Det
amp_Det# _         _         = False_Det

amp_ND# :: Curry (LiftedFunc Bool_ND (LiftedFunc Bool_ND Bool_ND))
amp_ND# = BasicDefinitions.liftConvert2 amp_Det#

eqcolonlteq_ND# :: Curryable a => Curry (LiftedFunc a (LiftedFunc a Bool_ND))
eqcolonlteq_ND# = BasicDefinitions.returnFunc (\a1 -> BasicDefinitions.returnFunc
  (BasicDefinitions.unifyL a1 P.>=> (BasicDefinitions.fromHaskell . fromForeign)))

eqcoloneq_ND# :: Curryable a => Curry (LiftedFunc a (LiftedFunc a Bool_ND))
eqcoloneq_ND# = BasicDefinitions.returnFunc (\a1 -> BasicDefinitions.returnFunc
  (BasicDefinitions.unify a1 P.>=> (BasicDefinitions.fromHaskell . fromForeign)))

cond_Det# :: Bool_Det -> a -> a
cond_Det# True_Det a = a
cond_Det# _        _ = failed_Det#

cond_ND# :: Curry (LiftedFunc Bool_ND (LiftedFunc a a))
cond_ND# = returnFunc (\a -> a >>= \case
  True_ND -> returnFunc P.id
  BoolFlat# True_Det -> returnFunc P.id
  BoolFlat# False_Det -> failed_ND
  False_ND -> failed_ND)

dollarbang_Det# :: (a -> b) -> a -> b
dollarbang_Det# = ($!)

dollarbang_ND# :: Curry (LiftedFunc (LiftedFunc a b) (LiftedFunc a b))
dollarbang_ND# = BasicDefinitions.dollarBangNDImpl

dollarbangbang_Det# :: (a -> b) -> a -> b
dollarbangbang_Det# = ($!)

dollarbangbang_ND# :: BasicDefinitions.Curryable a => Curry (LiftedFunc (LiftedFunc a b) (LiftedFunc a b))
dollarbangbang_ND# = BasicDefinitions.dollarBangBangNDImpl

dollarhashhash_Det# :: (a -> b) -> a -> b
dollarhashhash_Det# = ($!)

dollarhashhash_ND# :: BasicDefinitions.Curryable a => Curry (LiftedFunc (LiftedFunc a b) (LiftedFunc a b))
dollarhashhash_ND# = BasicDefinitions.dollarHashHashNDImpl

ensureNotFree_Det# :: a -> a
ensureNotFree_Det# !x = x

ensureNotFree_ND# :: Curry (LiftedFunc a a)
ensureNotFree_ND# = P.return (Func (\x -> x P.>>= P.noinline P.return))

-- -----------------------------------------------------------------------------
-- Primitive operations: Characters
-- -----------------------------------------------------------------------------

primuscoreord_Det# :: Char_Det -> Int_Det
primuscoreord_Det# = P.fromIntegral . P.fromEnum

primuscoreord_ND# :: Curry (LiftedFunc Char_ND Int_ND)
primuscoreord_ND# = BasicDefinitions.liftConvert1 primuscoreord_Det#

primuscorechr_Det# :: Int_Det -> Char_Det
primuscorechr_Det# = P.toEnum . P.fromIntegral

primuscorechr_ND# :: Curry (LiftedFunc Int_ND Char_ND)
primuscorechr_ND# = BasicDefinitions.liftConvert1 primuscorechr_Det#

-- -----------------------------------------------------------------------------
-- Primitive operations: Arithmetics
-- -----------------------------------------------------------------------------

primuscoreshowCharLiteral_Det# :: Char_Det -> CList_Det Char_Det
primuscoreshowCharLiteral_Det# = liftForeign1 P.show

primuscoreshowIntLiteral_Det# :: Int_Det -> CList_Det Char_Det
primuscoreshowIntLiteral_Det# = liftForeign1 P.show

primuscoreshowFloatLiteral_Det# :: Float_Det -> CList_Det Char_Det
primuscoreshowFloatLiteral_Det# = liftForeign1 P.show

primuscoreshowStringLiteral_Det# :: CList_Det Char_Det -> CList_Det Char_Det
primuscoreshowStringLiteral_Det# = liftForeign1 P.show

primuscorereadCharLiteral_Det# :: CList_Det Char_Det -> CList_Det (CTuple2_Det Char_Det (CList_Det Char_Det))
primuscorereadCharLiteral_Det# = liftForeign1 P.reads

primuscorereadStringLiteral_Det# :: CList_Det Char_Det -> CList_Det (CTuple2_Det (CList_Det Char_Det) (CList_Det Char_Det))
primuscorereadStringLiteral_Det# = liftForeign1 P.reads

primuscorereadNatLiteral_Det# :: CList_Det Char_Det -> CList_Det (CTuple2_Det Int_Det (CList_Det Char_Det))
primuscorereadNatLiteral_Det# = liftForeign1 P.reads

primuscorereadFloatLiteral_Det# :: CList_Det Char_Det -> CList_Det (CTuple2_Det Float_Det (CList_Det Char_Det))
primuscorereadFloatLiteral_Det# = liftForeign1 P.reads

plusInt_Det# :: Int_Det -> Int_Det -> Int_Det
plusInt_Det# = (+)

minusInt_Det# :: Int_Det -> Int_Det -> Int_Det
minusInt_Det# = (-)

timesInt_Det# :: Int_Det -> Int_Det -> Int_Det
timesInt_Det# = (*)

plusFloat_Det# :: Float_Det -> Float_Det -> Float_Det
plusFloat_Det# = (+)

minusFloat_Det# :: Float_Det -> Float_Det -> Float_Det
minusFloat_Det# = (-)

timesFloat_Det# :: Float_Det -> Float_Det -> Float_Det
timesFloat_Det# = (*)

negateFloat_Det# :: Float_Det -> Float_Det
negateFloat_Det# = P.negate

intToFloat_Det# :: Int_Det -> Float_Det
intToFloat_Det# = P.fromIntegral

divFloat_Det# :: Float_Det -> Float_Det -> Float_Det
divFloat_Det# = (/)

divInt_Det# :: Int_Det -> Int_Det -> Int_Det
divInt_Det# = P.div

modInt_Det# :: Int_Det -> Int_Det -> Int_Det
modInt_Det# = P.mod

quotInt_Det# :: Int_Det -> Int_Det -> Int_Det
quotInt_Det# = P.quot

remInt_Det# :: Int_Det -> Int_Det -> Int_Det
remInt_Det# = P.rem

truncateFloat_Det# :: Float_Det -> Int_Det
truncateFloat_Det# = P.truncate

roundFloat_Det# :: Float_Det -> Int_Det
roundFloat_Det# = P.round

logFloat_Det# :: Float_Det -> Float_Det
logFloat_Det# = P.log

expFloat_Det# :: Float_Det -> Float_Det
expFloat_Det# = P.exp

sqrtFloat_Det# :: Float_Det -> Float_Det
sqrtFloat_Det# = P.sqrt

sinFloat_Det# :: Float_Det -> Float_Det
sinFloat_Det# = P.sin

cosFloat_Det# :: Float_Det -> Float_Det
cosFloat_Det# = P.cos

tanFloat_Det# :: Float_Det -> Float_Det
tanFloat_Det# = P.tan

asinFloat_Det# :: Float_Det -> Float_Det
asinFloat_Det# = P.asin

acosFloat_Det# :: Float_Det -> Float_Det
acosFloat_Det# = P.acos

atanFloat_Det# :: Float_Det -> Float_Det
atanFloat_Det# = P.atan

asinhFloat_Det# :: Float_Det -> Float_Det
asinhFloat_Det# = P.asinh

acoshFloat_Det# :: Float_Det -> Float_Det
acoshFloat_Det# = P.acosh

atanhFloat_Det# :: Float_Det -> Float_Det
atanhFloat_Det# = P.atanh

sinhFloat_Det# :: Float_Det -> Float_Det
sinhFloat_Det# = P.sinh

coshFloat_Det# :: Float_Det -> Float_Det
coshFloat_Det# = P.cosh

tanhFloat_Det# :: Float_Det -> Float_Det
tanhFloat_Det# = P.tanh

primuscoreshowCharLiteral_ND# :: Curry (LiftedFunc Char_ND (CList_ND Char_ND))
primuscoreshowCharLiteral_ND# = liftConvert1 (liftForeign1 P.show)

primuscoreshowIntLiteral_ND# :: Curry (LiftedFunc Int_ND (CList_ND Char_ND))
primuscoreshowIntLiteral_ND# = liftConvert1 (liftForeign1 P.show)

primuscoreshowFloatLiteral_ND# :: Curry (LiftedFunc Float_ND (CList_ND Char_ND))
primuscoreshowFloatLiteral_ND# = liftConvert1 (liftForeign1 P.show)

primuscoreshowStringLiteral_ND# :: Curry (LiftedFunc (CList_ND Char_ND) (CList_ND Char_ND))
primuscoreshowStringLiteral_ND# = liftConvert1 (liftForeign1 P.show)

primuscorereadCharLiteral_ND# :: Curry (LiftedFunc (CList_ND Char_ND) (CList_ND (CTuple2_ND Char_ND (CList_ND Char_ND))))
primuscorereadCharLiteral_ND# = liftConvert1 (liftForeign1 P.reads)

primuscorereadStringLiteral_ND# :: Curry (LiftedFunc (CList_ND Char_ND) (CList_ND (CTuple2_ND (CList_ND Char_ND) (CList_ND Char_ND))))
primuscorereadStringLiteral_ND# = liftConvert1 (liftForeign1 P.reads)

primuscorereadNatLiteral_ND# :: Curry (LiftedFunc (CList_ND Char_ND) (CList_ND (CTuple2_ND Int_ND (CList_ND Char_ND))))
primuscorereadNatLiteral_ND# = liftConvert1 (liftForeign1 P.reads)

primuscorereadFloatLiteral_ND# :: Curry (LiftedFunc (CList_ND Char_ND) (CList_ND (CTuple2_ND Float_ND (CList_ND Char_ND))))
primuscorereadFloatLiteral_ND# = liftConvert1 (liftForeign1 P.reads)

plusInt_ND# :: Curry (LiftedFunc Int_ND (LiftedFunc Int_ND Int_ND))
plusInt_ND# = primitive2 (+) (+)

minusInt_ND# :: Curry (LiftedFunc Int_ND (LiftedFunc Int_ND Int_ND))
minusInt_ND# = primitive2 (-) (-)

timesInt_ND# :: Curry (LiftedFunc Int_ND (LiftedFunc Int_ND Int_ND))
timesInt_ND# = primitive2 (*) (*)

plusFloat_ND# :: Curry (LiftedFunc Float_ND (LiftedFunc Float_ND Float_ND))
plusFloat_ND# = primitive2 (+) (+)

minusFloat_ND# :: Curry (LiftedFunc Float_ND (LiftedFunc Float_ND Float_ND))
minusFloat_ND# = primitive2 (-) (-)

timesFloat_ND# :: Curry (LiftedFunc Float_ND (LiftedFunc Float_ND Float_ND))
timesFloat_ND# = primitive2 (*) (*)

negateFloat_ND# :: Curry (LiftedFunc Float_ND Float_ND)
negateFloat_ND# = primitive1 P.negate P.negate

intToFloat_ND# :: Curry (LiftedFunc Int_ND Float_ND)
intToFloat_ND# = primitive1 SBV.sFromIntegral P.fromIntegral

divFloat_ND# :: Curry (LiftedFunc Float_ND (LiftedFunc Float_ND Float_ND))
divFloat_ND# = primitive2 (/) (/)

divInt_ND# :: Curry (LiftedFunc Int_ND (LiftedFunc Int_ND Int_ND))
divInt_ND# = primitive2 SBV.sDiv P.div

modInt_ND# :: Curry (LiftedFunc Int_ND (LiftedFunc Int_ND Int_ND))
modInt_ND# = primitive2 SBV.sMod P.mod

quotInt_ND# :: Curry (LiftedFunc Int_ND (LiftedFunc Int_ND Int_ND))
quotInt_ND# = primitive2 SBV.sQuot P.quot

remInt_ND# :: Curry (LiftedFunc Int_ND (LiftedFunc Int_ND Int_ND))
remInt_ND# = primitive2 SBV.sRem P.rem

truncateFloat_ND# :: Curry (LiftedFunc Float_ND Int_ND)
truncateFloat_ND# = primitive1 (SBV.fromSDouble (SBV.literal SBV.RoundNearestTiesToEven)) P.truncate

roundFloat_ND# :: Curry (LiftedFunc Float_ND Int_ND)
roundFloat_ND# = primitive1 (SBV.fromSDouble (SBV.literal SBV.RoundTowardZero)) P.round

logFloat_ND# :: Curry (LiftedFunc Float_ND Float_ND)
logFloat_ND# = primitive1 P.log P.log

expFloat_ND# :: Curry (LiftedFunc Float_ND Float_ND)
expFloat_ND# = primitive1 P.exp P.exp

sqrtFloat_ND# :: Curry (LiftedFunc Float_ND Float_ND)
sqrtFloat_ND# = primitive1 P.sqrt P.sqrt

sinFloat_ND# :: Curry (LiftedFunc Float_ND Float_ND)
sinFloat_ND# = primitive1 P.sin P.sin

cosFloat_ND# :: Curry (LiftedFunc Float_ND Float_ND)
cosFloat_ND# = primitive1 P.cos P.cos

tanFloat_ND# :: Curry (LiftedFunc Float_ND Float_ND)
tanFloat_ND# = primitive1 P.tan P.tan

asinFloat_ND# :: Curry (LiftedFunc Float_ND Float_ND)
asinFloat_ND# = primitive1 P.asin P.asin

acosFloat_ND# :: Curry (LiftedFunc Float_ND Float_ND)
acosFloat_ND# = primitive1 P.acos P.acos

atanFloat_ND# :: Curry (LiftedFunc Float_ND Float_ND)
atanFloat_ND# = primitive1 P.atan P.atan

asinhFloat_ND# :: Curry (LiftedFunc Float_ND Float_ND)
asinhFloat_ND# = primitive1 P.asinh P.asinh

acoshFloat_ND# :: Curry (LiftedFunc Float_ND Float_ND)
acoshFloat_ND# = primitive1 P.acosh P.acosh

atanhFloat_ND# :: Curry (LiftedFunc Float_ND Float_ND)
atanhFloat_ND# = primitive1 P.atanh P.atanh

sinhFloat_ND# :: Curry (LiftedFunc Float_ND Float_ND)
sinhFloat_ND# = primitive1 P.sinh P.sinh

coshFloat_ND# :: Curry (LiftedFunc Float_ND Float_ND)
coshFloat_ND# = primitive1 P.cosh P.cosh

tanhFloat_ND# :: Curry (LiftedFunc Float_ND Float_ND)
tanhFloat_ND# = primitive1 P.tanh P.tanh

-- -----------------------------------------------------------------------------
-- Primitive operations: IO stuff
-- -----------------------------------------------------------------------------

bindIO_Det# :: IO_Det a -> (a -> IO_Det b) -> IO_Det b
bindIO_Det# = (>>=)

bindIO_ND# :: Curry (LiftedFunc (IO_Det a) (LiftedFunc (LiftedFunc a (IO_Det b)) (IO_Det b)))
bindIO_ND# = BasicDefinitions.bindIONDImpl

returnIO_Det# :: a -> IO_Det a
returnIO_Det# = P.pure

returnIO_ND# :: Curry (LiftedFunc a (IO_Det a))
returnIO_ND# = BasicDefinitions.returnIONDImpl

getChar_Det# :: IO_Det Char_Det
getChar_Det# = P.getChar

getChar_ND# :: Curry (IO_ND Char_ND)
getChar_ND# = P.return P.getChar

primuscoreputChar_Det# :: Char_Det -> IO_Det CUnit_Det
primuscoreputChar_Det# = liftForeign1 P.putChar

primuscoreputChar_ND# :: Curry (LiftedFunc Char_ND (IO_ND CUnit_ND))
primuscoreputChar_ND# = liftConvertIO1 primuscoreputChar_Det#

primuscorereadFile_Det# :: CList_Det Char_Det -> IO_Det (CList_Det Char_Det)
primuscorereadFile_Det# = liftForeign1 P.readFile

primuscorereadFile_ND# :: Curry (LiftedFunc (CList_ND Char_ND) (IO_ND (CList_ND Char_ND)))
primuscorereadFile_ND# = liftConvertIO1 primuscorereadFile_Det#

primuscorewriteFile_Det# :: CList_Det Char_Det -> CList_Det Char_Det -> IO_Det CUnit_Det
primuscorewriteFile_Det# = liftForeign2 P.writeFile

primuscorewriteFile_ND# :: Curry (LiftedFunc (CList_ND Char_ND) (LiftedFunc (CList_ND Char_ND) (IO_ND CUnit_ND)))
primuscorewriteFile_ND# = liftConvertIO2 primuscorewriteFile_Det#

primuscoreappendFile_Det# :: CList_Det Char_Det -> CList_Det Char_Det -> IO_Det CUnit_Det
primuscoreappendFile_Det# = liftForeign2 P.appendFile

primuscoreappendFile_ND# :: Curry (LiftedFunc (CList_ND Char_ND) (LiftedFunc (CList_ND Char_ND) (IO_ND CUnit_ND)))
primuscoreappendFile_ND# = liftConvertIO2 primuscoreappendFile_Det#

instance ForeignType IOError_Det where
  type Foreign IOError_Det = P.IOException
  fromForeign (P.IOError _ P.UserError _ s _ _) = UserError_Det (fromForeign s)
  fromForeign (P.IOError _ P.OtherError _ s _ _)
    | "FAILERR_ " `P.isPrefixOf` s = FailError_Det    (fromForeign (P.drop 9 s))
    | "NDERR_ "   `P.isPrefixOf` s = NondetError_Det  (fromForeign (P.drop 7 s))
    | "IOERR_ "   `P.isPrefixOf` s = IOError_Det      (fromForeign (P.drop 7 s))
  fromForeign (P.IOError _ _ _ s _ _) = IOError_Det (fromForeign s)

  toForeign (IOError_Det s)     = P.IOError P.Nothing P.OtherError "" ("IOERR_ " P.++ toForeign s) P.Nothing P.Nothing
  toForeign (UserError_Det s)   = P.IOError P.Nothing P.UserError "" (toForeign s) P.Nothing P.Nothing
  toForeign (FailError_Det s)   = P.IOError P.Nothing P.OtherError "" ("FAILERR_ " P.++ toForeign s) P.Nothing P.Nothing
  toForeign (NondetError_Det s) = P.IOError P.Nothing P.OtherError "" ("NDERR_ " P.++ toForeign s) P.Nothing P.Nothing

primuscoreioError_Det# :: IOError_Det -> IO_Det a
primuscoreioError_Det# err = P.throw (toForeign err)

primuscoreioError_ND# :: Curry (LiftedFunc IOError_ND (IO_Det a))
primuscoreioError_ND# = P.return (Func (\err -> do
  e <- BasicDefinitions.ensureOneResult (toHaskell err :: Curry IOError_Det)
  P.return (P.throw (toForeign e :: P.IOException))))

catch_Det# :: IO_Det a -> (IOError_Det -> IO_Det a) -> IO_Det a
catch_Det# io cont = P.catch io (cont . fromForeign)

catch_ND# :: Curry (LiftedFunc (IO_Det a) (LiftedFunc (LiftedFunc IOError_ND (IO_Det a)) (IO_Det a)))
catch_ND# = P.return (Func (\ioND -> P.return (Func (\contND -> do
  io <- BasicDefinitions.ensureOneResult ioND
  Func cont <- BasicDefinitions.ensureOneResult contND
  let res = P.unsafePerformIO (P.unsafeInterleaveIO (P.try io))
  case res of
    P.Left e -> cont (fromHaskell (fromForeign e))
    P.Right x -> P.return (P.return x)))))

-- -----------------------------------------------------------------------------
-- Primitive operations: Exception handling
-- -----------------------------------------------------------------------------

primuscoreerror_Det# :: CList_Det Char_Det -> a
primuscoreerror_Det# xs = P.error (toForeign xs)

primuscoreerror_ND# :: Curry (LiftedFunc (CList_ND Char_ND) a)
primuscoreerror_ND# = P.return $ Func $ toHaskell M.>=> \xs' -> primuscoreerror_Det# xs'

failed_Det# :: a
failed_Det# = P.throw Failed

failed_ND# :: Curry a
failed_ND# = P.mzero
