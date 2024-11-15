{-# LANGUAGE MagicHash, TypeOperators #-}
import BasicDefinitions
import MemoizedCurry
import Prelude
import Control.Monad.State

searchdotSetFunctionsdotset0_Det# :: (Curryable a', HsEquivalent a' ~ a) => a -> Values_Det a
searchdotSetFunctionsdotset0_Det# a = rnfC a `seq` Values_Det (CCons_Det a CList_Det)

searchdotSetFunctionsdotset0_ND# :: Curryable a => Curry (a :-> Values_ND a)
searchdotSetFunctionsdotset0_ND# = returnFunc set0

searchdotSetFunctionsdotset1_Det# :: (Curryable b', HsEquivalent b' ~ b) => (a -> b) -> a -> Values_Det b
searchdotSetFunctionsdotset1_Det# f a = rnfC b `seq` Values_Det (CCons_Det b CList_Det)
  where
    b = f a

searchdotSetFunctionsdotset1_ND# :: (Curryable a, Curryable b) => Curry ((a :-> b) :-> a :-> Values_ND b)
searchdotSetFunctionsdotset1_ND# = returnFunc $ \f -> returnFunc $ \arg1 -> set1 f arg1

searchdotSetFunctionsdotset2_Det# :: (Curryable c', HsEquivalent c' ~ c) => (a -> b -> c) -> a -> b -> Values_Det c
searchdotSetFunctionsdotset2_Det# f a b = rnfC c `seq` Values_Det (CCons_Det c CList_Det)
  where
    c = f a b

searchdotSetFunctionsdotset2_ND# :: (Curryable a, Curryable b, Curryable c) => Curry ((a :-> b :-> c) :-> a :-> b :-> Values_ND c)
searchdotSetFunctionsdotset2_ND# = returnFunc $ \f -> returnFunc $ \arg1 -> returnFunc $ \arg2 -> set2 f arg1 arg2

searchdotSetFunctionsdotset3_Det# :: (Curryable d', HsEquivalent d' ~ d) => (a -> b -> c -> d) -> a -> b -> c -> Values_Det d
searchdotSetFunctionsdotset3_Det# f a b c = rnfC d `seq` Values_Det (CCons_Det d CList_Det)
  where
    d = f a b c

searchdotSetFunctionsdotset3_ND# :: (Curryable a, Curryable b, Curryable c, Curryable d) => Curry ((a :-> b :-> c :-> d) :-> a :-> b :-> c :-> Values_ND d)
searchdotSetFunctionsdotset3_ND# = returnFunc $ \f -> returnFunc $ \arg1 -> returnFunc $ \arg2 -> returnFunc $ \arg3 -> set3 f arg1 arg2 arg3

searchdotSetFunctionsdotset4_Det# :: (Curryable e', HsEquivalent e' ~ e) => (a -> b -> c -> d -> e) -> a -> b -> c -> d -> Values_Det e
searchdotSetFunctionsdotset4_Det# f a b c d = rnfC e `seq` Values_Det (CCons_Det e CList_Det)
  where
    e = f a b c d

searchdotSetFunctionsdotset4_ND# :: (Curryable a, Curryable b, Curryable c, Curryable d, Curryable e) => Curry ((a :-> b :-> c :-> d :-> e) :-> a :-> b :-> c :-> d :-> Values_ND e)
searchdotSetFunctionsdotset4_ND# = returnFunc $ \f -> returnFunc $ \arg1 -> returnFunc $ \arg2 -> returnFunc $ \arg3 -> returnFunc $ \arg4 -> set4 f arg1 arg2 arg3 arg4

searchdotSetFunctionsdotset5_Det# :: (Curryable f', HsEquivalent f' ~ f) => (a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> Values_Det f
searchdotSetFunctionsdotset5_Det# f a b c d e = rnfC f' `seq` Values_Det (CCons_Det f' CList_Det)
  where
    f' = f a b c d e

searchdotSetFunctionsdotset5_ND# :: (Curryable a, Curryable b, Curryable c, Curryable d, Curryable e, Curryable f) => Curry ((a :-> b :-> c :-> d :-> e :-> f) :-> a :-> b :-> c :-> d :-> e :-> Values_ND f)
searchdotSetFunctionsdotset5_ND# = returnFunc $ \f -> returnFunc $ \arg1 -> returnFunc $ \arg2 -> returnFunc $ \arg3 -> returnFunc $ \arg4 -> returnFunc $ \arg5 -> set5 f arg1 arg2 arg3 arg4 arg5

searchdotSetFunctionsdotset6_Det# :: (Curryable g', HsEquivalent g' ~ g) => (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> Values_Det g
searchdotSetFunctionsdotset6_Det# f a b c d e f' = rnfC g `seq` Values_Det (CCons_Det g CList_Det)
  where
    g = f a b c d e f'

searchdotSetFunctionsdotset6_ND# :: (Curryable a, Curryable b, Curryable c, Curryable d, Curryable e, Curryable f, Curryable g) => Curry ((a :-> b :-> c :-> d :-> e :-> f :-> g) :-> a :-> b :-> c :-> d :-> e :-> f :-> Values_ND g)
searchdotSetFunctionsdotset6_ND# = returnFunc $ \f -> returnFunc $ \arg1 -> returnFunc $ \arg2 -> returnFunc $ \arg3 -> returnFunc $ \arg4 -> returnFunc $ \arg5 -> returnFunc $ \arg6 -> set6 f arg1 arg2 arg3 arg4 arg5 arg6

searchdotSetFunctionsdotset7_Det# :: (Curryable h', HsEquivalent h' ~ h) => (a -> b -> c -> d -> e -> f -> g -> h) -> a -> b -> c -> d -> e -> f -> g -> Values_Det h
searchdotSetFunctionsdotset7_Det# f a b c d e f' g = rnfC h `seq` Values_Det (CCons_Det h CList_Det)
  where
    h = f a b c d e f' g

searchdotSetFunctionsdotset7_ND# :: (Curryable a, Curryable b, Curryable c, Curryable d, Curryable e, Curryable f, Curryable g, Curryable h) => Curry ((a :-> b :-> c :-> d :-> e :-> f :-> g :-> h) :-> a :-> b :-> c :-> d :-> e :-> f :-> g :-> Values_ND h)
searchdotSetFunctionsdotset7_ND# = returnFunc $ \f -> returnFunc $ \arg1 -> returnFunc $ \arg2 -> returnFunc $ \arg3 -> returnFunc $ \arg4 -> returnFunc $ \arg5 -> returnFunc $ \arg6 -> returnFunc $ \arg7 -> set7 f arg1 arg2 arg3 arg4 arg5 arg6 arg7


-- Convert a tree list to a curry list.
transListTree :: ListTree a -> CList_ND a
transListTree NilTree         = CList_ND
transListTree (ConsTree x xs) =
  CCons_ND (treeToCurry x) (treeToCurry (fmap transListTree xs))


set0 :: Curryable a
     => Curry a -> Curry (Values_ND a)
set0 f = do
  lvl <- currentLevel <$> get
  modify (\s -> s { setComputation = True })
  Values_ND . return . transListTree <$> captureWithLvl lvl (groundNormalForm f)

set1 :: (Curryable a, Curryable b)
     => Curry (a :-> b) -> Curry a -> Curry (Values_ND b)
set1 f arg = do
  lvl <- currentLevel <$> get
  modify (\s -> s { setComputation = True })
  Values_ND . return . transListTree <$> captureWithLvl lvl (groundNormalForm (f
    `app` (setLevelC (succ lvl) arg)))

set2 :: (Curryable a, Curryable b, Curryable c)
     => Curry (a :-> b :-> c) -> Curry a -> Curry b -> Curry (Values_ND c)
set2 f arg1 arg2 = do
  lvl <- currentLevel <$> get
  modify (\s -> s { setComputation = True })
  Values_ND . return . transListTree <$> captureWithLvl lvl (groundNormalForm (f
    `app` (setLevelC (succ lvl) arg1)
    `app` (setLevelC (succ lvl) arg2)))

set3 :: (Curryable a, Curryable b, Curryable c, Curryable d)
     => Curry (a :-> b :-> c :-> d) -> Curry a -> Curry b -> Curry c -> Curry (Values_ND d)
set3 f arg1 arg2 arg3 = do
  lvl <- currentLevel <$> get
  modify (\s -> s { setComputation = True })
  Values_ND . return . transListTree <$> captureWithLvl lvl (groundNormalForm (f
    `app` (setLevelC (succ lvl) arg1)
    `app` (setLevelC (succ lvl) arg2)
    `app` (setLevelC (succ lvl) arg3)))

set4 :: (Curryable a, Curryable b, Curryable c, Curryable d, Curryable e)
     => Curry (a :-> b :-> c :-> d :-> e) -> Curry a -> Curry b -> Curry c -> Curry d -> Curry (Values_ND e)
set4 f arg1 arg2 arg3 arg4 = do
  lvl <- currentLevel <$> get
  modify (\s -> s { setComputation = True })
  Values_ND . return . transListTree <$> captureWithLvl lvl (groundNormalForm (f
    `app` (setLevelC (succ lvl) arg1)
    `app` (setLevelC (succ lvl) arg2)
    `app` (setLevelC (succ lvl) arg3)
    `app` (setLevelC (succ lvl) arg4)))

set5 :: (Curryable a, Curryable b, Curryable c, Curryable d, Curryable e, Curryable f)
     => Curry (a :-> b :-> c :-> d :-> e :-> f) -> Curry a -> Curry b -> Curry c -> Curry d -> Curry e -> Curry (Values_ND f)
set5 f arg1 arg2 arg3 arg4 arg5 = do
  lvl <- currentLevel <$> get
  modify (\s -> s { setComputation = True })
  Values_ND . return . transListTree <$> captureWithLvl lvl (groundNormalForm (f
    `app` (setLevelC (succ lvl) arg1)
    `app` (setLevelC (succ lvl) arg2)
    `app` (setLevelC (succ lvl) arg3)
    `app` (setLevelC (succ lvl) arg4)
    `app` (setLevelC (succ lvl) arg5)))

set6 :: (Curryable a, Curryable b, Curryable c, Curryable d, Curryable e, Curryable f, Curryable g)
     => Curry (a :-> b :-> c :-> d :-> e :-> f :-> g) -> Curry a -> Curry b -> Curry c -> Curry d -> Curry e -> Curry f -> Curry (Values_ND g)
set6 f arg1 arg2 arg3 arg4 arg5 arg6 = do
  lvl <- currentLevel <$> get
  modify (\s -> s { setComputation = True })
  Values_ND . return . transListTree <$> captureWithLvl lvl (groundNormalForm (f
    `app` (setLevelC (succ lvl) arg1)
    `app` (setLevelC (succ lvl) arg2)
    `app` (setLevelC (succ lvl) arg3)
    `app` (setLevelC (succ lvl) arg4)
    `app` (setLevelC (succ lvl) arg5)
    `app` (setLevelC (succ lvl) arg6)))

set7 :: (Curryable a, Curryable b, Curryable c, Curryable d, Curryable e, Curryable f, Curryable g, Curryable h)
     => Curry (a :-> b :-> c :-> d :-> e :-> f :-> g :-> h) -> Curry a -> Curry b -> Curry c -> Curry d -> Curry e -> Curry f -> Curry g -> Curry (Values_ND h)
set7 f arg1 arg2 arg3 arg4 arg5 arg6 arg7 = do
  lvl <- currentLevel <$> get
  modify (\s -> s { setComputation = True })
  Values_ND . return . transListTree <$> captureWithLvl lvl (groundNormalForm (f
    `app` (setLevelC (succ lvl) arg1)
    `app` (setLevelC (succ lvl) arg2)
    `app` (setLevelC (succ lvl) arg3)
    `app` (setLevelC (succ lvl) arg4)
    `app` (setLevelC (succ lvl) arg5)
    `app` (setLevelC (succ lvl) arg6)
    `app` (setLevelC (succ lvl) arg7)))
