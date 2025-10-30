{-# LANGUAGE MagicHash #-}
import qualified BasicDefinitions as BD
import qualified MemoizedCurry  as MC
import qualified Control.Monad.State as S
import Tree

searchdotUnsafedotallValues_Det# :: BD.Curryable a => BD.HsEquivalent a -> CList_Det (BD.HsEquivalent a)
searchdotUnsafedotallValues_Det# e = if BD.isBottomNF e
  then CList_Det
  else CCons_Det e CList_Det

searchdotUnsafedotoneValue_Det# :: BD.Curryable a => BD.HsEquivalent a -> Maybe_Det (BD.HsEquivalent a)
searchdotUnsafedotoneValue_Det# e = if BD.isBottomNF e
  then Nothing_Det
  else Just_Det e

searchdotUnsafedotallValues_ND# :: MC.Curryable a => B.Curry (B.LiftedFunc a (CList_ND a))
searchdotUnsafedotallValues_ND# = BD.returnFunc P.$ \x -> S.get P.>>= \s -> P.return P.$
  toCurryList (bfs (MC.evalCurryTreeWith (BD.groundNormalForm x) s))

searchdotUnsafedotoneValue_ND# :: MC.Curryable a => B.Curry (B.LiftedFunc a (Maybe_ND a))
searchdotUnsafedotoneValue_ND# = BD.returnFunc P.$ \x -> S.get P.>>= \s -> P.return P.$
  toCurryMaybe (bfs (MC.evalCurryTreeWith (BD.groundNormalForm x) s))

toCurryList :: [a] -> CList_ND a
toCurryList [] = CList_ND
toCurryList (x:xs) = CCons_ND (P.return x) (P.return (toCurryList xs))

toCurryMaybe :: [a] -> Maybe_ND a
toCurryMaybe [] = Nothing_ND
toCurryMaybe (x:_) = Just_ND (P.return x)
