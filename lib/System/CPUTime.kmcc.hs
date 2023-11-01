{-# LANGUAGE MagicHash #-}
import qualified Prelude as P
import qualified System.CPUTime as S
import BasicDefinitions

cPUTimedotgetCPUTime_Det# = fromForeign (S.getCPUTime P.>>= (P.return P.. (`P.div` (10 P.^ 9))))
cPUTimedotgetCPUTime_ND# = P.return cPUTimedotgetCPUTime_Det#

cPUTimedotgetElapsedTime_Det# = P.error "No implementation of getElapsedTime_Det"
cPUTimedotgetElapsedTime_ND# = P.error "No implementation of getElapsedTime_ND"