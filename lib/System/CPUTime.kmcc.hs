{-# LANGUAGE MagicHash #-}
import qualified Prelude as P
import qualified System.CPUTime as S
import qualified System.Time.Extra as E
import BasicDefinitions

cPUTimedotgetCPUTime_Det# = fromForeign (S.getCPUTime P.>>= (P.return P.. (`P.div` (10 P.^ 9))))
cPUTimedotgetCPUTime_ND# = P.return cPUTimedotgetCPUTime_Det#

cPUTimedotgetElapsedTime_Det# = fromForeign B.offsetTime
cPUTimedotgetElapsedTime_ND# = P.return cPUTimedotgetElapsedTime_Det#
