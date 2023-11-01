{-# LANGUAGE MagicHash #-}

import qualified Prelude as P
import qualified System.Environment as S
import qualified Distribution.System as D
import qualified Network.BSD as N
import BasicDefinitions

environmentdotgetArgs_Det# = fromForeign S.getArgs
environmentdotgetArgs_ND# = P.return P.$ from environmentdotgetArgs_Det#

environmentdotprimuscoregetEnviron_Det# = liftForeign1 lookup
 where
  lookup v = (S.lookupEnv v P.>>= (\x -> case x of
    P.Nothing -> P.return []
    P.Just y -> P.return y))
environmentdotprimuscoregetEnviron_ND# = liftConvertIO1 environmentdotprimuscoregetEnviron_Det#


environmentdotprimuscoresetEnviron_Det# = liftForeign2 S.setEnv
environmentdotprimuscoresetEnviron_ND# = liftConvertIO2 environmentdotprimuscoresetEnviron_Det#

environmentdotprimuscoreunsetEnviron_Det# = liftForeign1 S.unsetEnv
environmentdotprimuscoreunsetEnviron_ND# = liftConvertIO1 environmentdotprimuscoreunsetEnviron_Det#

environmentdotgetHostname_Det# = fromForeign N.getHostName
environmentdotgetHostname_ND# = P.return P.$ from environmentdotgetHostname_Det#

environmentdotgetProgName_Det# = fromForeign S.getProgName
environmentdotgetProgName_ND# = P.return P.$ from environmentdotgetProgName_Det#

environmentdotisWindows_Det# = fromForeign (D.buildOS P.== D.Windows)
environmentdotisWindows_ND# = P.return P.$ from environmentdotisWindows_Det#