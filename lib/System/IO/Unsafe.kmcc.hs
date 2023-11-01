{-# LANGUAGE MagicHash    #-}

import BasicDefinitions
import qualified Prelude as P
import qualified System.IO.Unsafe as S

iOdotUnsafedotunsafePerformIO_Det# = S.unsafePerformIO
iOdotUnsafedotunsafePerformIO_ND# = P.return P.$ Func P.$ \x -> do
  x' <- x
  P.return (S.unsafePerformIO x')

iOdotUnsafedotspawnConstraint_Det# = P.error "No implementation of spawnConstraint_Det"
iOdotUnsafedotspawnConstraint_ND# = P.error "No implementation of spawnConstraint_ND"

iOdotUnsafedotprimuscoreisVar_Det# = P.error "No implementation of isVar_Det"
iOdotUnsafedotprimuscoreisVar_ND# = P.error "No implementation of isVar_ND"

iOdotUnsafedotprimuscoreidenticalVar_Det# = P.error "No implementation of identicalVar_Det"
iOdotUnsafedotprimuscoreidenticalVar_ND# = P.error "No implementation of identicalVar_ND"

iOdotUnsafedotprimuscoreisGround_Det# = P.error "No implementation of isGround_Det"
iOdotUnsafedotprimuscoreisGround_ND# = P.error "No implementation of isGround_ND"

iOdotUnsafedotcompareAnyTerm_Det# = P.error "No implementation of compareAnyTerm_Det"
iOdotUnsafedotcompareAnyTerm_ND# = P.error "No implementation of compareAnyTerm_ND"

iOdotUnsafedotprimuscoreshowAnyTerm_Det# = P.error "No implementation of showAnyTerm_Det"
iOdotUnsafedotprimuscoreshowAnyTerm_ND# = P.error "No implementation of showAnyTerm_ND"

iOdotUnsafedotprimuscorereadsAnyUnqualifiedTerm_Det# = P.error "No implementation of readsAnyUnqualifiedTerm_Det"
iOdotUnsafedotprimuscorereadsAnyUnqualifiedTerm_ND# = P.error "No implementation of readsAnyUnqualifiedTerm_ND"

iOdotUnsafedotshowAnyExpression_Det# = P.error "No implementation of showAnyExpression_Det"
iOdotUnsafedotshowAnyExpression_ND# = P.error "No implementation of showAnyExpression_ND"