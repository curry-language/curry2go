{-# LANGUAGE MagicHash #-}

import qualified Prelude as P
import qualified System.Info as S
import qualified Data.Version as D
import BasicDefinitions

compilerdotDistributiondotcurryCompiler_Det# = P.error "No implementation of curryCompiler"
compilerdotDistributiondotcurryCompiler_ND# = P.return P.$ from compilerdotDistributiondotcurryCompiler_Det#

compilerdotDistributiondotcurryCompilerMajorVersion_Det# = P.error "No implementation of curryCompilerMajorVersion"
compilerdotDistributiondotcurryCompilerMajorVersion_ND# = P.return compilerdotDistributiondotcurryCompilerMajorVersion_Det#

compilerdotDistributiondotcurryCompilerMinorVersion_Det# = P.error "No implementation of curryCompilerMinorVersion"
compilerdotDistributiondotcurryCompilerMinorVersion_ND# = P.return compilerdotDistributiondotcurryCompilerMinorVersion_Det#

compilerdotDistributiondotcurryCompilerRevisionVersion_Det# = P.error "No implementation of curryCompilerRevisionVersion"
compilerdotDistributiondotcurryCompilerRevisionVersion_ND# = P.return compilerdotDistributiondotcurryCompilerRevisionVersion_Det#

compilerdotDistributiondotcurryRuntime_Det# = fromForeign S.compilerName
compilerdotDistributiondotcurryRuntime_ND# = P.return P.$ from compilerdotDistributiondotcurryRuntime_Det#

compilerdotDistributiondotcurryRuntimeMajorVersion_Det# = fromForeign P.$ P.toInteger P.$ (D.versionBranch S.compilerVersion) P.!! 0
compilerdotDistributiondotcurryRuntimeMajorVersion_ND# = P.return compilerdotDistributiondotcurryRuntimeMajorVersion_Det#

compilerdotDistributiondotcurryRuntimeMinorVersion_Det# = fromForeign P.$ P.toInteger P.$ (D.versionBranch S.compilerVersion) P.!! 1
compilerdotDistributiondotcurryRuntimeMinorVersion_ND# = P.return compilerdotDistributiondotcurryRuntimeMinorVersion_Det#

compilerdotDistributiondotbaseVersion_Det# = P.error "No implementation of baseVersion"
compilerdotDistributiondotbaseVersion_ND# = P.return P.$ from compilerdotDistributiondotbaseVersion_Det#

compilerdotDistributiondotinstallDir_Det# = P.error "No implementation of installDir"
compilerdotDistributiondotinstallDir_ND# = P.return P.$ from compilerdotDistributiondotinstallDir_Det#
