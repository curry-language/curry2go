import System.FilePath       ( joinPath )
import Language.Go.Types
import Language.Go.ShowS     ( showGoProg )
import Curry2Go.InstallPath  ( curry2GoHomeDir )

import Curry2Go.Config 
  (lowerCompilerName, compilerMajorVersion
  , compilerMinorVersion, compilerRevisionVersion)

main :: IO ()
main = do
  c2ghome <- curry2GoHomeDir
  bvs <- readFile (joinPath [c2ghome, "lib", "VERSION"])
  writeFile (joinPath [c2ghome, "lib", "Curry", "Compiler",
                       "Distribution_external.go"])
    (showGoProg (createGoDistribution (head (lines bvs)) c2ghome "go") "")

createGoDistribution :: String -> String -> String -> GoProg
createGoDistribution baseversion c2ghome gocompiler =
 GoProg "Curry_DOT_Compiler_DOT_Distribution" ["gocurry"]
  [ GoTopLevelFuncDecl 
    (GoFuncDecl "ExternalCurryDot_CompilerDot_DistributionDot_curryCompiler"
    [GoParam ["task"] "*gocurry.Task"] [] 
    [GoExprStat (GoCall (GoOpName "gocurry.StringCreate")
    [GoCall (GoSelector (GoOpName "task") "GetControl") []
    , GoStringLit lowerCompilerName])])
  , GoTopLevelFuncDecl 
    (GoFuncDecl "ExternalCurryDot_CompilerDot_DistributionDot_curryCompilerMajorVersion"
    [GoParam ["task"] "*gocurry.Task"] [] 
    [GoExprStat (GoCall (GoOpName "gocurry.IntLitCreate")
    [GoCall (GoSelector (GoOpName "task") "GetControl") []
    , GoIntLit compilerMajorVersion])])
  , GoTopLevelFuncDecl 
    (GoFuncDecl "ExternalCurryDot_CompilerDot_DistributionDot_curryCompilerMinorVersion"
    [GoParam ["task"] "*gocurry.Task"] [] 
    [GoExprStat (GoCall (GoOpName "gocurry.IntLitCreate")
    [GoCall (GoSelector (GoOpName "task") "GetControl") []
    , GoIntLit compilerMinorVersion])])
  , GoTopLevelFuncDecl 
    (GoFuncDecl "ExternalCurryDot_CompilerDot_DistributionDot_curryCompilerRevisionVersion"
    [GoParam ["task"] "*gocurry.Task"] [] 
    [GoExprStat (GoCall (GoOpName "gocurry.IntLitCreate")
    [GoCall (GoSelector (GoOpName "task") "GetControl") []
    , GoIntLit compilerRevisionVersion])])
  , GoTopLevelFuncDecl 
    (GoFuncDecl "ExternalCurryDot_CompilerDot_DistributionDot_curryRuntime"
    [GoParam ["task"] "*gocurry.Task"] [] 
    [GoExprStat (GoCall (GoOpName "gocurry.StringCreate")
    [GoCall (GoSelector (GoOpName "task") "GetControl") []
    , GoStringLit gocompiler])])
  , GoTopLevelFuncDecl 
    (GoFuncDecl "ExternalCurryDot_CompilerDot_DistributionDot_curryRuntimeMajorVersion"
    [GoParam ["task"] "*gocurry.Task"] [] 
    [GoExprStat (GoCall (GoOpName "gocurry.GoMajVer")
    [GoCall (GoSelector (GoOpName "task") "GetControl") []])])
  , GoTopLevelFuncDecl 
    (GoFuncDecl "ExternalCurryDot_CompilerDot_DistributionDot_curryRuntimeMinorVersion"
    [GoParam ["task"] "*gocurry.Task"] [] 
    [GoExprStat (GoCall (GoOpName "gocurry.GoMinVer")
    [GoCall (GoSelector (GoOpName "task") "GetControl") []])])
  , GoTopLevelFuncDecl 
    (GoFuncDecl "ExternalCurryDot_CompilerDot_DistributionDot_baseVersion"
    [GoParam ["task"] "*gocurry.Task"] [] 
    [GoExprStat (GoCall (GoOpName "gocurry.StringCreate")
    [GoCall (GoSelector (GoOpName "task") "GetControl") []
    , GoStringLit baseversion])])
  , GoTopLevelFuncDecl 
    (GoFuncDecl "ExternalCurryDot_CompilerDot_DistributionDot_installDir"
    [GoParam ["task"] "*gocurry.Task"] [] 
    [GoExprStat (GoCall (GoOpName "gocurry.StringCreate")
    [GoCall (GoSelector (GoOpName "task") "GetControl") []
    , GoStringLit c2ghome])])
  ]

