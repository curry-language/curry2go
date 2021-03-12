import Language.Go.Types
import Language.Go.Show (showGoProg)
import Curry2Go.PkgConfig (packagePath)

import Curry.Compiler.Distribution 
  (curryRuntime, curryRuntimeMajorVersion
  , curryRuntimeMinorVersion, baseVersion)

import Curry2Go.Config 
  (compilerName, compilerMajorVersion
  , compilerMinorVersion, compilerRevisionVersion)

main :: IO ()
main = writeFile (packagePath ++ "lib/Curry/Compiler/Distribution_external.go") (showGoProg createGoDistribution)

createGoDistribution :: GoProg
createGoDistribution = GoProg "CurryCompilerDistribution" ["gocurry"]
  [ GoTopLevelFuncDecl 
    (GoFuncDecl "ExternalCurry_Compiler_Distribution_curryCompiler"
    [GoParam ["task"] "*gocurry.Task"] [] 
    [GoExprStat (GoCall (GoOpName "gocurry.StringCreate")
    [GoCall (GoSelector (GoOpName "task") "GetControl") []
    , GoStringLit compilerName])])
  , GoTopLevelFuncDecl 
    (GoFuncDecl "ExternalCurry_Compiler_Distribution_curryCompilerMajorVersion"
    [GoParam ["task"] "*gocurry.Task"] [] 
    [GoExprStat (GoCall (GoOpName "gocurry.IntLitCreate")
    [GoCall (GoSelector (GoOpName "task") "GetControl") []
    , GoIntLit compilerMajorVersion])])
  , GoTopLevelFuncDecl 
    (GoFuncDecl "ExternalCurry_Compiler_Distribution_curryCompilerMinorVersion"
    [GoParam ["task"] "*gocurry.Task"] [] 
    [GoExprStat (GoCall (GoOpName "gocurry.IntLitCreate")
    [GoCall (GoSelector (GoOpName "task") "GetControl") []
    , GoIntLit compilerMinorVersion])])
  , GoTopLevelFuncDecl 
    (GoFuncDecl "ExternalCurry_Compiler_Distribution_curryCompilerRevisionVersion"
    [GoParam ["task"] "*gocurry.Task"] [] 
    [GoExprStat (GoCall (GoOpName "gocurry.IntLitCreate")
    [GoCall (GoSelector (GoOpName "task") "GetControl") []
    , GoIntLit compilerRevisionVersion])])
  , GoTopLevelFuncDecl 
    (GoFuncDecl "ExternalCurry_Compiler_Distribution_curryRuntime"
    [GoParam ["task"] "*gocurry.Task"] [] 
    [GoExprStat (GoCall (GoOpName "gocurry.StringCreate")
    [GoCall (GoSelector (GoOpName "task") "GetControl") []
    , GoStringLit curryRuntime])])
  , GoTopLevelFuncDecl 
    (GoFuncDecl "ExternalCurry_Compiler_Distribution_curryRuntimeMajorVersion"
    [GoParam ["task"] "*gocurry.Task"] [] 
    [GoExprStat (GoCall (GoOpName "gocurry.IntLitCreate")
    [GoCall (GoSelector (GoOpName "task") "GetControl") []
    , GoIntLit curryRuntimeMajorVersion])])
  , GoTopLevelFuncDecl 
    (GoFuncDecl "ExternalCurry_Compiler_Distribution_curryRuntimeMinorVersion"
    [GoParam ["task"] "*gocurry.Task"] [] 
    [GoExprStat (GoCall (GoOpName "gocurry.IntLitCreate")
    [GoCall (GoSelector (GoOpName "task") "GetControl") []
    , GoIntLit curryRuntimeMinorVersion])])
  , GoTopLevelFuncDecl 
    (GoFuncDecl "ExternalCurry_Compiler_Distribution_baseVersion"
    [GoParam ["task"] "*gocurry.Task"] [] 
    [GoExprStat (GoCall (GoOpName "gocurry.StringCreate")
    [GoCall (GoSelector (GoOpName "task") "GetControl") []
    , GoStringLit baseVersion])])
  , GoTopLevelFuncDecl 
    (GoFuncDecl "ExternalCurry_Compiler_Distribution_installDir"
    [GoParam ["task"] "*gocurry.Task"] [] 
    [GoExprStat (GoCall (GoOpName "gocurry.StringCreate")
    [GoCall (GoSelector (GoOpName "task") "GetControl") []
    , GoStringLit packagePath])])
  ]
  
