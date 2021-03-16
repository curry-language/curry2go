import Language.Go.Types
import Language.Go.Show (showGoProg)
import Curry2Go.PkgConfig (packagePath)

import Curry2Go.Config 
  (lowerCompilerName, compilerMajorVersion
  , compilerMinorVersion, compilerRevisionVersion)

main :: IO ()
main = do
  bvs <- readFile (packagePath ++ "/lib/VERSION")
  -- TODO: extract this information from actual go compiler
  let gomajor = 1
      gominor = 14
  writeFile (packagePath ++ "lib/Curry/Compiler/Distribution_external.go")
            (showGoProg (createGoDistribution (head (lines bvs)) "go"
                                              gomajor gominor))

createGoDistribution :: String -> String -> Int -> Int -> GoProg
createGoDistribution baseversion gocompiler gomajor gominor =
 GoProg "CurryCompilerDistribution" ["gocurry"]
  [ GoTopLevelFuncDecl 
    (GoFuncDecl "ExternalCurry_Compiler_Distribution_curryCompiler"
    [GoParam ["task"] "*gocurry.Task"] [] 
    [GoExprStat (GoCall (GoOpName "gocurry.StringCreate")
    [GoCall (GoSelector (GoOpName "task") "GetControl") []
    , GoStringLit lowerCompilerName])])
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
    , GoStringLit gocompiler])])
  , GoTopLevelFuncDecl 
    (GoFuncDecl "ExternalCurry_Compiler_Distribution_curryRuntimeMajorVersion"
    [GoParam ["task"] "*gocurry.Task"] [] 
    [GoExprStat (GoCall (GoOpName "gocurry.IntLitCreate")
    [GoCall (GoSelector (GoOpName "task") "GetControl") []
    , GoIntLit gomajor])])
  , GoTopLevelFuncDecl 
    (GoFuncDecl "ExternalCurry_Compiler_Distribution_curryRuntimeMinorVersion"
    [GoParam ["task"] "*gocurry.Task"] [] 
    [GoExprStat (GoCall (GoOpName "gocurry.IntLitCreate")
    [GoCall (GoSelector (GoOpName "task") "GetControl") []
    , GoIntLit gominor])])
  , GoTopLevelFuncDecl 
    (GoFuncDecl "ExternalCurry_Compiler_Distribution_baseVersion"
    [GoParam ["task"] "*gocurry.Task"] [] 
    [GoExprStat (GoCall (GoOpName "gocurry.StringCreate")
    [GoCall (GoSelector (GoOpName "task") "GetControl") []
    , GoStringLit baseversion])])
  , GoTopLevelFuncDecl 
    (GoFuncDecl "ExternalCurry_Compiler_Distribution_installDir"
    [GoParam ["task"] "*gocurry.Task"] [] 
    [GoExprStat (GoCall (GoOpName "gocurry.StringCreate")
    [GoCall (GoSelector (GoOpName "task") "GetControl") []
    , GoStringLit packagePath])])
  ]

