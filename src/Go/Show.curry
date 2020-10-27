module Go.Show where

import Go.Types

--- Shows a Go expression as a string in Go syntax.
showGoExpr :: GoExpr -> String
showGoExpr (GoBoolLit b)                = if b then "true" else "false"
showGoExpr (GoIntLit i)                 = show i
showGoExpr (GoFloatLit f)               = show f
showGoExpr (GoStringLit s)              = show s
showGoExpr (GoByteLit c)                = show c
showGoExpr (GoCompositeLit t exprs)     = t ++ "{ "
  ++ (showGoCommaList showGoExpr exprs) ++ " }"
showGoExpr (GoOpName s)                 = s
showGoExpr (GoOpExpr expr)              = "(" ++ (showGoExpr expr) ++ ")"
showGoExpr (GoConversion t expr)        = t ++ "( " 
  ++ (showGoExpr expr) ++ " )"
showGoExpr (GoSelector expr s)          = (showGoExpr expr) ++ "." ++ s
showGoExpr (GoIndex expr1 expr2)        = (showGoExpr expr1) ++ "[ "
  ++ (showGoExpr expr2) ++ " ]"
showGoExpr (GoSlice expr1 expr2 expr3)  = (showGoExpr expr1) ++ "[ "
  ++ (showGoExpr expr2) ++ " : " ++ (showGoExpr expr3) ++ " ]"
showGoExpr (GoVariadic expr)            = (showGoExpr expr) ++ "..."
showGoExpr (GoCall expr exprs)          = (showGoExpr expr) ++ "( "
  ++ (showGoCommaList showGoExpr exprs) ++ " )"
showGoExpr (GoUnaryExpr s expr)         = s ++  " " ++ (showGoExpr expr)
showGoExpr (GoBinaryExpr expr1 s expr2) = (showGoExpr expr1)
  ++ " " ++ s ++ " " ++ (showGoExpr expr2)

--- Shows a Go statement as a string in Go syntax with indenting.
--- @param n      - number of spaces to indent
--- @param gostat - the Go statement to show
showGoStat :: Int -> GoStat -> String
showGoStat n (GoConstDecl ids t [])          = (indent n) ++ "const "
  ++ (showGoCommaList id ids) ++ " " ++ t ++ "\n" 
showGoStat n (GoConstDecl ids t exprs@(_:_)) = (indent n) ++ "const "
  ++ (showGoCommaList id ids) ++ " " ++ t ++ " = "
  ++ (showGoCommaList showGoExpr exprs) ++ "\n"
showGoStat n (GoVarDecl ids t [])            = (indent n) ++ "var "
  ++ (showGoCommaList id ids) ++ " " ++ t ++  "\n" 
showGoStat n (GoVarDecl ids t exprs@(_:_))   = (indent n) ++ "var "
  ++ (showGoCommaList id ids) ++ " " ++ t ++ " = " 
  ++ (showGoCommaList showGoExpr exprs) ++ "\n"
showGoStat n (GoShortVarDecl ids exprs)      = (indent n) 
  ++ (showGoCommaList id ids) ++ " := "
  ++ (showGoCommaList showGoExpr exprs) ++ "\n" 
showGoStat n (GoExprStat expr)               = (indent n) ++ (showGoExpr expr)
  ++ "\n"
showGoStat n (GoAssign exprs1 op exprs2)     = (indent n)
  ++ (showGoCommaList showGoExpr exprs1) ++ " " ++ op ++ " "
  ++ (showGoCommaList showGoExpr exprs2) ++ "\n"
showGoStat _ (GoEmpty)                       = "\n"
showGoStat n (GoReturn [])                   = (indent n) ++ "return\n"
showGoStat n (GoReturn exprs@(_:_))          = (indent n) ++ "return( "
  ++ (showGoCommaList showGoExpr exprs) ++ " )\n"
showGoStat n (GoBreak)                       = (indent n) ++ "break\n"
showGoStat n (GoContinue)                    = (indent n) ++ "continue\n"
showGoStat n (GoBlock stats)                 = (indent n) ++ "{\n"
  ++ (concatMap (showGoStat (n+1)) stats) ++ (indent n) ++ "}\n"
showGoStat n (GoIf expr block1 block2)       = (indent n) ++ "if( "
  ++ (showGoExpr expr)  ++ " ){\n" ++ (concatMap (showGoStat (n+1)) block1)
  ++ (indent n) ++ "}else {\n" ++ (concatMap (showGoStat (n+1)) block2) 
  ++ (indent n) ++ "}\n"
showGoStat n (GoExprSwitch expr branches)    = (indent n) ++ "switch "
  ++ (showGoExpr expr) ++ "{\n"
  ++ (concatMap (goShowExprBranch (n+1)) branches) ++ (indent n) ++ "}\n"

--- Shows a Go expression branch as a String in Go Syntax.
--- @param n      - number of spaces to indent
--- @param branch - branch to show
goShowExprBranch :: Int -> GoExprBranch -> String
goShowExprBranch n (GoExprDefault stats)      = (indent n) ++ "default:\n"
  ++ (concatMap (showGoStat (n+1)) stats)
goShowExprBranch n (GoExprBranch exprs stats) = (indent n) ++ "case "
  ++ (showGoCommaList showGoExpr exprs) ++ ":\n"
  ++ (concatMap (showGoStat (n+1)) stats)

--- Shows a Go program as a string in Go syntax.
showGoProg :: GoProg -> String
showGoProg (GoProg package imports decls) = "package " ++ package ++ "\n\n"
  ++ (showGoImports imports )
  ++ (concatMap (\x -> (showGoTopLevelDecl x) ++ "\n") decls)

--- Shows a list of imports as a string in Go syntax.
showGoImports :: [String] -> String
showGoImports []       = "\n"
showGoImports (x : xs) = "import " ++ (show x) ++ "\n" ++ (showGoImports xs)

--- Shows a Go top-level declaration as a string in Go syntax.
showGoTopLevelDecl :: GoTopLevelDecl -> String
showGoTopLevelDecl (GoTopLevelDecl stat)     = (showGoStat 0 stat)
showGoTopLevelDecl (GoTopLevelFuncDecl func) = (showGoFuncDecl func)

--- Shows a Go function declaration as a string in Go syntax.
showGoFuncDecl :: GoFuncDecl -> String
showGoFuncDecl (GoFuncDecl name params results body) = "func "++ name ++ "( "
  ++ (showGoCommaList showGoParam params) ++ " )( "
  ++ (showGoCommaList showGoParam results) ++ " ){\n"
  ++ (concatMap (showGoStat 1) body) ++ "}\n"

--- Shows a Go parameter as a string in Go syntax.
showGoParam :: GoParam -> String
showGoParam (GoParam [] t)       = t
showGoParam (GoParam xs@(_:_) t) = (showGoCommaList id xs) ++ " " ++ t

--- Shows a List of a Go type as a comma separated list in go syntax.
--- @param f    - function to show the Go type as a string in go syntax
--- @param list - list of a Go type
showGoCommaList :: (a -> String) -> [a] -> String
showGoCommaList _ []           = ""
showGoCommaList f [x]          = f x
showGoCommaList f (x:xs@(_:_)) = (f x) ++ ", " ++ (showGoCommaList f xs)

--- Creates a string of blanks with length 4*n
indent :: Int -> String
indent n = replicate (4*n) ' '
