module Curry2Go.Compiler
  ( CGOptions(..), defaultCGOptions, printVerb
  , SearchStrat(..), compileIProg2GoString, compileIProg2Go
  , createMainProg, removeDots)
 where

import Control.Monad ( when )
import Go.Show
import Go.Types
import CompilerStructure
import ICurry.Types
import ICurry.Compiler
import System.Environment
import System.CurryPath
import System.Console.GetOpt
import System.Directory
import Data.Char
import Data.Time
import System.FilePath
import Data.List

--- Data types ---

--- Data type for compiler options.
data CGOptions = CGOptions
  { help         :: Bool         -- should help text be displayed
  , verbosity    :: Int          -- verbosity (0..4)
                                 -- (0: quiet, 1: status,...)
  , genMain      :: Bool         -- should a main method be generated
  , strat        :: SearchStrat  -- search strategy to be used by the runtime system
  , maxResults   :: Int          -- maximum number of results to compute
  , maxTasks     :: Int          -- maximum number of concurrent tasks in a fair search
  , run          :: Bool         -- run the program after compilation
  , modName      :: String       -- used internally to track main module. not configurable
  , time         :: Bool         -- measure execution time
  , times        :: Int          -- number of runs to average execution time over
  , interact     :: Bool         -- interactive result printing
  , mainName     :: String       -- name of the function to run as main
  , onlyHnf      :: Bool         -- only compute hnf
  , printName    :: Bool         -- print compiler name
  , printNumVer  :: Bool         -- print numeric version
  , printBaseVer :: Bool         -- print base version
  }

--- Default options.
defaultCGOptions :: CGOptions
defaultCGOptions = CGOptions 
  { help         = False
  , verbosity    = 1
  , genMain      = True
  , strat        = DFS
  , maxResults   = 0
  , maxTasks     = 0
  , run          = False
  , modName      = ""
  , time         = False
  , times        = 1
  , interact     = False
  , mainName     = "main"
  , onlyHnf      = False
  , printName    = False
  , printNumVer  = False
  , printBaseVer = False
  }

printVerb :: CGOptions -> Int -> String -> IO ()
printVerb opts v s = when (verbosity opts >= v) $ putStrLn s

--- Data type for search strategies.
data SearchStrat = DFS
                 | BFS
                 | FS
  deriving Show

--- Constant functions ---

--- Name of the Go package with the curry runtime.
runtime :: String
runtime = "gocurry"

--- Returns a string of the node type in go syntax.
node :: String
node = runtime ++ ".Node"

--- Returns a string of the task type in go syntax.
task :: String
task = "*" ++ runtime ++ ".Task"

--- Go expression creating a new node.
newNode :: GoExpr
newNode = GoCall (GoOpName "new") [GoOpName node]

--- Go expression to access the root node.
root :: GoExpr
root = var 0

--- Converts a variable index into a Go variable name.
varName :: Int -> String
varName n | n == 0    = "root"
          | otherwise = "x" ++ show n

--- Converts a variable index into a Go operand.
var :: Int -> GoExpr
var n = GoOpName (varName n)

--- Functions compiling ICurry to Go code ---

--- Creates an executable Go program, which calls
--- the evaluation function on the main function
--- of a curry module, that is being translated.
--- Throws an error if there is no main function in funcs.
--- @param funcs - list of all function definitions in the curry module.
--- @param opts  - compiler options 
createMainProg :: [IFunction] -> CGOptions -> GoProg
createMainProg [] _ = error "No main function found!"
createMainProg ((IFunction name@(modName, fname,_) ar _ _ _):xs) opts
  | fname == mainName opts && ar > 0 = error $ "Overloaded initial expression"
  | fname == mainName opts = GoProg "main"
  ["gocurry", "./" ++ modNameToPath modName] [GoTopLevelFuncDecl
  (GoFuncDecl "main" [] []
  [GoShortVarDecl ["node"] [GoCall (GoOpName (iqname2Go opts name ++ "Create"))
  [newNode]]
  , GoExprStat (GoCall
    (GoOpName (runtime ++ if time opts then ".Benchmark" else ".Evaluate"))
    ((if time opts then [GoOpName "node", GoIntLit (times opts)]
                   else [GoOpName "node", GoBoolLit (interact opts)])
    ++ [GoBoolLit (onlyHnf opts), GoOpName (runtime ++ "." ++ (show (strat opts)))
    , GoIntLit (maxResults opts), GoIntLit (maxTasks opts)]))])]
 | otherwise = createMainProg xs opts

--- Creates a string in Go syntax from an IProg.
--- @param opts     - compiler options
--- @param iprog    - IProg to convert
compileIProg2GoString :: CGOptions -> IProg -> String
compileIProg2GoString opts iprog@(IProg moduleName _ _ _) =
  showGoProg (compileIProg2Go (opts {modName = moduleName}) iprog)

--- Creates a Go program from an IProg.
--- @param opts     - compiler options
--- @param iprog    - IProg to convert
compileIProg2Go :: CGOptions -> IProg -> GoProg
compileIProg2Go opts (IProg moduleName _ dtypes funcs) =
  GoProg (removeDots moduleName)
  ("gocurry" : (foldl union [] (map (getImports opts) funcs)))
  ([ifuncNames2Go opts funcs]
  ++ map (idataNames2Go opts) dtypes
  ++ concatMap (idata2GoCreate (opts {modName = moduleName})) dtypes
  ++ ifunc2GoCreate (opts {modName = moduleName}) 0 funcs
  ++ map (ifunc2Go (opts {modName = moduleName})) funcs)

--- Extracts the modules used by an IFunction.
getImports :: CGOptions -> IFunction -> [String]
getImports opts (IFunction _ _ _ _ body) = toImport (getImportsBody body)
 where
  getImportsBody (IExternal _)     = []
  getImportsBody (IFuncBody block) = getImportsBlock block
  getImportsBlock (IBlock _ _ stat) = getImportsStat stat
  getImportsStat IExempt                = []
  getImportsStat (IReturn expr)         = getImportsExpr expr
  getImportsStat (ICaseCons _ branches) =
    unionize (map getImportsCons branches)
  getImportsStat (ICaseLit _ branches)  =
    unionize (map getImportsLit branches)
  getImportsCons (IConsBranch _ _ block) = getImportsBlock block
  getImportsLit (ILitBranch _ block) = getImportsBlock block
  getImportsExpr (IVar _)                    = []
  getImportsExpr (IVarAccess _ _)            = []
  getImportsExpr (ILit _)                    = []
  getImportsExpr (IFCall (m, _, _) exprs)    = 
    m:(unionize (map getImportsExpr exprs))
  getImportsExpr (ICCall (m, _, _) exprs)    = 
    m:(unionize (map getImportsExpr exprs))
  getImportsExpr (IFPCall (m, _, _) _ exprs) = 
    m:(unionize (map getImportsExpr exprs))
  getImportsExpr (ICPCall (m, _, _) _ exprs) = 
    m:(unionize (map getImportsExpr exprs))
  getImportsExpr (IOr expr1 expr2)           = 
    union (getImportsExpr expr1) (getImportsExpr expr2)
  unionize ls = foldl union [] ls
  toImport [] = []
  toImport (x:xs)
    | x == "Prelude"    = (toImport xs)
    | x == modName opts = (toImport xs)       
    | otherwise         =
      (concat (replicate (length (splitModuleIdentifiers (modName opts)))"../")
      ++ modNameToPath x) : (toImport xs)
      
--- Creates a top-level declaration for an array with all function names.
--- @param opts  - compiler options
--- @param funcs - list of IFunctions
ifuncNames2Go :: CGOptions -> [IFunction] -> GoTopLevelDecl
ifuncNames2Go opts funcs = GoTopLevelDecl 
  (GoVarDecl ["func_names"] "[]string"
  [GoCompositeLit "[]string" (map getName funcs)])
 where
  getName (IFunction (_,name,_) _ _ _ _) = GoStringLit name
  
--- Creates a top-level declaration for an array with all constructor names.
--- @param opts   - compiler options
--- @param dtype  - datatype
idataNames2Go :: CGOptions -> IDataType -> GoTopLevelDecl
idataNames2Go opts (IDataType dname constrs) = GoTopLevelDecl
  (GoVarDecl [iqname2Go opts dname ++ "_names"] "[]string"
  [GoCompositeLit "[]string" (map getName constrs)])
 where
  getName ((_,name,_), _) = GoStringLit name

--- Creates a list of Go top-level function declarations
--- of constructor create functions for every constructor of a data type.
--- @param opts - compiler options
--- @param dtype - IDataType to generate create functions for
idata2GoCreate :: CGOptions -> IDataType -> [GoTopLevelDecl]
idata2GoCreate opts (IDataType name constrs) = 
  constr2GoCreate opts name 0 constrs


--- Creates a Go top-level function declaration
--- of a constructor create function for a constructor.
--- @param opts   - compiler options
--- @param dname  - datatype name
--- @param i      - index of the current constructor 
--- @param cnames - list of constructor names to generatre create functions for
constr2GoCreate :: CGOptions -> IQName -> Int -> [(IQName, IArity)] -> [GoTopLevelDecl]
constr2GoCreate _    _     _ []                          = []
constr2GoCreate opts dname i ((cname, n):xs) = (GoTopLevelFuncDecl 
  (GoFuncDecl ((iqname2Go opts cname) ++ "Create")
  [GoParam ["root"] ("*" ++ node)
  , GoParam ["args"] ("...*" ++ node)]
  [GoParam [] ("*" ++ node)]
  [GoExprStat (GoCall (GoOpName (runtime ++ ".ConstCreate")) 
  [root, GoIntLit i, GoIntLit n
  , GoUnaryExpr "&" 
    (GoIndex (GoOpName (iqname2Go opts dname ++ "_names")) (GoIntLit i))
  , GoVariadic (GoOpName "args")])
  , GoReturn [root]])):(constr2GoCreate opts dname (i+1) xs)

--- Creates a Go top-level function declaration
--- of a create function for an IFunction.
--- @param opts - compiler options
--- @param i    - index of the current function
--- @param func - list of IFunctions to generate create functions for
ifunc2GoCreate :: CGOptions -> Int -> [IFunction] -> [GoTopLevelDecl]
ifunc2GoCreate _    _ []                                  = []
ifunc2GoCreate opts i ((IFunction name n _ args _):xs) = (GoTopLevelFuncDecl
  (GoFuncDecl ((iqname2Go opts name) ++ "Create")
  [GoParam ["root"] ("*" ++ node)
  , GoParam ["args"] ("...*" ++ node)]
  [GoParam [] ("*" ++ node)]
  [GoExprStat (GoCall (GoOpName (runtime ++ ".FuncCreate"))
    [GoOpName "root", GoOpName (iqname2Go opts name)
    , GoUnaryExpr "&" (GoIndex (GoOpName "func_names") (GoIntLit i))
    , GoIntLit n, demandedArg, GoVariadic (GoOpName "args")])
  , GoReturn [root]])):(ifunc2GoCreate opts (i+1) xs)
 where
  demandedArg = case args of
    []    -> GoIntLit (-1)
    (x:_) -> GoIntLit x

--- Creates a Go top-level function declaration for an IFunction.
--- @param opts  - compiler options
--- @param ifunc - IFunction to compile
ifunc2Go :: CGOptions -> IFunction -> GoTopLevelDecl
ifunc2Go opts (IFunction name _ _ _ body) = GoTopLevelFuncDecl
  (GoFuncDecl (iqname2Go opts name) [GoParam ["task"] task]
  [] (ifuncbody2Go opts body))

--- Creates a list of Go statements for an IFuncBody.
--- @param opts      - compiler options
--- @param ifuncbody - IFuncBody to convert
ifuncbody2Go :: CGOptions -> IFuncBody -> [GoStat]
ifuncbody2Go _ (IExternal s)     = [GoExprStat 
  (GoCall (GoOpName ("External" ++ replaceInvalidChars s)) [GoOpName "task"])
  , GoReturn []]
ifuncbody2Go opts (IFuncBody block) = GoShortVarDecl ["root"] 
  [GoCall (GoSelector (GoOpName "task") "GetControl") []]
  : iblock2Go opts block

--- Creates a list of Go statements for an IBlock.
--- @param opts   - compiler options
--- @param iblock - IBlock to convert
iblock2Go :: CGOptions -> IBlock -> [GoStat]
iblock2Go opts (IBlock decls assigns stat) = map ivardecl2Go decls
  ++ map (iassign2Go opts) assigns
  ++ istatement2Go opts stat

--- Creates a Go statement for an IVarDecl.
ivardecl2Go :: IVarDecl -> GoStat
ivardecl2Go (IVarDecl i)  = GoVarDecl
  [varName i] ("*" ++ node) []
ivardecl2Go (IFreeDecl i) = GoShortVarDecl
  [varName i] [GoCall (GoOpName (runtime ++ ".FreeCreate")) [newNode]]

--- Creates a Go statement for an IAssign.
--- @param opts    - compiler options
--- @param iassign - IAssign to convert
iassign2Go :: CGOptions -> IAssign -> GoStat
iassign2Go opts (IVarAssign i expr)     = GoAssign
  [var i] "=" [iexpr2Go opts newNode expr]
iassign2Go opts (INodeAssign i ls expr) = GoExprStat 
  (GoCall (GoSelector (childAccess i (tail revLs)) "SetChild")
  [GoIntLit (head revLs),iexpr2Go opts newNode expr])
 where
  revLs = reverse ls

--- Creates a list of Go statements for an IStatement.
--- @param otps       - compiler options
--- @param istatement - IStatement to convert
istatement2Go :: CGOptions -> IStatement -> [GoStat]
istatement2Go opts  IExempt            = [GoExprStat
  (GoCall (GoOpName (runtime ++ ".ExemptCreate")) [root]), GoReturn []]
istatement2Go opts (IReturn expr)      = case expr of
 IVar i          -> [GoExprStat (GoCall
  (GoOpName (runtime ++ ".RedirectCreate")) [root, var i]), GoReturn []]
 IVarAccess i xs -> [GoExprStat (GoCall
  (GoOpName (runtime ++ ".RedirectCreate"))
  [root, childAccess i (reverse xs)]), GoReturn []]
 _               -> [GoExprStat (iexpr2Go opts root expr), GoReturn []]
istatement2Go opts (ICaseCons i cases) = [GoExprSwitch
  (GoCall (GoSelector (var i) "GetConstructor") [])
  ((GoExprBranch [GoIntLit (-1)] 
  [GoExprStat (GoCall (GoOpName (runtime ++ ".RedirectCreate"))
  [var i, createGenerator opts cases]), GoReturn []])
  :(map (iConsBranch2Go opts) cases))]
istatement2Go opts (ICaseLit i cases)  = 
  [GoIf (GoCall (GoSelector (var i) "IsFree") []) 
  [GoExprStat (GoCall (GoOpName (runtime ++ ".RedirectCreate"))
  [var i, createLitGenerator cases]), GoReturn []] []
  , iLitCases2Go opts i cases]

--- Creates a generator from a list of IConsBranches.
--- @param opts     - compiler options
--- @param branches - list of IConstBranches to base generator on
createGenerator :: CGOptions ->  [IConsBranch] -> GoExpr
createGenerator _ []                                 = error "Empty Cons list"
createGenerator opts [(IConsBranch name n _)]        = GoCall
  (GoOpName (iqname2Go opts name ++ "Create"))
  (newNode : replicate n (GoCall (GoOpName (runtime ++ ".FreeCreate"))
  [newNode]))
createGenerator opts (IConsBranch name n _:xs@(_:_)) = GoCall 
  (GoOpName (runtime ++ ".ChoiceCreate"))
  [newNode, GoCall (GoOpName ((iqname2Go opts name) ++ "Create"))
  (newNode : replicate n (GoCall (GoOpName (runtime ++ ".FreeCreate"))
  [newNode])), createGenerator opts xs]

--- Creates a Go expression branch for an IConsBranch.
--- @param opts   - compiler options
--- @param branch - IConsBranch to convert
iConsBranch2Go :: CGOptions -> IConsBranch -> GoExprBranch
iConsBranch2Go opts (IConsBranch (_, _, i) _ block) = GoExprBranch
  [GoIntLit i] (iblock2Go opts block)

--- Creates a Go expression switch statement for ILitCases.
--- @param opts  - compiler options
--- @param i     - switch variable index
--- @param cases - list of ILitCases
iLitCases2Go :: CGOptions -> Int -> [ILitBranch] -> GoStat
iLitCases2Go opts i cases@(c:_)   = GoExprSwitch 
  (GoCall (GoSelector (var i) getLit) [])
  (map (iLitBranch2Go opts) cases ++ [GoExprDefault 
  [GoExprStat (GoCall (GoOpName (runtime ++ ".ExemptCreate")) [root])]])
 where 
  getLit = case c of
    ILitBranch (IInt   _) _ -> "GetInt"
    ILitBranch (IChar  _) _ -> "GetChar"
    ILitBranch (IFloat _) _ -> "GetFloat"

--- Creates a generator from a list of ILitBranches.
createLitGenerator :: [ILitBranch] -> GoExpr
createLitGenerator cases = case cases of
  []                      -> error "Empty case list"
  [(ILitBranch lit _)]    -> litCreate lit
  ((ILitBranch lit _):xs) -> GoCall (GoOpName (runtime ++ ".ChoiceCreate"))
    [newNode, litCreate lit, createLitGenerator xs]
 where
  litCreate l = case l of
    IInt   i  -> GoCall 
      (GoOpName (runtime ++ ".IntLitCreate")) [newNode, GoIntLit i]
    IChar  c -> GoCall
      (GoOpName (runtime ++ ".CharLitCreate")) [newNode, GoByteLit c]
    IFloat f -> GoCall
      (GoOpName (runtime ++ ".FloatLitCreate")) [newNode, GoFloatLit f]

--- Creates a Go expression branch for an ILitBranch.
--- @param opts   - compiler options
--- @param branch - ILitBranch to convert
iLitBranch2Go :: CGOptions -> ILitBranch -> GoExprBranch
iLitBranch2Go opts (ILitBranch (IInt i) block)   = GoExprBranch 
  [GoIntLit i] (iblock2Go opts block)
iLitBranch2Go opts (ILitBranch (IChar c) block)  = GoExprBranch 
  [GoIntLit (ord c)] (iblock2Go opts block)
iLitBranch2Go opts (ILitBranch (IFloat f) block) = GoExprBranch 
  [GoFloatLit f] (iblock2Go opts block)

--- Creates a Go expression from an IExpr.
--- @param opts  - compiler options
--- @param expr  - Go operand to use as root for runtime function calls
--- @param iexpr - IExpr to convert
iexpr2Go :: CGOptions -> GoExpr -> IExpr -> GoExpr
iexpr2Go opts expr x = case x of
  (IVar i)               -> var i
  (IVarAccess i xs)      -> childAccess i (reverse xs)
  (ILit lit)             -> ilit2Go opts expr lit
  (IFCall name fargs)    -> createCall name fargs
  (ICCall name fargs)    -> createCall name fargs
  (IFPCall name _ fargs) -> createCall name fargs
  (ICPCall name _ fargs) -> createCall name fargs
  (IOr expr1 expr2)      -> GoCall
    (GoSelector (GoOpName runtime) "ChoiceCreate")
    [expr, iexpr2Go opts newNode expr1
    , iexpr2Go opts newNode expr2]
 where
  createCall name fargs  = GoCall
    (GoOpName (iqname2Go opts name ++ "Create"))
    (expr :(map (iexpr2Go opts newNode) fargs))

--- Creates a Go expression from an ILiteral.
--- @param expr  - Go operand to use as root for runtime function calls
--- @param iliteral - literal to convert
ilit2Go :: CGOptions -> GoExpr -> ILiteral -> GoExpr
ilit2Go _ expr (IInt i)   = GoCall
  (GoOpName (runtime ++ ".IntLitCreate")) [expr, GoIntLit i]
ilit2Go _ expr (IChar c)  = GoCall
  (GoOpName (runtime ++ ".CharLitCreate")) [expr, GoIntLit (ord c)]
ilit2Go _ expr (IFloat f) = GoCall
  (GoOpName (runtime ++ ".FloatLitCreate")) [expr, GoFloatLit f]

--- Creates a function name from an IQName, replacing invalid characters.
--- @param opts - compiler options
--- @param name - IQName to convert
iqname2Go :: CGOptions -> IQName -> String
iqname2Go opts (m, n, _) | m == "Prelude"    = "gocurry."
  ++ replaceInvalidChars ("Prelude_" ++ n)
                         | m /= modName opts = removeDots m 
  ++ "." ++ replaceInvalidChars (fstUp (removeDots m) ++ "_" ++ n)
                         | otherwise         = 
  replaceInvalidChars (fstUp (removeDots m) ++ "_" ++ n)
 where
  fstUp [] = []
  fstUp (x:xs) = (toUpper x) : xs

--- Replaces invalid characters in a string.
replaceInvalidChars :: String -> String
replaceInvalidChars [] = []
replaceInvalidChars (x : xs) 
  | x == '$'   = "Dol" ++ replaceInvalidChars xs
  | x == ')'   = "Rb" ++ replaceInvalidChars xs
  | x == '('   = "Lb" ++ replaceInvalidChars xs
  | x == '+'   = "Add" ++ replaceInvalidChars xs
  | x == ','   = "Comma" ++ replaceInvalidChars xs
  | x == '.'   = "_" ++ replaceInvalidChars xs
  | x == '#'   = "Hash" ++ replaceInvalidChars xs
  | x == '-'   = "Sub" ++ replaceInvalidChars xs
  | x == '*'   = "Mul" ++ replaceInvalidChars xs
  | x == '/'   = "Slash" ++ replaceInvalidChars xs
  | x == '%'   = "Percent" ++ replaceInvalidChars xs
  | x == '['   = "LSb" ++ replaceInvalidChars xs
  | x == ']'   = "RSb" ++ replaceInvalidChars xs
  | x == '{'   = "LCb" ++ replaceInvalidChars xs
  | x == '}'   = "RCb" ++ replaceInvalidChars xs
  | x == ':'   = "Col" ++ replaceInvalidChars xs
  | x == '^'   = "Pow" ++ replaceInvalidChars xs
  | x == '@'   = "At" ++ replaceInvalidChars xs
  | x == '!'   = "Excl" ++ replaceInvalidChars xs
  | x == '?'   = "Qstn" ++ replaceInvalidChars xs
  | x == '&'   = "And" ++ replaceInvalidChars xs
  | x == '='   = "Eq" ++ replaceInvalidChars xs
  | x == '<'   = "Lt" ++ replaceInvalidChars xs
  | x == '>'   = "Gt" ++ replaceInvalidChars xs
  | x == ';'   = "Semi" ++ replaceInvalidChars xs
  | x == '|'   = "Strt" ++ replaceInvalidChars xs
  | x == '\\'  = "BSlash" ++ replaceInvalidChars xs
  | x == '\''  = "SQuote" ++ replaceInvalidChars xs
  | x == '"'   = "DQuote" ++ replaceInvalidChars xs
  | x == '~'   = "Tilde" ++ replaceInvalidChars xs
  | x == '`'   = "Accent" ++ replaceInvalidChars xs
  | otherwise  = x : replaceInvalidChars xs

--- Creates a chain of child accesses.
--- Chain is in reverse order of the list.
--- The final access is done without dereferencing.
--- @param i    - root variable of access chain
--- @param l    - list of children to be accessed
childAccess :: Int -> [Int] -> GoExpr
childAccess i []           = var i
childAccess i [x]          = GoIndex
  (GoSelector (var i) "Children") (GoIntLit x)
childAccess i (x:xs@(_:_)) = GoIndex
  (GoSelector (recursiveChildAccess i xs) "Children") (GoIntLit x)

--- Creates a chain of child accesses.
--- Chain is in reverse order of the list.
--- @param i    - root variable of access chain
--- @param l    - list of children to be accessed
recursiveChildAccess :: Int -> [Int] -> GoExpr
recursiveChildAccess i []     = var i
recursiveChildAccess i (y:ys) = GoCall (GoSelector
  (recursiveChildAccess i ys) "GetChild") [GoIntLit y]

--- Removes dots from a String.
removeDots :: String -> String
removeDots [] = []
removeDots (x:xs) | x == '.'  = removeDots xs
                  | otherwise = x : (removeDots xs)
