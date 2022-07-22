------------------------------------------------------------------------------
--- This module contains a compiler from ICurry to Go programs.
---
--- @author Jonas Boehm (with modifications by Michael Hanus)
--- @version July 2021
------------------------------------------------------------------------------

module Curry2Go.Compiler
  ( CGOptions(..), defaultCGOptions, printVerb
  , SearchStrat(..), compileIProg2GoString, compileIProg2Go
  , createMainProg, modName2Go)
 where

import Prelude hiding ( empty )
import Control.Monad     ( when )
import Data.Char         ( toUpper )
import Data.List         ( init )

import Data.Set
import Debug.Profile    -- for show run-time
import ICurry.Types
import Language.Go.ShowS ( showGoProg )
import Language.Go.Types
import System.CurryPath

import CompilerStructure ( printWithElapsedTime )

--- Data types ---

--- Data type for compiler options.
data CGOptions = CGOptions
  { verbosity    :: Int          -- verbosity (0..4)
                                 -- (0: quiet, 1: status,...)
  , modName      :: String       -- used internally to track main module. not configurable
  , optCTime     :: Bool         -- print compile messages with elapsed time?
  , optErrDepth  :: Int          -- depth of expressions in error messages
  , optGenMain   :: Bool         -- should a main method be generated
  , optHNF       :: Bool         -- only compute hnf
  , optInteract  :: Bool         -- interactive result printing
  , optMainName  :: String       -- name of the function to run as main
  , optMaxTasks  :: Int          -- maximum number of concurrent tasks in a fair search
  , optNoBanner  :: Bool         -- do not print banner on start up
  , optNoImports :: Bool         -- compile only main module (i.e., the
                                 -- imported modules are already compiled)
  , optParser    :: String       -- additional options for the parser
  , optPrintHelp :: Bool         -- should help text be displayed
  , optResults   :: Int          -- maximum number of results to compute
  , optRun       :: Bool         -- run the program after compilation
  , optRuns      :: Int          -- number of runs to average execution time
  , optStrat     :: SearchStrat  -- search strategy to be used by the run-time system
  , optTime      :: Bool         -- measure execution time
  , printName    :: Bool         -- print compiler name
  , printNumVer  :: Bool         -- print numeric version
  , printBaseVer :: Bool         -- print base version
  }

--- Default options.
defaultCGOptions :: CGOptions
defaultCGOptions = CGOptions 
  { verbosity    = 1
  , modName      = ""
  , optCTime     = False
  , optErrDepth  = 0
  , optGenMain   = True
  , optHNF       = False
  , optInteract  = False
  , optMainName  = "main"
  , optMaxTasks  = 0
  , optNoBanner  = False
  , optNoImports = False
  , optParser    = ""
  , optPrintHelp = False
  , optResults   = 0
  , optRun       = False
  , optRuns      = 1
  , optStrat     = DFS
  , optTime      = False
  , printName    = False
  , printNumVer  = False
  , printBaseVer = False
  }

printVerb :: CGOptions -> Int -> String -> IO ()
printVerb opts v s =
  when (verbosity opts >= v) $ printWithElapsedTime (optCTime opts) s

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
newNode = GoCall (GoSelector (var 0) "NewNode") []

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
  | fname == optMainName opts && ar > 0 = error $ "Overloaded initial expression"
  | fname == optMainName opts = GoProg "main"
  [runtime, "curry2go/" ++ modNameToPath modName]
  [GoTopLevelFuncDecl (GoFuncDecl "main" [] []
  [GoShortVarDecl ["node"] [GoCall (GoOpName (iqname2GoCreate opts name))
  [GoCall (GoOpName "new") [GoOpName node]]]
  , GoExprStat (GoCall
    (GoOpName (runtime ++ if optTime opts then ".Benchmark" else ".Evaluate"))
    ((if optTime opts then [GoOpName "node", GoIntLit (optRuns opts)]
                      else [GoOpName "node", GoBoolLit (optInteract opts)]) ++
     [GoBoolLit (optHNF opts), GoOpName (runtime ++ "." ++ (show (optStrat opts)))
    , GoIntLit (optResults opts), GoIntLit (optMaxTasks opts), GoIntLit (optErrDepth opts)]))])]
 | otherwise = createMainProg xs opts

--- Creates a string in Go syntax from an IProg.
--- @param opts     - compiler options
--- @param iprog    - IProg to convert
compileIProg2GoString :: CGOptions -> IProg -> String
compileIProg2GoString opts iprog@(IProg moduleName _ _ _) =
  showGoProg (compileIProg2Go (opts {modName = moduleName}) iprog) ""

--- Creates a Go program from an `IProg`.
--- @param opts     - compiler options
--- @param iprog    - IProg to convert
compileIProg2Go :: CGOptions -> IProg -> GoProg
compileIProg2Go opts (IProg moduleName _ dtypes funcs) =
  GoProg (modName2Go moduleName)
  (runtime : getFuncsImports opts funcs)
  ([ifuncNames2Go opts funcs]
  ++ map (idataNames2Go opts) dtypes
  ++ concatMap (idata2GoCreate (opts {modName = moduleName})) dtypes
  ++ ifunc2GoCreate (opts {modName = moduleName}) 0 funcs
  ++ map (ifunc2Go (opts {modName = moduleName})) funcs)

--- Extracts the modules used by a list of `IFunction`s.
getFuncsImports :: CGOptions -> [IFunction] -> [String]
getFuncsImports opts ifuncs =
  toImport (toList (unionize (map getImportsFunc ifuncs)))
 where
  getImportsFunc (IFunction _ _ _ _ body) = getImportsBody body

  getImportsBody (IExternal _)     = empty
  getImportsBody (IFuncBody block) = getImportsBlock block

  getImportsBlock (IBlock _ assign stat) =
    union (getImportsStat stat) (unionize (map getImportsAssign assign))

  getImportsAssign (IVarAssign _ expr)    = getImportsExpr expr
  getImportsAssign (INodeAssign _ _ expr) = getImportsExpr expr

  getImportsStat IExempt                = empty
  getImportsStat (IReturn expr)         = getImportsExpr expr
  getImportsStat (ICaseCons _ branches) =
    unionize (map getImportsCons branches)
  getImportsStat (ICaseLit _ branches)  =
    unionize (map getImportsLit branches)

  getImportsCons (IConsBranch (m, _, _) _ block) =
    insert m (getImportsBlock block)

  getImportsLit (ILitBranch _ block) = getImportsBlock block

  getImportsExpr (IVar _)                             = empty
  getImportsExpr (IVarAccess _ _)                     = empty
  getImportsExpr (ILit _)                             = empty
  getImportsExpr (IFCall (m, _, _) exprs)             =
    insert m (unionize (map getImportsExpr exprs))
  getImportsExpr (ICCall (m, n, _) exprs)
    | n == ":"
    = case exprs of
        (ILit (IChar _):[ICCall (_,"[]",_) _])   -> empty
        (ILit (IChar _):xs@[ICCall (_,":",_) _]) ->
          unionize (map getImportsExpr xs)
        (ILit (IChar _):r) -> insert m (unionize (map getImportsExpr r))
        _                  -> insert m (unionize (map getImportsExpr exprs))
    | otherwise
    = insert m (unionize (map getImportsExpr exprs))
  getImportsExpr (IFPCall (m, _, _) _ exprs)          =
    insert m (unionize (map getImportsExpr exprs))
  getImportsExpr (ICPCall (m, _, _) _ exprs)          =
    insert m (unionize (map getImportsExpr exprs))
  getImportsExpr (IOr expr1 expr2)                    =
    union (getImportsExpr expr1) (getImportsExpr expr2)

  unionize ls = foldr union empty ls

  toImport [] = []
  toImport (x:xs)
    | x == modName opts = toImport xs
    | otherwise         = ("curry2go/" ++ modNameToPath x) : toImport xs

--- Creates a top-level declaration for an array with all function names.
--- @param opts  - compiler options
--- @param funcs - list of IFunctions
ifuncNames2Go :: CGOptions -> [IFunction] -> GoTopLevelDecl
ifuncNames2Go _ funcs = GoTopLevelDecl 
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
constr2GoCreate :: CGOptions -> IQName -> Int -> [(IQName, IArity)]
                -> [GoTopLevelDecl]
constr2GoCreate _    _     _ []                          = []
constr2GoCreate opts dname i ((cname, n):xs) = (GoTopLevelFuncDecl 
  (GoFuncDecl (iqname2GoCreate opts cname)
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
  (GoFuncDecl (iqname2GoCreate opts name)
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
istatement2Go _    IExempt            = [GoExprStat
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
  [GoIf (GoCall (GoSelector (GoOpName "task") "IsBound") [var i])
  [GoExprStat (GoCall (GoSelector (GoOpName "task") "ToHnf") [var i])
  , GoReturn []] []
  , GoExprStat (GoCall (GoSelector (var i) "SetTrLock")
  [GoCall (GoSelector (GoOpName "task") "GetId") []
  , createGenerator opts cases]), GoReturn []])
  :(map (iConsBranch2Go opts) cases))]
istatement2Go opts (ICaseLit i cases)  = 
  [GoIf (GoCall (GoSelector (var i) "IsFree") []) 
  [GoIf (GoCall (GoSelector (GoOpName "task") "IsBound") [var i])
  [GoExprStat (GoCall (GoSelector (GoOpName "task") "ToHnf") [var i])
  , GoReturn []] []
  , GoExprStat (GoCall (GoSelector (var i) "SetTrLock")
  [GoCall (GoSelector (GoOpName "task") "GetId") []
  , createLitGenerator cases]), GoReturn []] []
  , iLitCases2Go opts i cases]

--- Creates a generator from a list of IConsBranches.
--- @param opts     - compiler options
--- @param branches - list of IConstBranches to base generator on
createGenerator :: CGOptions ->  [IConsBranch] -> GoExpr
createGenerator _ []                                 = error "Empty Cons list"
createGenerator opts [(IConsBranch name n _)]        = GoCall
  (GoOpName (iqname2GoCreate opts name))
  ((GoCall (GoSelector (GoOpName "task") "NewNode") [])
  : replicate n (GoCall (GoOpName (runtime ++ ".FreeCreate"))
  [(GoCall (GoSelector (GoOpName "task") "NewNode") [])]))
createGenerator opts (IConsBranch name n _:xs@(_:_)) = GoCall 
  (GoOpName (runtime ++ ".ChoiceCreate"))
  [(GoCall (GoSelector (GoOpName "task") "NewNode") [])
  , GoCall (GoOpName (iqname2GoCreate opts name))
  ((GoCall (GoSelector (GoOpName "task") "NewNode") [])
  : replicate n (GoCall (GoOpName (runtime ++ ".FreeCreate"))
  [(GoCall (GoSelector (GoOpName "task") "NewNode") [])]))
  , createGenerator opts xs]

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
iLitCases2Go _ _ [] =
  error "ICurry.Compiler.itLitCases2Go: case literal without branches occurred"
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
    [(GoCall (GoSelector (GoOpName "task") "NewNode") [])
    , litCreate lit, createLitGenerator xs]
 where
  litCreate l = case l of
    IInt   i  -> GoCall 
      (GoOpName (runtime ++ ".IntLitCreate"))
      [(GoCall (GoSelector (GoOpName "task") "NewNode") []), GoIntLit i]
    IChar  c -> GoCall
      (GoOpName (runtime ++ ".CharLitCreate"))
      [(GoCall (GoSelector (GoOpName "task") "NewNode") []), GoByteLit c]
    IFloat f -> GoCall
      (GoOpName (runtime ++ ".FloatLitCreate"))
      [(GoCall (GoSelector (GoOpName "task") "NewNode") []), GoFloatLit f]

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
  (ICCall name@(_,":",_) fargs@(ILit (IChar _):_)) -> createString name fargs
  (ICCall name fargs)    -> createCall name fargs
  (IFPCall name _ fargs) -> createCall name fargs
  (ICPCall name _ fargs) -> createCall name fargs
  (IOr expr1 expr2)      -> GoCall
    (GoSelector (GoOpName runtime) "ChoiceCreate")
    [expr, iexpr2Go opts newNode expr1
    , iexpr2Go opts newNode expr2]
 where
  createCall name fargs   = GoCall
    (GoOpName (iqname2GoCreate opts name))
    (expr :(map (iexpr2Go opts newNode) fargs))
  createString name fargs = case extractString fargs "" of
    Just s  -> GoCall (GoOpName (runtime ++ ".StringCreate")) [expr
      , GoCall (GoOpName (runtime ++ ".ParseString")) [GoStringLit s]]
    Nothing -> createCall name fargs
  extractString fargs str = case fargs of
    [ICCall (_,"[]",_) _] -> Just str
    [ICCall (_,":",_) r]  -> extractString r str
    (ILit (IChar c):r)    -> extractString r (str ++ tail (init (show [c])))
    _                     -> Nothing

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

--- Maps an IQName into the name of a Go function name to create the entity.
--- @param opts - compiler options
--- @param name - IQName to convert
iqname2GoCreate :: CGOptions -> IQName -> String
iqname2GoCreate opts (m,n,i) = 
  (iqname2Go opts (m, "", i)) ++ "_CREATE_" ++ replaceInvalidChars n

--- Maps an IQName into a Go function name, replacing invalid characters.
--- @param opts - compiler options
--- @param name - IQName to convert
iqname2Go :: CGOptions -> IQName -> String
iqname2Go opts (m, n, _)
  | m /= modName opts
  = modName2Go m ++ "." ++ 
    (fstUp (modName2Go m) ++ "_" ++ replaceInvalidChars n)
  | otherwise
  = (fstUp (modName2Go m) ++ "_" ++ replaceInvalidChars n)
 where
  fstUp []     = []
  fstUp (x:xs) = toUpper x : xs
  
--- Maps a Module name into a Go package name.
modName2Go :: String -> String
modName2Go [] = []
modName2Go (x:xs) | x == '.'  = "_DOT_" ++ modName2Go xs
                  | x == '_'  = "_US_" ++ modName2Go xs
                  | otherwise = x : modName2Go xs

--- Replaces invalid characters in a string.
replaceInvalidChars :: String -> String
replaceInvalidChars = concatMap replaceInvalidChar
 where
  replaceInvalidChar x
    | x == '$'   = "Dol_"
    | x == ')'   = "Rb_"
    | x == '('   = "Lb_"
    | x == '+'   = "Add_"
    | x == ','   = "Comma_"
    | x == '.'   = "Dot_"
    | x == '#'   = "Hash_"
    | x == '-'   = "Sub_"
    | x == '*'   = "Mul_"
    | x == '/'   = "Slash_"
    | x == '%'   = "Percent_"
    | x == '['   = "LSb_"
    | x == ']'   = "RSb_"
    | x == '{'   = "LCb_"
    | x == '}'   = "RCb_"
    | x == ':'   = "Col_"
    | x == '^'   = "Pow_"
    | x == '@'   = "At_"
    | x == '!'   = "Excl_"
    | x == '?'   = "Qstn_"
    | x == '&'   = "And_"
    | x == '='   = "Eq_"
    | x == '<'   = "Lt_"
    | x == '>'   = "Gt_"
    | x == ';'   = "Semi_"
    | x == '|'   = "Strt_"
    | x == '\\'  = "BSlash_"
    | x == '\''  = "SQuote_"
    | x == '"'   = "DQuote_"
    | x == '~'   = "Tilde_"
    | x == '`'   = "Accent_"
    | x == '_'   = "Us_"
    | otherwise  = [x]

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
