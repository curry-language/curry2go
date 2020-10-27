module Go.Types where

--- Data types representing Go programs.
--- The types were based on the golang specification,
--- which can be found here: golang.org/ref/spec

--- Go types are represented as Strings.
type Type = String

--- Go identificators are represented as Strings.
type Id = String

--- Go operators are represented as Strings.
type Op = String

--- Type of Go expressions.
--- @Cons GoBoolLit      - boolean literal
--- @Cons GoIntLit       - integer literal
--- @Cons GoFloatLit     - float literal
--- @Cons GoStringLit    - string literal
--- @Cons GoByteLit      - byte literal
--- @Cons GoCompositeLit - composite literal
--- @Cons GoOpName       - (qualified) identifier
--- @Cons GoOpExpr       - expression placed as operand
--- @Cons GoConversion   - conversion of an expression to a new type
--- @Cons GoSelector     - selects a field of an expression
--- @Cons GoIndex        - indexed access to an array
--- @Cons GoSlice        - slicing of a string or array
--- @Cons GoVariadic     - variadic expression
--- @Cons GoCall         - function call
--- @Cons GoUnaryExpr    - application of an unary operator
--- @Cons GoBinaryExpr   - application of a binary operator
data GoExpr = GoBoolLit Bool
            | GoIntLit Int
            | GoFloatLit Float
            | GoStringLit String
            | GoByteLit Char
            | GoCompositeLit Type [GoExpr]
            | GoOpName Id
            | GoOpExpr GoExpr
            | GoConversion Type GoExpr
            | GoSelector GoExpr Id
            | GoIndex GoExpr GoExpr
            | GoSlice GoExpr GoExpr GoExpr
            | GoVariadic GoExpr
            | GoCall GoExpr [GoExpr]
            | GoUnaryExpr Op GoExpr
            | GoBinaryExpr GoExpr Op GoExpr

--- Type of Go statements.
--- @Cons GoConstDecl    - declaration of a constant
--- @Cons GoVarDecl      - declaration of a variable
--- @Cons GoShortVarDecl - short variable declaration
--- @Cons GoExprStat     - expression as a statement
--- @Cons GoAssign       - assignment
--- @Cons GoEmpty        - empty statement
--- @Cons GoReturn       - return statement
--- @Cons GoBreak        - break statement
--- @Cons GoContinue     - continue statement
--- @Cons GoBlock        - block of statements
--- @Cons GoIf           - if statement
--- @Cons GoExprSwitch   - expression switch statement
data GoStat = GoConstDecl [Id] Type [GoExpr]
            | GoVarDecl [Id] Type [GoExpr]
            | GoShortVarDecl [Id] [GoExpr]
            | GoExprStat GoExpr
            | GoAssign [GoExpr] Op [GoExpr]
            | GoEmpty
            | GoReturn [GoExpr]
            | GoBreak
            | GoContinue
            | GoBlock [GoStat]
            | GoIf GoExpr [GoStat] [GoStat]
            | GoExprSwitch GoExpr [GoExprBranch]

--- Type representing a branch of an expression switch statement.
--- @Cons GoExprBranch  - case triggered by expressions
--- @Cons GoExprDefault - default case
data GoExprBranch = GoExprBranch [GoExpr] [GoStat]
                  | GoExprDefault [GoStat]

--- Type of Go program.
--- Arguments are name, imports, top-level declarations.
data GoProg = GoProg String [String] [GoTopLevelDecl]

--- Type of Go top-level declarations.
--- @Cons GoTopLevelDecl     - top-level declaration
--- @Cons GoTopLevelFuncDecl - top-level function declaration
data GoTopLevelDecl = GoTopLevelDecl GoStat
                    | GoTopLevelFuncDecl GoFuncDecl

--- Type of Go function declaration.
--- Arguments are name, parameters, return values, body.
data GoFuncDecl = GoFuncDecl Id [GoParam] [GoParam] [GoStat]

--- Type of Go parameter
data GoParam = GoParam [Id] Type
