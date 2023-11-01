------------------------------------------------------------------------------
--- This library defines a representation of a search space as
--- a tree and various search strategies on this tree.
--- This module implements **strong encapsulation** as discussed in
--- [the JFLP'04 paper](http://www.informatik.uni-kiel.de/~mh/papers/JFLP04_findall.html).
---
--- @author  Michael Hanus, Bjoern Peemoeller, Fabian Reck
--- @version December 2018
------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}

module Control.Search.SearchTree
  ( SearchTree (..), someSearchTree, getSearchTree
  , isDefined, showSearchTree, searchTreeSize, limitSearchTree
  , Strategy
  , dfsStrategy, bfsStrategy, idsStrategy, idsStrategyWith, diagStrategy
  , allValuesWith
  , allValuesDFS, allValuesBFS, allValuesIDS, allValuesIDSwith, allValuesDiag
  , ValueSequence, vsToList
  , getAllValuesWith, printAllValuesWith, printValuesWith
  , someValue, someValueWith
  ) where

import System.IO       ( hFlush, stdout )
import Data.List       ( diagonal )

#ifdef __PAKCS__
import Control.Search.Unsafe ( allValues )
#endif

--- A search tree is a value, a failure, or a choice between two search trees.
data SearchTree a = Value a
                  | Fail Int
                  | Or (SearchTree a) (SearchTree a)

--- A search strategy maps a search tree into some sequence of values.
--- Using the abtract type of sequence of values (rather than list of values)
--- enables the use of search strategies for encapsulated search
--- with search trees (strong encapsulation) as well as
--- with set functions (weak encapsulation).
type Strategy a = SearchTree a -> ValueSequence a

--- Returns the search tree for some expression.
getSearchTree :: a -> IO (SearchTree a)
getSearchTree x = return (someSearchTree x)

--- Internal operation to return the search tree for some expression.
--- Note that this operation is not purely declarative since
--- the ordering in the resulting search tree depends on the
--- ordering of the program rules.
---
--- Note that in PAKCS the search tree is just a degenerated tree
--- representing all values of the argument expression
--- and it is computed at once (i.e., not lazily!).
someSearchTree :: a -> SearchTree a
#ifdef __PAKCS__
someSearchTree = list2st . allValues
 where list2st []  = Fail 0
       list2st [x] = Value x
       list2st (x:y:ys) = Or (Value x) (list2st (y:ys))
#else
someSearchTree external
#endif

--- Returns True iff the argument is defined, i.e., has a value.
isDefined :: a -> Bool
isDefined x = hasValue (someSearchTree x)
 where hasValue y = case y of Value _  -> True
                              Fail _   -> False
                              Or t1 t2 -> hasValue t1 || hasValue t2

--- Shows the search tree as an intended line structure
showSearchTree :: Show a => SearchTree a -> String
showSearchTree st = showsST [] st ""
 where
  -- `showsST ctxt <SearchTree>`, where `ctxt` is a stack of boolean flags
  -- indicating whether we show the last alternative of the respective
  -- level to enable drawing aesthetical corners
  showsST ctxt (Value  a) = indent ctxt . shows a      . nl
  showsST ctxt (Fail _)   = indent ctxt . showChar '!' . nl
  showsST ctxt (Or t1 t2) = indent ctxt . showChar '?' . nl
                          . showsST (False : ctxt) t1
                          . showsST (True  : ctxt) t2

  indent []     = id
  indent (i:is) = showString (concatMap showIndent $ reverse is)
                . showChar   (if i then llc else lmc)
                . showString (hbar : " ")
    where showIndent isLast = (if isLast then ' ' else vbar) : "  "

  vbar = '\x2502' -- vertical bar
  hbar = '\x2500' -- horizontal bar
  llc  = '\x2514' -- left lower corner
  lmc  = '\x251c' -- left middle corner

  nl           = showChar '\n'
  shows x      = showString (show x)
  showChar c   = (c:)
  showString s = (s++)

-- showSearchTree st = showST 0 st ""
--  where
--   showST _ (Value a)  = showString "Value: " . shows a . nl
--   showST _ Fail       = showString "Fail"    . nl
--   showST i (Or t1 t2) = showString "Or "
--                       . showST i' t1 . tab i' . showST i' t2
--     where i'    = i + 1
--           tab j = showString $ replicate (3 * j) ' '


--- Returns the size (number of Value/Fail/Or nodes) of the search tree.
searchTreeSize :: SearchTree _ -> (Int, Int, Int)
searchTreeSize (Value _)  = (1, 0, 0)
searchTreeSize (Fail _)   = (0, 1, 0)
searchTreeSize (Or t1 t2) = let (v1, f1, o1) = searchTreeSize t1
                                (v2, f2, o2) = searchTreeSize t2
                             in (v1 + v2, f1 + f2, o1 + o2 + 1)

--- Limit the depth of a search tree. Branches which a depth larger
--- than the first argument are replace by `Fail (-1)`.
limitSearchTree :: Int -> SearchTree a -> SearchTree a
limitSearchTree _ v@(Value _) = v
limitSearchTree _ f@(Fail _)  = f
limitSearchTree n (Or t1 t2)  =
  if n<0 then Fail (-1)
         else Or (limitSearchTree (n-1) t1) (limitSearchTree (n-1) t2)

------------------------------------------------------------------------------
-- Definition of various search strategies:
------------------------------------------------------------------------------

--- Depth-first search strategy.
dfsStrategy :: Strategy a
dfsStrategy (Fail d)  = failVS d
dfsStrategy (Value x) = addVS x emptyVS
dfsStrategy (Or x y)  = dfsStrategy x |++| dfsStrategy y


------------------------------------------------------------------------------

--- Breadth-first search strategy.
bfsStrategy :: Strategy a
bfsStrategy t = allBFS [t]

allBFS :: [SearchTree a] -> ValueSequence a
allBFS []     = emptyVS
allBFS (t:ts) = values (t:ts) |++| allBFS (children (t:ts))

children :: [SearchTree a] -> [SearchTree a]
children []             = []
children (Fail _  : ts) = children ts
children (Value _ : ts) = children ts
children (Or x y  : ts) = x:y:children ts

-- Transforms a list of search trees into a value sequence where
-- choices are ignored.
values :: [SearchTree a] -> ValueSequence a
values []             = emptyVS
values (Fail d  : ts) = failVS d |++| values ts
values (Value x : ts) = addVS x (values ts)
values (Or _ _  : ts) = values ts


------------------------------------------------------------------------------

--- Iterative-deepening search strategy.
idsStrategy :: Strategy a
idsStrategy t = idsStrategyWith defIDSDepth defIDSInc t

--- The default initial search depth for IDS
defIDSDepth :: Int
defIDSDepth = 100

--- The default increasing function for IDS
defIDSInc :: Int -> Int
defIDSInc = (2*)

--- Parameterized iterative-deepening search strategy.
--- The first argument is the initial depth bound and
--- the second argument is a function to increase the depth in each
--- iteration.
idsStrategyWith :: Int -> (Int -> Int) -> Strategy a
idsStrategyWith initdepth incrdepth st =
  iterIDS initdepth (collectInBounds 0 initdepth st)
 where
  iterIDS _ Nil = emptyVS
  iterIDS n (Cons x xs) = addVS x (iterIDS n xs)
  iterIDS n (FCons fd xs) = failVS fd |++| iterIDS n xs
  iterIDS n Abort = let newdepth = incrdepth n
                     in iterIDS newdepth (collectInBounds n newdepth st)

-- Collect solutions within some level bounds in a tree.
collectInBounds :: Int -> Int -> SearchTree a -> AbortList a
collectInBounds oldbound newbound st = collectLevel newbound st
 where
  collectLevel d (Fail fd)  = if d <newbound-oldbound then FCons fd Nil else Nil
  collectLevel d (Value x) = if d<newbound-oldbound then Cons x Nil else Nil
  collectLevel d (Or x y)  =
    if d>0 then concA (collectLevel (d-1) x) (collectLevel (d-1) y)
           else Abort

-- List containing "aborts" are used to implement the iterative
-- depeening strategy:

data AbortList a = Nil | Cons a (AbortList a) | FCons Int (AbortList a) | Abort

-- Concatenation on abort lists where aborts are moved to the right.
concA :: AbortList a -> AbortList a -> AbortList a
concA Abort       Abort = Abort
concA Abort       Nil = Abort
concA Abort       (Cons x xs) = Cons x (concA Abort xs)
concA Abort       (FCons d xs) = FCons d (concA Abort xs)
concA Nil         ys = ys
concA (Cons x xs) ys = Cons x (concA xs ys)
concA (FCons d xs) ys = FCons d (concA xs ys)


------------------------------------------------------------------------------
-- Diagonalization search according to
-- J. Christiansen, S Fischer: EasyCheck - Test Data for Free (FLOPS 2008)

--- Diagonalization search strategy.
diagStrategy :: Strategy a
diagStrategy st = values (diagonal (levels [st]))

-- Enumerate all nodes of a forest of search trees in a level manner.
levels :: [SearchTree a] -> [[SearchTree a]]
levels st | null st   = []
          | otherwise = st : levels [ u | Or x y <- st, u <- [x,y] ]

------------------------------------------------------------------------------
-- Operations to map search trees into list of values.
------------------------------------------------------------------------------

--- Return all values in a search tree via some given search strategy.
allValuesWith :: Strategy a -> SearchTree a -> [a]
allValuesWith strategy searchtree = vsToList (strategy searchtree)

--- Return all values in a search tree via depth-first search.
allValuesDFS :: SearchTree a -> [a]
allValuesDFS = allValuesWith dfsStrategy

--- Return all values in a search tree via breadth-first search.
allValuesBFS :: SearchTree a -> [a]
allValuesBFS = allValuesWith bfsStrategy

--- Return all values in a search tree via iterative-deepening search.
allValuesIDS :: SearchTree a -> [a]
allValuesIDS = allValuesIDSwith defIDSDepth defIDSInc

--- Return all values in a search tree via iterative-deepening search.
--- The first argument is the initial depth bound and
--- the second argument is a function to increase the depth in each
--- iteration.
allValuesIDSwith :: Int -> (Int -> Int) -> SearchTree a -> [a]
allValuesIDSwith initdepth incrdepth =
  allValuesWith (idsStrategyWith initdepth incrdepth)

--- Return all values in a search tree via diagonalization search strategy.
allValuesDiag :: SearchTree a -> [a]
allValuesDiag = allValuesWith diagStrategy


--- Gets all values of an expression w.r.t. a search strategy.
--- A search strategy is an operation to traverse a search tree
--- and collect all values, e.g., 'dfsStrategy' or 'bfsStrategy'.
--- Conceptually, all values are computed on a copy of the expression,
--- i.e., the evaluation of the expression does not share any results.
getAllValuesWith :: Strategy a -> a -> IO [a]
getAllValuesWith strategy exp = do
  t <- getSearchTree exp
  return (vsToList (strategy t))


--- Prints all values of an expression w.r.t. a search strategy.
--- A search strategy is an operation to traverse a search tree
--- and collect all values, e.g., 'dfsStrategy' or 'bfsStrategy'.
--- Conceptually, all printed values are computed on a copy of the expression,
--- i.e., the evaluation of the expression does not share any results.
printAllValuesWith :: Show a => Strategy a -> a -> IO ()
printAllValuesWith strategy exp =
  getAllValuesWith strategy exp >>= mapM_ print


--- Prints the values of an expression w.r.t. a search strategy
--- on demand by the user. Thus, the user must type <ENTER> before
--- another value is computed and printed.
--- A search strategy is an operation to traverse a search tree
--- and collect all values, e.g., 'dfsStrategy' or 'bfsStrategy'.
--- Conceptually, all printed values are computed on a copy of the expression,
--- i.e., the evaluation of the expression does not share any results.
printValuesWith :: Show a => Strategy a -> a -> IO ()
printValuesWith strategy exp =
  getAllValuesWith strategy exp >>= printValues
 where
  printValues [] = return ()
  printValues (x:xs) = do
   putStr (show x)
   hFlush stdout
   _ <- getLine
   printValues xs

------------------------------------------------------------------------------
--- Returns some value for an expression.
---
--- Note that this operation is not purely declarative since
--- the computed value depends on the ordering of the program rules.
--- Thus, this operation should be used only if the expression
--- has a single value. It fails if the expression has no value.
someValue :: a -> a
someValue = someValueWith bfsStrategy

--- Returns some value for an expression w.r.t. a search strategy.
--- A search strategy is an operation to traverse a search tree
--- and collect all values, e.g., 'dfsStrategy' or 'bfsStrategy'.
---
--- Note that this operation is not purely declarative since
--- the computed value depends on the ordering of the program rules.
--- Thus, this operation should be used only if the expression
--- has a single value. It fails if the expression has no value.
someValueWith :: Strategy a -> a -> a
someValueWith strategy x = head (vsToList (strategy (someSearchTree x)))

------------------------------------------------------------------------------
--- The subsequent part defines a data structure for sequence of values
--- which is used in the implementation of search trees.
--- Using sequence of values (rather than standard lists of values)
--- is necessary to get the behavior of set functions
--- w.r.t. finite failures right, as described in the paper
---
--- > J. Christiansen, M. Hanus, F. Reck, D. Seidel:
--- > A Semantics for Weakly Encapsulated Search in Functional Logic Programs
--- > Proc. 15th International Conference on Principles and Practice
--- > of Declarative Programming (PPDP'13), pp. 49-60, ACM Press, 2013
---
--- Note that the implementation for PAKCS is simplified in order to provide
--- some functionality used by other modules.
--- In particular, the intended semantics of failures is not provided
--- in the PAKCS implementation.

--- A value sequence is an abstract sequence of values.
--- It also contains failure elements in order to implement the semantics
--- of set functions w.r.t. failures in the intended manner (only in KiCS2).
#ifdef __PAKCS__
data ValueSequence a = EmptyVS | ConsVS a (ValueSequence a)
#else
external data ValueSequence _ -- external
#endif

--- An empty sequence of values.
emptyVS :: ValueSequence a
#ifdef __PAKCS__
emptyVS = EmptyVS
#else
emptyVS external
#endif

--- Adds a value to a sequence of values.
addVS :: a -> ValueSequence a -> ValueSequence a
#ifdef __PAKCS__
addVS = ConsVS
#else
addVS external
#endif

--- Adds a failure to a sequence of values.
--- The argument is the encapsulation level of the failure.
failVS :: Int -> ValueSequence a
#ifdef __PAKCS__
failVS _ = EmptyVS -- cannot be implemented in PAKCS!"
#else
failVS external
#endif

--- Concatenates two sequences of values.
(|++|) :: ValueSequence a -> ValueSequence a -> ValueSequence a
#ifdef __PAKCS__
xs |++| ys = case xs of EmptyVS     -> ys
                        ConsVS z zs -> ConsVS z (zs |++| ys)
#else
(|++|) external
#endif

--- Transforms a sequence of values into a list of values.
vsToList :: ValueSequence a -> [a]
#ifdef __PAKCS__
vsToList EmptyVS       = []
vsToList (ConsVS x xs) = x : vsToList xs
#else
vsToList external
#endif

------------------------------------------------------------------------------
