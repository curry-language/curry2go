--- -----------------------------------------------------------------
--- This module is a modified version of the module
--- `System.Console.GetOpt` by Sven Panne from the ghc-base package.
--- It has been adapted for Curry by Bjoern Peemoeller
---
--- (c) Sven Panne 2002-2005

--- The Glasgow Haskell Compiler License
---
--- Copyright 2004, The University Court of the University of Glasgow.
--- All rights reserved.
---
--- Redistribution and use in source and binary forms, with or without
--- modification, are permitted provided that the following conditions are met:
---
--- - Redistributions of source code must retain the above copyright notice,
--- this list of conditions and the following disclaimer.
---
--- - Redistributions in binary form must reproduce the above copyright notice,
--- this list of conditions and the following disclaimer in the documentation
--- and/or other materials provided with the distribution.
---
--- - Neither name of the University nor the names of its contributors may be
--- used to endorse or promote products derived from this software without
--- specific prior written permission.
---
--- THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY COURT OF THE UNIVERSITY OF
--- GLASGOW AND THE CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
--- INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
--- FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
--- UNIVERSITY COURT OF THE UNIVERSITY OF GLASGOW OR THE CONTRIBUTORS BE LIABLE
--- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
--- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
--- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
--- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
--- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
--- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
--- DAMAGE.
---
--- @category general
--- ---------------------------------------------------------------------------



{-
Two rather obscure features are missing: The Bash 2.0 non-option hack
(if you don't already know it, you probably don't want to hear about
it...) and the recognition of long options with a single dash
(e.g. '-help' is recognised as '--help', as long as there is no short
option 'h').

Other differences between GNU's getopt and this implementation:

* To enforce a coherent description of options and arguments, there
  are explanation fields in the option/argument descriptor.

* Error messages are now more informative, but no longer POSIX
  compliant... :-(
-}

module System.Console.GetOpt
  -- * GetOpt
  ( getOpt, getOpt', usageInfo, ArgOrder (..), OptDescr (..), ArgDescr (..)

   -- * Examples

   -- |To hopefully illuminate the role of the different data structures,
   -- here are the command-line options for a (very simple) compiler,
   -- done in two different ways.
   -- The difference arises because the type of 'getOpt' is
   -- parameterized by the type of values derived from flags.

   -- ** Interpreting flags as concrete values
   -- $example1

   -- ** Interpreting flags as transformations of an options record
   -- $example2
  ) where

import Prelude -- necessary to get dependencies right
import Data.List (isPrefixOf, find)

-- |What to do with options following non-options
data ArgOrder a
  = RequireOrder                -- ^ no option processing after first non-option
  | Permute                     -- ^ freely intersperse options and non-options
  | ReturnInOrder (String -> a) -- ^ wrap non-options into options

{-|
Each 'OptDescr' describes a single option.

The arguments to 'Option' are:

* list of short option characters

* list of long option strings (without \"--\")

* argument descriptor

* explanation of option for user
-}
data OptDescr a =              -- description of a single options:
   Option [Char]                --    list of short option characters
          [String]              --    list of long option strings (without "--")
          (ArgDescr a)          --    argument descriptor
          String                --    explanation of option for user

-- |Describes whether an option takes an argument or not, and if so
-- how the argument is injected into a value of type @a@.
data ArgDescr a
   = NoArg                   a         -- ^no argument expected
   | ReqArg (String       -> a) String -- ^option requires argument
   | OptArg (Maybe String -> a) String -- ^optional argument

data OptKind a                 -- kind of cmd line arg (internal use only):
  = Opt       a                -- an option
  | UnreqOpt  String           -- an un-recognized option
  | NonOpt    String           -- a non-option
  | EndOfOpts                  -- end-of-options marker (i.e. "--")
  | OptErr    String           -- something went wrong...

-- | Return a string describing the usage of a command, derived from
-- the header (first argument) and the options described by the
-- second argument.
usageInfo :: String                    -- header
          -> [OptDescr a]              -- option descriptors
          -> String                    -- nicely formatted decription of options
usageInfo header optDescr = unlines (header:table) where
  (ss,ls,ds)     = (unzip3 . concatMap fmtOpt) optDescr
  table          = zipWith3 paste (sameLen ss) (sameLen ls) ds
  paste x y z    = "  " ++ x ++ "  " ++ y ++ "  " ++ z
  sameLen xs     = flushLeft ((maximum . map length) xs) xs
  flushLeft n xs = [ take n (x ++ repeat ' ') | x <- xs ]

maximum :: Ord a => [a] -> a
maximum [] = error "maximum with empty list"
maximum xs@(_:_) = foldl1 max xs

fmtOpt :: OptDescr a -> [(String,String,String)]
fmtOpt (Option sos los ad descr) =
  case lines descr of
    []     -> [(sosFmt,losFmt,"")]
    (d:ds) ->  (sosFmt,losFmt,d) : [ ("","",d') | d' <- ds ]
  where sepBy _  []       = ""
        sepBy _  [x]      = x
        sepBy ch (x:y:xs) = x ++ ch : ' ' : sepBy ch (y:xs)
        sosFmt = sepBy ',' (map (fmtShort ad) sos)
        losFmt = sepBy ',' (map (fmtLong  ad) los)

fmtShort :: ArgDescr a -> Char -> String
fmtShort (NoArg  _   ) so = "-" ++ [so]
fmtShort (ReqArg _ ad) so = "-" ++ [so] ++ " " ++ ad
fmtShort (OptArg _ ad) so = "-" ++ [so] ++ "[" ++ ad ++ "]"

fmtLong :: ArgDescr a -> String -> String
fmtLong (NoArg  _   ) lo = "--" ++ lo
fmtLong (ReqArg _ ad) lo = "--" ++ lo ++ "=" ++ ad
fmtLong (OptArg _ ad) lo = "--" ++ lo ++ "[=" ++ ad ++ "]"

{-|
Process the command-line, and return the list of values that matched
(and those that didn\'t). The arguments are:

* The order requirements (see 'ArgOrder')

* The option descriptions (see 'OptDescr')

* The actual command line arguments (presumably got from
  'System.Environment.getArgs').

'getOpt' returns a triple consisting of the option arguments, a list
of non-options, and a list of error messages.
-}
getOpt :: ArgOrder a                   -- non-option handling
       -> [OptDescr a]                 -- option descriptors
       -> [String]                     -- the command-line arguments
       -> ([a],[String],[String])      -- (options,non-options,error messages)
getOpt ordering optDescr args = (os,xs,es ++ map errUnrec us)
   where (os,xs,us,es) = getOpt' ordering optDescr args

{-|
This is almost the same as 'getOpt', but returns a quadruple
consisting of the option arguments, a list of non-options, a list of
unrecognized options, and a list of error messages.
-}
getOpt' :: ArgOrder a                         -- non-option handling
        -> [OptDescr a]                       -- option descriptors
        -> [String]                           -- the command-line arguments
        -> ([a],[String], [String] ,[String]) -- (options,non-options,unrecognized,error messages)
getOpt' _        _        []         =  ([],[],[],[])
getOpt' ordering optDescr (arg:args) = procNextOpt opt ordering where
  procNextOpt (Opt o)      _                 = (o:os,xs,us,es)
  procNextOpt (UnreqOpt u) _                 = (os,xs,u:us,es)
  procNextOpt (NonOpt x)   RequireOrder      = ([],x:rest,[],[])
  procNextOpt (NonOpt x)   Permute           = (os,x:xs,us,es)
  procNextOpt (NonOpt x)   (ReturnInOrder f) = (f x :os, xs,us,es)
  procNextOpt EndOfOpts    RequireOrder      = ([],rest,[],[])
  procNextOpt EndOfOpts    Permute           = ([],rest,[],[])
  procNextOpt EndOfOpts    (ReturnInOrder f) = (map f rest,[],[],[])
  procNextOpt (OptErr e)   _                 = (os,xs,us,e:es)

  (opt,rest) = getNext arg args optDescr
  (os,xs,us,es) = getOpt' ordering optDescr rest

-- take a look at the next cmd line arg and decide what to do with it
getNext :: String -> [String] -> [OptDescr a] -> (OptKind a,[String])
getNext s rest optDescr = case s of
  '-':'-':[] -> (EndOfOpts, rest)
  '-':'-':xs -> longOpt xs rest optDescr
  '-': x :xs -> shortOpt x xs rest optDescr
  _          -> (NonOpt s,rest)

-- handle long option
longOpt :: String -> [String] -> [OptDescr a] -> (OptKind a,[String])
longOpt ls rs optDescr = long ads arg rs where
  (opt,arg) = break (=='=') ls
  getWith p = [ o | o@(Option _ xs _ _) <- optDescr
                  , find (p opt) xs /= Nothing ]
  exact     = getWith (==)
  options   = if null exact then getWith isPrefixOf else exact
  ads       = [ ad | Option _ _ ad _ <- options ]
  optStr    = ("--"++opt)

  long ads0 arg0 rs0 = case (ads0, arg0, rs0) of
    ((_:_:_)     , _       , rest    ) -> (errAmbig options optStr,rest)
    ([NoArg  a  ], []      , rest    ) -> (Opt a                  ,rest)
    ([NoArg  _  ], ('=':_) , rest    ) -> (errNoArg optStr        ,rest)
    ([ReqArg _ d], []      , []      ) -> (errReq d optStr        ,[]  )
    ([ReqArg f _], []      , (r:rest)) -> (Opt (f r)              ,rest)
    ([ReqArg f _], ('=':xs), rest    ) -> (Opt (f xs)             ,rest)
    ([OptArg f _], []      , rest    ) -> (Opt (f Nothing)        ,rest)
    ([OptArg f _], ('=':xs), rest    ) -> (Opt (f (Just xs))      ,rest)
    (_           , _       , rest    ) -> (UnreqOpt ("--" ++ ls)  ,rest)

-- handle short option
shortOpt :: Char -> String -> [String] -> [OptDescr a] -> (OptKind a,[String])
shortOpt y ys rs optDescr = short ads ys rs where
  options = [ o  | o@(Option ss _ _ _) <- optDescr, s <- ss, y == s ]
  ads     = [ ad | Option _ _ ad _ <- options ]
  optStr  = '-':[y]

  short []           []       rest     = (UnreqOpt optStr,rest)
  short []           xs@(_:_) rest     = (UnreqOpt optStr,('-':xs):rest)
  short [NoArg  a  ] []       rest     = (Opt a,rest)
  short [NoArg  a  ] xs@(_:_) rest     = (Opt a,('-':xs):rest)
  short [ReqArg _ d] []       []       = (errReq d optStr,[])
  short [ReqArg f _] []       (r:rest) = (Opt (f r),rest)
  short [ReqArg f _] xs@(_:_) rest     = (Opt (f xs),rest)
  short [OptArg f _] []       rest     = (Opt (f Nothing),rest)
  short [OptArg f _] xs@(_:_) rest     = (Opt (f (Just xs)),rest)
  short (_:_:_)      _        rest     = (errAmbig options optStr,rest)

-- miscellaneous error formatting

errAmbig :: [OptDescr a] -> String -> OptKind a
errAmbig ods optStr = OptErr (usageInfo header ods) where
  header = "option `" ++ optStr ++ "' is ambiguous; could be one of:"

errReq :: String -> String -> OptKind a
errReq d optStr = OptErr ("option `" ++ optStr ++ "' requires an argument " ++ d ++ "\n")

errUnrec :: String -> String
errUnrec optStr = "unrecognized option `" ++ optStr ++ "'\n"

errNoArg :: String -> OptKind a
errNoArg optStr = OptErr ("option `" ++ optStr ++ "' doesn't allow an argument\n")

{-
-----------------------------------------------------------------------------------------
-- and here a small and hopefully enlightening example:

data Flag = Verbose | Version | Name String | Output String | Arg String   deriving Show

options :: [OptDescr Flag]
options =
   [Option ['v']     ["verbose"]           (NoArg Verbose)      "verbosely list files",
    Option ['V','?'] ["version","release"] (NoArg Version)      "show version info",
    Option ['o']     ["output"]            (OptArg out "FILE")  "use FILE for dump",
    Option ['n']     ["name"]              (ReqArg Name "USER") "only dump USER's files"]

out :: Maybe String -> Flag
out Nothing  = Output "stdout"
out (Just o) = Output o

test :: ArgOrder Flag -> [String] -> String
test order cmdline = case getOpt order options cmdline of
                        (o,n,[]  ) -> "options=" ++ show o ++ "  args=" ++ show n ++ "\n"
                        (_,_,errs) -> concat errs ++ usageInfo header options
   where header = "Usage: foobar [OPTION...] files..."

-- example runs:
-- putStr (test RequireOrder ["foo","-v"])
--    ==> options=[]  args=["foo", "-v"]
-- putStr (test Permute ["foo","-v"])
--    ==> options=[Verbose]  args=["foo"]
-- putStr (test (ReturnInOrder Arg) ["foo","-v"])
--    ==> options=[Arg "foo", Verbose]  args=[]
-- putStr (test Permute ["foo","--","-v"])
--    ==> options=[]  args=["foo", "-v"]
-- putStr (test Permute ["-?o","--name","bar","--na=baz"])
--    ==> options=[Version, Output "stdout", Name "bar", Name "baz"]  args=[]
-- putStr (test Permute ["--ver","foo"])
--    ==> option `--ver' is ambiguous; could be one of:
--          -v      --verbose             verbosely list files
--          -V, -?  --version, --release  show version info
--        Usage: foobar [OPTION...] files...
--          -v        --verbose             verbosely list files
--          -V, -?    --version, --release  show version info
--          -o[FILE]  --output[=FILE]       use FILE for dump
--          -n USER   --name=USER           only dump USER's files
-----------------------------------------------------------------------------------------
-}

{- $example1

A simple choice for the type associated with flags is to define a type
@Flag@ as an algebraic type representing the possible flags and their
arguments:

>    module Opts1 where
>
>    import System.Console.GetOpt
>    import Data.Maybe ( fromMaybe )
>
>    data Flag
>     = Verbose  | Version
>     | Input String | Output String | LibDir String
>       deriving Show
>
>    options :: [OptDescr Flag]
>    options =
>     [ Option ['v']     ["verbose"] (NoArg Verbose)       "chatty output on stderr"
>     , Option ['V','?'] ["version"] (NoArg Version)       "show version number"
>     , Option ['o']     ["output"]  (OptArg outp "FILE")  "output FILE"
>     , Option ['c']     []          (OptArg inp  "FILE")  "input FILE"
>     , Option ['L']     ["libdir"]  (ReqArg LibDir "DIR") "library directory"
>     ]
>
>    inp,outp :: Maybe String -> Flag
>    outp = Output . fromMaybe "stdout"
>    inp  = Input  . fromMaybe "stdin"
>
>    compilerOpts :: [String] -> IO ([Flag], [String])
>    compilerOpts argv =
>       case getOpt Permute options argv of
>          (o,n,[]  ) -> return (o,n)
>          (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
>      where header = "Usage: ic [OPTION...] files..."

Then the rest of the program will use the constructed list of flags
to determine it\'s behaviour.

-}

{- $example2

A different approach is to group the option values in a record of type
@Options@, and have each flag yield a function of type
@Options -> Options@ transforming this record.

>    module Opts2 where
>
>    import System.Console.GetOpt
>    import Data.Maybe ( fromMaybe )
>
>    data Options = Options
>     { optVerbose     :: Bool
>     , optShowVersion :: Bool
>     , optOutput      :: Maybe FilePath
>     , optInput       :: Maybe FilePath
>     , optLibDirs     :: [FilePath]
>     } deriving Show
>
>    defaultOptions    = Options
>     { optVerbose     = False
>     , optShowVersion = False
>     , optOutput      = Nothing
>     , optInput       = Nothing
>     , optLibDirs     = []
>     }
>
>    options :: [OptDescr (Options -> Options)]
>    options =
>     [ Option ['v']     ["verbose"]
>         (NoArg (\ opts -> opts { optVerbose = True }))
>         "chatty output on stderr"
>     , Option ['V','?'] ["version"]
>         (NoArg (\ opts -> opts { optShowVersion = True }))
>         "show version number"
>     , Option ['o']     ["output"]
>         (OptArg ((\ f opts -> opts { optOutput = Just f }) . fromMaybe "output")
>                 "FILE")
>         "output FILE"
>     , Option ['c']     []
>         (OptArg ((\ f opts -> opts { optInput = Just f }) . fromMaybe "input")
>                 "FILE")
>         "input FILE"
>     , Option ['L']     ["libdir"]
>         (ReqArg (\ d opts -> opts { optLibDirs = optLibDirs opts ++ [d] }) "DIR")
>         "library directory"
>     ]
>
>    compilerOpts :: [String] -> IO (Options, [String])
>    compilerOpts argv =
>       case getOpt Permute options argv of
>          (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
>          (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
>      where header = "Usage: ic [OPTION...] files..."

Similarly, each flag could yield a monadic function transforming a record,
of type @Options -> IO Options@ (or any other monad), allowing option
processing to perform actions of the chosen monad, e.g. printing help or
version messages, checking that file arguments exist, etc.

-}
