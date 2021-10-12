;;; curry-doc.el --- show function types in echo area

;; Time-stamp: <Thu Dec 10 1998 17:26:21 Stardate: [-30]2203.42 hwloidl>

;; Copyright (C) 1997 Hans-Wolfgang Loidl

;; Author: Hans-Wolfgang Loidl <hwloidl@dcs.glasgow.ac.uk>
;; Maintainer: Hans-Wolfgang Loidl <hwloidl@dcs.glasgow.ac.uk>
;; Temporary Maintainer and Hacker: Graeme E Moss <gem@cs.york.ac.uk>
;; Keywords: extensions, minor mode, language mode, Curry
;; Created: 1997-06-17
;; Revision: $Revision: 1.2 $
;; FTP archive: /ftp@ftp.dcs.gla.ac.uk:/pub/glasgow-fp/authors/Hans_Loidl/Elisp/curry-doc.el
;; Status: Beta version

;; $Id: curry-doc.el,v 1.2 2004/10/25 11:29:00 pakcs Exp $

;;; Copyright:
;;  ==========

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:
;;  ===========

;; This program shows the type of the Curry function under the cursor in the
;; minibuffer. It acts as a kind of "emacs background process", by regularly
;; checking the word under the cursor and matching it against a list of
;; prelude, library, local and global functions.

;; The preferred usage of this package is in combination with `pakcs-mode'.
;; In that case `curry-doc' checks an internal variable updated by `imenu'
;; to access the types of all local functions. In `curry-mode' this is not 
;; possible. However, types of prelude functions are still shown.

;; To show types of global functions, i.e. functions defined in a module 
;; imported by the current module, call the function 
;; `turn-on-curry-doc-global-types'. This automatically loads all modules
;; and builds `imenu' tables to get the types of all functions (again this 
;; currently requires `pakcs-mode'). 
;; Note: The modules are loaded recursively, so you might pull in
;;       many modules by just turning on global function support.
;; This features is currently not very well supported.

;; This program was inspired by the `eldoc.el' package by Noah Friedman.

;;; Installation:
;;  =============

;; One useful way to enable this minor mode is to put the following in your
;; .emacs:
;;
;;      (autoload 'turn-on-curry-doc-mode "curry-doc" nil t)

;;   and depending on the major mode you use for your Curry programs:
;;      (add-hook 'pakcs-mode-hook 'turn-on-curry-doc-mode)    ; pakcs-mode
;;     or
;;      (add-hook 'curry-mode-hook 'turn-on-curry-doc-mode) ; curry-mode

;;; Customisation:
;;  ==============

;; You can control what exactly is shown by setting the following variables to
;; either t or nil:
;;  `curry-doc-show-global-types' (default: nil)
;;  `curry-doc-show-reserved'     (default: t)
;;  `curry-doc-show-prelude'      (default: '1.4)
;;  `curry-doc-show-strategy'     (default: t)
;;  `curry-doc-show-user-defined' (default: t)

;; If you want to define your own strings for some identifiers define an
;; alist of (ID . STRING) and set `curry-doc-show-user-defined' to t. 
;; E.g:
;;
;;   (setq curry-doc-show-user-defined t)
;;   (setq curry-doc-user-defined-ids 
;;	(list 
;;	   '("main" . "just another pathetic main function")
;;	   '("foo" . "a very dummy name")
;;	   '("bar" . "another dummy name")))

;;  The following two variables are useful to make the type fit on one line:
;;  If `curry-doc-chop-off-context' is non-nil the context part of the type
;;  of a local fct will be eliminated (default: t). 
;;  If `curry-doc-chop-off-fctname' is non-nil the function name is not 
;;  shown together with the type (default: nil).

;;; Internals:
;;  ==========

;; `curry-doc-mode' is implemented as a minor-mode. So, you can combine it
;; with any other mode. To enable it just type
;;   M-x turn-on-curry-doc-mode

;; These are the names of the functions that can be called directly by the
;; user (with keybindings in `pakcs-mode' and `curry-mode'):
;;  `curry-doc-mode' ... toggle curry-doc-mode; with prefix turn it on
;;                        unconditionally if the prefix is greater 0 otherwise
;;                        turn it off
;;                        Key: CTRL-c CTRL-o (CTRL-u CTRL-c CTRL-o)
;;  `curry-doc-ask-mouse-for-type' ... show the type of the id under the mouse
;;                                      Key: C-S-M-mouse-3
;;  `curry-doc-show-reserved'     ... toggle echoing of reserved id's types
;;  `curry-doc-show-prelude'      ... toggle echoing of prelude id's types
;;  `curry-doc-show-strategy'     ... toggle echoing of strategy id's types
;;  `curry-doc-show-user-defined' ... toggle echoing of user def id's types
;;  `curry-doc-check-active' ... check whether curry-doc is active via the 
;;                                `post-command-idle-hook' (for testing); 
;;                                 Key: CTRL-c ESC-/

;;; ToDo:
;;  =====

;;   - Fix byte-compile problems in `curry-doc-prelude-types' for getArgs etc 
;;   - Write a parser for .hi files and make curry-doc independent from
;;     pakcs-mode. Read library interfaces via this parser.
;;   - Support both Curry 1.4 and 1.2
;;   - Indicate kind of object with colours
;;   - Handle multi-line types
;;   - Encode i-am-fct info in the alist of ids and types.
;;   - Replace the usage of `post-command-idle-hook' with idle timers

;;; Bugs:
;;  =====

;;   - Some prelude fcts aren't displayed properly. This might be due to a 
;;     name clash of Curry and Elisp functions (e.g. length) which
;;     confuses emacs when reading `curry-doc-prelude-types'

;;; Changelog:
;;  ==========
;;  $Log: curry-doc.el,v $
;;  Revision 1.2  2004/10/25 11:29:00  pakcs
;;  Modified emacs mode with support for PAKCS
;;
;;  Revision 1.1  2004/10/22 16:15:23  pakcs
;;  New emacs mode added.
;;
;;  Revision 1.6  1998/12/10 16:27:25  hwloidl
;;  Minor changes ("Doc" as modeline string, mouse-3 moved to C-S-M-mouse-3)
;;
;;  Revision 1.5  1998/09/24 14:25:46  gem
;;  Fixed minor compatibility bugs with Curry mode of Moss&Thorn.
;;  Disabled M-/ binding.
;;
;;  Revision 1.4  1997/11/12 23:51:19  hwloidl
;;  Fixed start-up problem under emacs-19.34.
;;  Added support for wrapped (multi-line) types and 2 vars to control the
;;  behaviour with long fct types
;;
;;  Revision 1.3  1997/11/03 00:48:03  hwloidl
;;  Major revision for first release.
;;  Added alists for showing prelude fcts, curry syntax, and strategies
;;  Added mouse interface to show type under mouse
;;  Fixed bug which causes demon to fall over
;;  Works now with pakcs-mode and curry-mode under emacs 19.34,20 and xemacs 19.15
;;

;;; Code:
;;  =====

;@menu
;* Constants and Variables::	
;* Install as minor mode::	
;* Menubar Support::		
;* Curry Doc Mode::		
;* Switch it on or off::	
;* Check::			
;* Top level function::		
;* Mouse interface::		
;* Print fctsym::		
;* Movement::			
;* Bug Reports::		
;* Visit home site::		
;* Index::			
;* Token::			
;@end menu

;@node top, Constants and Variables, (dir), (dir)
;@top

;@node Constants and Variables, Install as minor mode, top, top
;@section Constants and Variables

;@menu
;* Emacs portability::		
;* Maintenance stuff::		
;* Mode Variable::		
;* Variables::			
;* Prelude types::		
;* Test membership::		
;@end menu

;@node Emacs portability, Maintenance stuff, Constants and Variables, Constants and Variables
;@subsection Emacs portability

(defconst curry-doc-xemacs-p (string-match "XEmacs\\|Lucid" emacs-version)
  "Running under XEmacs?")

(defconst curry-doc-emacs-p (and (or (string-match "^19" emacs-version)
				       (string-match "^20" emacs-version))
				(not curry-doc-xemacs-p))
  "Running under Emacs?")

;@node Maintenance stuff, Mode Variable, Emacs portability, Constants and Variables
;@subsection Maintenance stuff

(defconst curry-doc-version "$Revision: 1.2 $"
 "Version of `curry-doc' as RCS Revision.")

(defconst curry-doc-maintainer "Hans-Wolfgang Loidl <hwloidl@dcs.glasgow.ac.uk>"
  "Maintainer of `curry-doc'.")

(defconst curry-doc-ftp-site "/ftp@ftp.dcs.gla.ac.uk:/pub/glasgow-fp/authors/Hans_Loidl/Elisp/"
  "Main FTP site with latest version of `curry-doc' and sample files.")

;@node Mode Variable, Variables, Maintenance stuff, Constants and Variables
;@subsection Mode Variable

;;;###autoload
(defvar curry-doc-mode nil
  "*If non-nil, show the type of the function near point or a related comment.

If the identifier near point is a Curry keyword and the variable
`curry-doc-show-reserved' is non-nil show a one line summary
of the syntax.

If the identifier near point is a Prelude or one of the standard library 
functions and `curry-doc-show-prelude' is non-nil show its type. Currently 
only Curry 1.4 functions are supported. In future versions the 
`curry-doc-show-prelude' variable should determine which prelude/library
to use for type lookup.

If the identifier near point is local \(i.e. defined in this module\) check
the `imenu' list of functions for the type. This obviously requires that
your language mode uses `imenu' \(`pakcs-mode' 0.6 for example\).

If the identifier near point is global \(i.e. defined in an imported module\) 
and the variable `curry-doc-show-global-types' is non-nil show the type of its 
function.

If the identifier near point is a standard strategy or a function, type related
related to strategies and `curry-doc-show-strategy' is non-nil show the type
of the function. Strategies are special to the parallel execution of Curry.
If you're not interested in that just turn it off.

If the identifier near point is a user defined function that occurs as key
in the alist `curry-doc-user-defined-ids' and the variable 
`curry-doc-show-user-defined' is non-nil show the type of the function.

This variable is buffer-local.")
(make-variable-buffer-local 'curry-doc-mode)

(defvar curry-doc-mode-hook nil
 "Hook invoked when entering curry-doc-mode.")

(defvar curry-doc-index nil
 "Variable holding an alist matching file names to fct-type alists.
The function `curry-doc-make-global-fct-index' rebuilds this variables \(similar to an
`imenu' rescan\).
This variable is buffer-local.")
(make-variable-buffer-local 'curry-doc-index)

(defvar curry-doc-show-global-types nil
 "*If non-nil, search for the types of global functions by loading the files.
This variable is buffer-local.")
(make-variable-buffer-local 'curry-doc-show-global-types)

(defvar curry-doc-show-reserved t
 "*If non-nil, show a documentation string for reserved ids.
This variable is buffer-local.")
(make-variable-buffer-local 'curry-doc-show-reserved)

(defvar curry-doc-show-prelude t ; '1.4
 "*If non-nil, show a documentation string for prelude functions.
Possible values are Curry versions. Currently, only `1.4' is supported.
This variable is buffer-local.")
(make-variable-buffer-local 'curry-doc-show-prelude)

(defvar curry-doc-show-strategy t
 "*If non-nil, show a documentation string for strategies.
This variable is buffer-local.")
(make-variable-buffer-local 'curry-doc-show-strategy)

(defvar curry-doc-show-user-defined t
 "*If non-nil, show a documentation string for user defined ids.
This variable is buffer-local.")
(make-variable-buffer-local 'curry-doc-show-user-defined)

(defvar curry-doc-chop-off-context t
 "*If non-nil eliminate the context part in a Curry type.")

(defvar curry-doc-chop-off-fctname nil
 "*If non-nil omit the function name and show only the type.")

(defvar curry-doc-search-distance 40  ; distance in characters
 "*How far to search when looking for the type declaration of fct under cursor.")

;@node Variables, Prelude types, Mode Variable, Constants and Variables
;@subsection Variables

(defvar curry-doc-idle-delay 0.50
  "*Number of seconds of idle time to wait before printing.
If user input arrives before this interval of time has elapsed after the
last input, no documentation will be printed.

If this variable is set to 0, no idle time is required.")

(defvar curry-doc-argument-case 'identity ; 'upcase
  "Case to display argument names of functions, as a symbol.
This has two preferred values: `upcase' or `downcase'.
Actually, any name of a function which takes a string as an argument and
returns another string is acceptable.")

(defvar curry-doc-mode-message-commands nil
  "*Obarray of command names where it is appropriate to print in the echo area.

This is not done for all commands since some print their own
messages in the echo area, and these functions would instantly overwrite
them.  But self-insert-command as well as most motion commands are good
candidates.

It is probably best to manipulate this data structure with the commands
`curry-doc-add-command' and `curry-doc-remove-command'.")

;(cond ((null curry-doc-mode-message-commands)
;       ;; If you increase the number of buckets, keep it a prime number.
;       (setq curry-doc-mode-message-commands (make-vector 31 0))
;       (let ((list '("self-insert-command"
;                     "next-"         "previous-"
;                     "forward-"      "backward-"
;                     "beginning-of-" "end-of-"
;                     "goto-"
;                     "recenter"
;                     "scroll-"))
;             (syms nil))
;         (while list
;           (setq syms (all-completions (car list) obarray 'fboundp))
;           (setq list (cdr list))
;           (while syms
;             (set (intern (car syms) curry-doc-mode-message-commands) t)
;             (setq syms (cdr syms)))))))

;; Bookkeeping; the car contains the last symbol read from the buffer.
;; The cdr contains the string last displayed in the echo area, so it can
;; be printed again if necessary without reconsing.
(defvar curry-doc-last-data '(nil . nil))

(defvar curry-doc-minor-mode-string " Doc"              ; " Curry-Doc"
  "*String to display in mode line when Curry-Doc Mode is enabled.")

(defconst curry-doc-varlist
  (list
   'curry-doc-xemacs-p
   'curry-doc-emacs-p
   'curry-doc-version
   'curry-doc-mode
   'curry-doc-mode-hook
   'curry-doc-index
   'curry-doc-show-global-types
   'curry-doc-show-reserved
   'curry-doc-show-prelude
   'curry-doc-show-strategy
   'curry-doc-show-user-defined
   'curry-doc-idle-delay
   'curry-doc-argument-case
   'curry-doc-mode-message-commands
  )
  "List of variables sent via `curry-doc-submit-bug-report'.")

;@node Prelude types, Test membership, Variables, Constants and Variables
;@subsection Prelude types

;@cindex curry-doc-reserved-ids

(defvar curry-doc-reserved-ids
 (list
  '("case" . "case exp of { alts [;] }")
  '("fcase" . "fcase exp of { alts [;] }")
  '("class" . "class [context =>] simpleclass [where { cbody [;] }]")
  '("data" . "data [context =>] simpletype = constrs [deriving]")
  '("default" . "default (type1 , ... , typen)")
  '("deriving" . "deriving (dclass | (dclass1, ... , dclassn))") ; used with data or newtype
  '("do" . "do { stmts [;] }  stmts -> exp [; stmts] | pat <- exp ; stmts | let decllist ; stmts")
  '("else" . "if exp then exp else exp")
  '("if" . "if exp then exp else exp")
  '("import" . "import [qualified] modid [as modid] [impspec]")
  '("in" . "let decllist in exp")
  '("infix" . "infix [digit] ops")
  '("infixl" . "infixl [digit] ops")
  '("infixr" . "infixr [digit] ops")
  '("instance" . "instance [context =>] qtycls inst [where { valdefs [;] }]")
  '("let" . "let { decl; ...; decl [;] } in exp")
  '("module" . "module modid [exports] where body")
  '("newtype" . "newtype [context =>] simpletype = con atype [deriving]")
  '("of" . "case exp of { alts [;] }")
  '("then" . "if exp then exp else exp")
  '("type" . "type simpletype = type")
  '("where" . "exp where { decl; ...; decl [;] }") ; check that ; see also class, instance, module
  '("as" . "import [qualified] modid [as modid] [impspec]")
  '("qualified" . "import [qualified] modid [as modid] [impspec]")
  '("hiding" . "hiding ( import1 , ... , importn [ , ] )")
 )
 "An alist of reserved identifiers and a string describing the construct they are used in.")

;@cindex curry-doc-prelude-types

(defvar curry-doc-prelude-types
 (list
 ; Taken from the prelude of the 1.4 report
 ; ToDo: clean this up
 ; ToDo: add overloaded fcts at the beginning of the report.
; '("subtract"          . "(Num a) => a -> a -> a")
; '("odd"               . "(Integral a) => a -> Bool")
; '("even"              . "(Integral a) => a -> Bool")
; '("gcd"               . "(Integral a) => a -> a -> a")
; '("lcm"               . "(Integral a) => a -> a -> a")
; ;'("^"                . "(Num a, Integral b) => a -> b -> a")
; ;'("^^"               . "(Fractional a, Integral b) => a -> b -> a")
; '("fromIntegral"      . "(Integral a, Num b) => a -> b")
; '("fromRealFrac"      . "(RealFrac a, Fractional b) => a -> b")
; '("atan2"             . "(RealFloat a) => a -> a -> a")
; '("map"               . "(Functor f) => (a -> b) -> f a -> f b")
; ;'("(>>=)             . "(Monad m) => m a -> (a -> m b) -> m b")
; ;'("(>>)              . "(Monad m) => m a -> m b -> m b")
; '("return"            . "(Monad m) => a -> m a")
; '("zero"              . "(Monad m) => m a")
; ;'("(++)              . "(Monad m) => m a -> m a -> m a")
; '("accumulate"        . "Monad m => [m a] -> m [a] ")
; '("sequence"          . "Monad m => [m a] -> m () ")
; '("mapM"              . "Monad m => (a -> m b) -> [a] -> m [b]")
; '("mapM_"             . "Monad m => (a -> m b) -> [a] -> m ()")
; '("guard"             . "MonadZero m => Bool -> m ()")
; '("filter"            . "MonadZero m => (a -> Bool) -> m a -> m a")
; '("concat"            . "MonadPlus m => [m a] -> m a")
; '("applyM"            . "Monad m => (a -> m b) -> m a -> m b")
; '("seq"               . "(Eval a) => a -> b -> b")
; '("strict"            . "(Eval a) => (a -> b) -> a -> b")
; '("id"                . "a -> a")
; '("const"             . "a -> b -> a")
; ; '("."               . "(b -> c) -> (a -> b) -> a -> c")
; '("flip"              . "(a -> b -> c) -> b -> a -> c")
; ;'("$"                . "(a -> b) -> a -> b")
; ;'("&&"               . "Bool -> Bool -> Bool")
; ;'("||"               . "Bool -> Bool -> Bool")
; '("not"               . "Bool -> Bool")
; '("maybe"             . "b -> (a -> b) -> Maybe a -> b")
; '("either"            . "(a -> c) -> (b -> c) -> Either a b -> c")
; '("numericEnumFrom"          . "(Real a) => a -> [a]")
; '("numericEnumFromThen"      . "(Real a) => a -> a -> [a]")
; '("numericEnumFromTo"        . "(Real a) => a -> a -> [a]")
; '("numericEnumFromThenTo"    . "(Real a) => a -> a -> a -> [a]")
; '("fst"               . "(a,b) -> a")
; '("snd"               . "(a,b) -> b")
; '("curry"             . "((a, b) -> c) -> a -> b -> c")
; '("uncurry"           . "(a -> b -> c) -> ((a, b) -> c)")
; '("until"             . "(a -> Bool) -> (a -> a) -> a -> a")
; '("asTypeOf"          . "a -> a -> a")
; '("error"             . "String -> a")
; '("undefined"         . "a")
; ; List fcts
; '("head"              . "[a] -> a")
; '("last"              . "[a] -> a")
; '("tail"              . "[a] -> [a]")
; '("init"              . "[a] -> [a]")
; '("null"              . "[a] -> Bool")
; '("length"            . "[a] -> Int")
; ; '("!!"              . "[a] -> Int -> a")
; '("foldl"             . "(a -> b -> a) -> a -> [b] -> a")
; '("foldl1"            . "(a -> a -> a) -> [a] -> a")
; '("scanl"             . "(a -> b -> a) -> a -> [b] -> [a]")
; '("scanl1"            . "(a -> a -> a) -> [a] -> [a]")
; '("foldr"             . "(a -> b -> b) -> b -> [a] -> b")
; '("foldr1"            . "(a -> a -> a) -> [a] -> a")
; '("scanr"             . "(a -> b -> b) -> b -> [a] -> [b]")
; '("scanr1"            . "(a -> a -> a) -> [a] -> [a]")
; '("iterate"           . "(a -> a) -> a -> [a]")
; '("repeat"            . "a -> [a]")
; '("replicate"         . "Int -> a -> [a]")
; '("cycle"             . "[a] -> [a]")
; '("take"              . "Int -> [a] -> [a]")
; '("drop"              . "Int -> [a] -> [a]")
; '("splitAt"           . "Int -> [a] -> ([a],[a])")
; '("takeWhile"         . "(a -> Bool) -> [a] -> [a]")
; '("dropWhile"         . "(a -> Bool) -> [a] -> [a]")
; '("span"              . "(a -> Bool) -> [a] -> ([a],[a])")
; '("break"             . "(a -> Bool) -> [a] -> ([a],[a])")
; '("lines"             . "String -> [String]")
; '("words"             . "String -> [String]")
; '("unlines"           . "[String] -> String")
; '("unwords"           . "[String] -> String")
; '("reverse"           . "[a] -> [a]")
; '("and"               . "[Bool] -> Bool")
; '("or"                . "[Bool] -> Bool")
; '("any"               . "(a -> Bool) -> [a] -> Bool")
; '("all"               . "(a -> Bool) -> [a] -> Bool")
; '("elem"              . "(Eq a) => a -> [a] -> Bool")
; '("notElem"           . "(Eq a) => a -> [a] -> Bool")
; '("lookup"            . "(Eq a) => a -> [(a,b)] -> Maybe b")
; '("sum"               . "(Num a) => [a] -> a")
; '("product"           . "(Num a) => [a] -> a")
; '("maximum"           . "(Ord a) => [a] -> a")
; '("minimum"           . "(Ord a) => [a] -> a")
; '("concatMap"         . "(a -> [b]) -> [a] -> [b]")
; '("zip"               . "[a] -> [b] -> [(a,b)]")
; '("zip3"              . "[a] -> [b] -> [c] -> [(a,b,c)]")
; '("zipWith"           . "(a->b->c) -> [a]->[b]->[c]")
; '("zipWith3"          . "(a->b->c->d) -> [a]->[b]->[c]->[d]")
; '("unzip"             . "[(a,b)] -> ([a],[b])")
; '("unzip3"            . "[(a,b,c)] -> ([a],[b],[c])")
; '("readsPrec"         . "(Read a) => Int -> ReadS a")
; '("readList"          . "(Read a) => ReadS [a]")
; '("showsPrec"         . "(Show a) => Int -> a -> ShowS")
; '("showList"          . "(Show a) => [a] -> ShowS")
; '("reads"             . "(Read a) => ReadS a")
; '("shows"             . "(Show a) => a -> ShowS")
; '("read"              . "(Read a) => String -> a")
; '("show"              . "(Show a) => a -> String")
; '("showChar"          . "Char -> ShowS")
; '("showString"        . "String -> ShowS")
; '("showParen"         . "Bool -> ShowS -> ShowS")
; '("readParen"         . "Bool -> ReadS a -> ReadS a")
; '("lex"               . "ReadS String")
; '("lexDigits"         . "ReadS String ")
; '("nonnull"           . "(Char -> Bool) -> ReadS String")
; '("lexLitChar"        . "ReadS String")
; ;'("showSigned"       . "(Real a) => (a -> ShowS) -> Int -> a -> ShowS")
; ;'("readSigned"       . "(Real a) => ReadS a -> ReadS a")
; ;'("showInt"          . "(Integral a) => a -> ShowS")
; ;'("readInt"          . "(Integral a) => a -> (Char -> Bool) -> (Char -> Int) -> ReadS a")
; '("readDec"           . "(Integral a) => ReadS a")
; '("readOct"           . "(Integral a) => ReadS a")
; '("readHex"           . "(Integral a) => ReadS a")
; ; IO fcts
; '("fail"              . "IOError -> IO a")
; '("userError"         . "String -> IOError")
; '("catch"             . "IO a -> (IOError -> IO a) -> IO a ")
; '("putChar"           . "Char -> IO ()")
; '("putStr"            . "String -> IO ()")
; '("putStrLn"          . "String -> IO ()")
; '("print"             . "Show a => a -> rIO ()")
; '("getChar"           . "IO Char")
; '("getLine"           . "IO String")
; '("getContents"       . "IO String")
; '("interact"          . "(String -> String) -> IO ()")
; '("readFile"          . "FilePath -> IO String")
; '("writeFile"         . "FilePath -> String -> IO ()")
; '("appendFile"        . "FilePath -> String -> IO ()")
; '("readIO"            . "Read a => String -> IO a")
; '("readLn"            . "Read a => IO a")
 ;; ---------------------------------------------------------------------------
 ;; taken from Prelude Index of the Curry 1.4 report
 '("!!"             . "[a] -> Int -> a ")
 '("$"              . "(a -> b) -> a -> b ")
 '("&&"             . "Bool -> Bool -> Bool ")
 '("||"             . "Bool -> Bool -> Bool ")
 '("*"              . "Num a => a -> a -> a ")
 '("**"             . "Floating a => a -> a -> a ")
 '("+"              . "Num a => a -> a -> a")
 '("++"             . "MonadPlus m => m a -> m a -> m a ")
 '("-"              . "Num a => a -> a -> a ")
 '("."              . "(b -> c) -> (a -> b) -> a -> c ")
 '("/"              . "Fractional a => a -> a -> a ")
 '("/="             . "Eq a => a -> a -> Bool ")
 '("<"              . "Ord a => a -> a -> Bool ")
 '("<="             . "Ord a => a -> a -> Bool ")
 '("=="             . "Eq a => a -> a -> Bool ")
 '(">"              . "Ord a => a -> a -> Bool ")
 '(">="             . "Ord a => a -> a -> Bool ")
 '(">>"             . "m a -> m b -> m b ")
 '(">>="            . "Monad m => m a -> (a -> m b) -> m b ")
 '("^"              . "(Num a, Integral b) => a -> b -> a ")
 '("^^"             . "(Fractional a, Integral b) => a -> b -> a ")
 '("abs"              . "Num a => a -> a ")
 '("accumulate"       . "Monad m => [m a] -> m [a] ")
 '("acos"             . "Floating a => a -> a ")
 '("acosh"            . "Floating a => a -> a ")
 '("all"              . "(a -> Bool) -> [a] -> Bool ")
 '("and"              . "[Bool] -> Bool ")
 '("any"              . "(a -> Bool) -> [a] -> Bool ")
 '("appendFile"       . "FilePath -> String -> IO ()")
 '("applyM"           . "Monad m => (a -> m b) -> m a -> m b")
 '("asTypeOf"         . "a -> a -> a ")
 '("asin"             . "Floating a => a -> a ")
 '("asinh"            . "Floating a => a -> a ")
 '("atan"             . "Floating a => a -> a ")
 '("atan2"            . "RealFrac a => a -> a ")
 '("atanh"            . "Floating a => a -> a ")
 '("break"            . "(a -> Bool) -> [a] -> ([a], [a]) ")
 '("catch"            . "IO a -> (IOError -> IO a) -> IO a ")
 '("ceiling"          . "(RealFrac a, Integral b) => a -> b ")
 '("compare"          . "Ord a => a -> a -> Ordering ")
 '("concat"           . "MonadPlus m => [m a] -> m a ")
 '("concatMap"        . "(a -> [b]) -> [a] -> [b]")
 '("const"            . "a -> b -> a")
 '("cos"              . "Floating a => a -> a ")
 '("cosh"             . "Floating a => a -> a ")
 '("curry"            . "((a, b) -> c) -> a -> b -> c")
 '("cycle"            . "[a] -> [a] ")
 '("decodeFloat"      . "RealFloat a => a -> (Integer, Int) ")
 '("div"              . "Integral a => a -> a -> a ")
 '("divMod"           . "Integral a => a -> a -> (a, a) ")
 '("drop"             . "Int -> [a] -> [a] ")
 '("dropWhile"        . "(a -> Bool) -> [a] -> [a] ")
 '("elem"             . "Eq a => a -> [a] -> Bool ")
 '("encodeFloat"      . "RealFloat a => Integer -> Int -> a ")
 '("enumFrom"         . "Enum a => a -> [a] ")
 '("enumFromThen"     . "Enum a => a -> a -> [a] ")
 '("enumFromThenTo"   . "Enum a => a -> a -> a -> [a] ")
 '("enumFromTo"       . "Enum a => a -> a -> [a] ")
 '("error"            . "String -> a ")
 '("even"             . "Integral a => a -> Bool")
 '("exp"              . "Floating a => a -> a ")
 '("exponent"         . "RealFloat a => a -> Int ")
 '("fail"             . "IOError -> IO a ")
 '("filter"           . "MonadZero m => (a -> Bool) -> m a -> m a ")
 '("flip"             . "(a -> b -> c) -> (b -> a -> c)")
 '("floatDigits"      . "RealFloat a => a -> Int ")
 '("floatRadix"       . "RealFloat a => a -> Integer ")
 '("floatRange"       . "RealFloat a => a -> (Int, Int) ")
 '("floor"            . "(RealFrac a, Integral b) => a -> b ")
 '("foldl"            . "(a -> b -> a) -> a -> [b] -> a ")
 '("foldl1"           . "(a -> a -> a) -> [a] -> a ")
 '("foldr"            . "(a -> b -> b) -> b -> [a] -> b ")
 '("foldr1"           . "(a -> a -> a) -> [a] -> a ")
 '("fromEnum"         . "Enum a => a -> Int ")
 '("fromInteger"      . "Num a => Integer -> a ")
 '("fromIntegral"     . "(Integral a, Num b) => a -> b")
 '("fromRational"     . "Fractional a => Rational -> a ")
 '("fromRealFrac"     . "(RealFrac a, Fractional b) => a -> b")
 '("fst"              . "(a, b) -> a")
 '("gcd"              . "(Integral a) => a -> a -> a")
 '("getChar"          . "IO Char ")
 '("getContents"      . "IO String")
 '("getLine"          . "IO Char ")
 '("guard"            . "MonadZero m => Bool -> m ()")
 '("head"             . "[a] -> a")
 '("id"               . "a -> a")
 '("init"             . "[a] -> [a]")
 '("interact"         . "(String -> String) -> IO ()")
 '("isDenormalized"   . "RealFloat a => a -> Bool ")
 '("isIEEE"           . "RealFloat a => a -> Bool ")
 '("isInfinite"       . "RealFloat a => a -> Bool ")
 '("isNaN"            . "RealFloat a => a -> Bool ")
 '("isNegativeZero"   . "RealFloat a => a -> Bool ")
 '("iterate"          . "(a -> a) -> a -> [a] ")
 '("last"             . "[a] -> a ")
 '("lcm"              . "Integral a => a -> a -> a")
 '("length"           . "[a] -> Int")
 '("lex"              . "ReadS String ")
 '("lines"            . "String -> [String]")
 '("log"              . "Floating a => a -> a ")
 '("logBase"          . "Floating a => a -> a -> a ")
 '("lookup"           . "Eq a => a -> [(a, b)] -> Maybe b")
 '("map"              . "Functor f => (a -> b) -> f a -> f b ")
 '("mapM"             . "Monad m => (a -> m b) -> [a] -> m [b]")
 '("mapM_"            . "Monad m => (a -> m b) -> [a] -> m ()")
 '("max"              . "Ord a => a -> a -> a ")
 '("maxBound"         . "Bounded a => a ")
 '("maximum"          . "Ord a => [a] -> a")
 '("maybe"            . "b -> (a -> b) -> Maybe a -> b ")
 '("min"              . "Ord a => a -> a -> a ")
 '("minBound"         . "Bounded a => a ")
 '("minimum"          . "Ord a => [a] -> a")
 '("mod"              . "Integral a => a -> a -> a ")
 '("negate"           . "Num a => a -> a ")
 '("not"              . "Bool -> Bool")
 '("notElem"          . "Eq a => a -> [a] -> Bool")
 '("null"             . "[a] -> Bool")
 '("odd"              . "Integral a => a -> Bool")
 '("or"               . "[Bool] -> Bool")
 '("otherwise"        . "Bool")
 '("pi"               . "Floating a => a ")
 '("pred"             . "Enum a => a -> a ")
 '("print"            . "Show a => IO () ")
 '("product"          . "Num a => [a] -> a")
 '("properFraction"   . "(RealFrac a, Integral b) => a -> (b, a) ")
 '("putChar"          . "Char -> IO ()")
 '("putStr"           . "String -> IO ()")
 '("putStrLn"         . "String -> IO () ")
 '("quot"             . "Integral a => a -> a -> a ")
 '("quotRem"          . "Integral a => a -> a -> (a, a) ")
 '("read"             . "Read a => String -> a")
 '("readFile"         . "FilePath -> IO String")
 '("readIO"           . "Read a => String -> IO a ")
 '("readList"         . "Read a => ReadS [a]")
 '("readLn"           . "Read a => IO a")
 '("readParen"        . "Bool -> ReadS a -> ReadS a")
 '("reads"            . "Read a => ReadS a ")
 '("readsPrec"        . "Read a => Int -> ReadS a")
 '("recip"            . "Fractional a => a -> a ")
 '("rem"              . "Integral a => a -> a -> a ")
 '("repeat"           . "a -> [a] ")
 '("replicate"        . "Int -> a -> [a] ")
 '("return"           . "Monad m => a -> m a ")
 '("reverse"          . "[a] -> [a] ")
 '("round"            . "(RealFrac a, Integral b) => a -> b ")
 '("scaleFloat"       . "RealFloat a => Int -> a -> a ")
 '("scanl"            . "(a -> b -> a) -> a -> [b] -> [a] ")
 '("scanl1"           . "(a -> a -> a) -> [a] -> [a] ")
 '("scanr"            . "(a -> b -> b) -> b -> [a] -> [b] ")
 '("scanr1"           . "(a -> a -> a) -> [a] -> [a] ")
 '("seq"              . "Eval a => a -> a -> b ")
 '("sequence"         . "Monad m => [m a] -> m () ")
 '("show"             . "Show a => a -> String ")
 '("showChar"         . "Char -> ShowS")
 '("showList"         . "Show a => [a] -> ShowS")
 '("showParen"        . "Bool -> ShowS -> ShowS")
 '("showString"       . "String -> ShowS")
 '("shows"            . "Show a => a -> ShowS ")
 '("showsPrec"        . "Show a => Int -> a -> ShowS ")
 '("significand"      . "RealFloat a => a -> a ")
 '("signum"           . "Num a => a -> a ")
 '("sin"              . "Floating a => a -> a ")
 '("sinh"             . "Floating a => a -> a ")
 '("snd"              . "(a, b) -> b")
 '("span"             . "(a -> Bool) -> [a] -> ([a], [a]) ")
 '("splitAt"          . "Int -> [a] -> ([a], [a]) ")
 '("sqrt"             . "Floating a => a -> a ")
 '("strict"           . "Eval a => (a -> b) -> (a -> b) ")
 '("subtract"         . "Num a => a -> a -> a")
 '("succ"             . "Enum a => a -> a ")
 '("sum"              . "Num a => [a] -> a ")
 '("tail"             . "[a] -> [a] ")
 '("take"             . "Int -> [a] -> [a] ")
 '("takeWhile"        . "(a -> Bool) -> [a] -> [a] ")
 '("tan"              . "Floating a => a -> a ")
 '("tanh"             . "Floating a => a -> a ")
 '("toEnum"           . "Enum a => Int -> a ")
 '("toInteger"        . "Integral a => a -> Integer ")
 '("toRational"       . "Real a => a -> Rational ")
 '("truncate"         . "(RealFrac a, Integral b) => a -> b ")
 '("uncurry"          . "(a -> b -> c) -> ((a, b) -> c)")
 '("undefined"        . "a ")
 '("unlines"          . "[String] -> String")
 '("until"            . "(a -> Bool) -> (a -> a) -> a -> a ")
 '("unwords"          . "[String] -> String")
 '("unzip"            . "[(a, b)] -> ([a], [b]) ")
 '("unzip3"           . "[(a, b, c)] -> ([a], [b], [c])")
 '("userError"        . "String  -> IOError")
 '("words"            . "String -> [String] ")
 '("writeFile"        . "FilePath -> String -> IO ()")
 '("zero"             . "MonadZero m => m a ")
 '("zip"              . "[a] -> [b] -> [(a, b)] ")
 '("zip3"             . "[a] -> [b] -> [c] -> [(a, b, c)]")
 '("zipWith"          . "(a -> b -> c) -> [a] -> [b] -> [c] ")
 '("zipWith3"         . "(a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]")
 ;; ---------------------------------------------------------------------------
 ;; The following functions are from the 1.4 Library Report (headers/ dir)
 ;; headers/Ratio.hs
 '("numerator"               . "(Integral a) => Ratio a -> a")
 '("denominator"             . "(Integral a) => Ratio a -> a")
 '("approxRational"          . "(RealFrac a) => a -> a -> Rational")
 ;; headers/Complex.hs
 '("realPart" . "(RealFloat a) => Complex a -> a")
 '("imagPart" . "(RealFloat a) => Complex a -> a")
 '("conjugate"	 . "(RealFloat a) => Complex a -> Complex a")
 '("mkPolar"		 . "(RealFloat a) => a -> a -> Complex a")
 '("cis"		 . "(RealFloat a) => a -> Complex a")
 '("polar"		 . "(RealFloat a) => Complex a -> (a,a)")
 '("magnitude" . "(RealFloat a) => Complex a -> a")
 '("phase" . "(RealFloat a) => Complex a -> a")
 ;; headers/Numeric.hs
 '("fromRat" . "(RealFloat a) => Rational -> a")
 '("fromRat'" . "(RealFloat a) => Rational -> a")
 '("scaleRat" . "Rational -> Int -> Rational -> Rational -> Int -> Rational -> (Rational, Int)")
 '("minExpt" . "Int")
 '("maxExpt" . "Int")
 '("expt" . "Integer -> Int -> Integer")
 '("expts" . "Array Int Integer")
 '("integerLogBase" . "Integer -> Integer -> Int")
 '("showSigned"    . "Real a => (a -> ShowS) -> Int -> a -> ShowS")
 '("showInt"    . "Integral a => a -> ShowS")
 '("readSigned" . "(Real a) => ReadS a -> ReadS a")
 '("readInt" . "(Integral a) => a -> (Char -> Bool) -> (Char -> Int) -> ReadS a")
 '("readDec" . "(Integral a) => ReadS a")
 '("readOct" . "(Integral a) => ReadS a")
 '("readHex" . "(Integral a) => ReadS a")
 '("showEFloat"     . "(RealFloat a) => Maybe Int -> a -> ShowS")
 '("showFFloat"     . "(RealFloat a) => Maybe Int -> a -> ShowS")
 '("showGFloat"     . "(RealFloat a) => Maybe Int -> a -> ShowS")
 '("showFloat"      . "(RealFloat a) => a -> ShowS")
 '("formatRealFloat" . "(RealFloat a) => FFFormat -> Maybe Int -> a -> String")
 '("roundTo" . "Int -> Int -> [Int] -> (Int, [Int])")
 '("floatToDigits" . "(RealFloat a) => Integer -> a -> ([Int], Int)")
 '("readFloat"     . "(RealFloat a) => ReadS a")
 '("lexDigits"        . "ReadS String ")
 '("nonnull"          . "(Char -> Bool) -> ReadS String")
 ;; headers/Ix.hs
 '("rangeSize" . "Ix a => (a,a) -> Int")
 ;; headers/Array.hs
 '("array"           . "(Ix a) => (a,a) -> [(a,b)] -> Array a b")
 '("listArray"       . "(Ix a) => (a,a) -> [b] -> Array a b")
 ; '("(!)            . "(Ix a) => Array a b -> a -> b")
 '("bounds"          . "(Ix a) => Array a b -> (a,a)")
 '("indices"         . "(Ix a) => Array a b -> [a]")
 '("elems"           . "(Ix a) => Array a b -> [b]")
 '("assocs"          . "(Ix a) => Array a b -> [(a,b)]")
 '("accumArray"      . "(Ix a) => (b -> c -> b) -> b -> (a,a) -> [(a,c)] -> Array a b")
 ; (//)            . (Ix a) => Array a b -> [(a,b)] -> Array a b
 '("accum"           . "(Ix a) => (b -> c -> b) -> Array a b -> [(a,c)] -> Array a b")
 '("ixmap"           . "(Ix a, Ix b) => (a,a) -> (a -> b) -> Array b c -> Array a c")
 ;; headers/List.hs (omitted; see 1.2 List module above)
 ;; headers/Maybe.hs
 '("isJust"                 . "Maybe a -> Bool")
 '("fromJust"               . "Maybe a -> a")
 '("fromMaybe"              . "a -> Maybe a -> a")
 '("maybeToList"            . "Maybe a -> [a]")
 '("listToMaybe"            . "[a] -> Maybe a")
 '("catMaybes"              . "[Maybe a] -> [a]")
 '("mapMaybe"               . "(a -> Maybe b) -> [a] -> [b]")
 '("unfoldr"                . "([a] -> Maybe ([a], a)) -> [a] -> ([a],[a])")
 ;; headers/Char.hs
 '("isAscii" . "Char -> Bool")
 '("isControl" . "Char -> Bool")
 '("isPrint" . "Char -> Bool")
 '("isSpace" . "Char -> Bool")
 '("isUpper" . "Char -> Bool")
 '("isLower" . "Char -> Bool")
 '("isAlpha" . "Char -> Bool")
 '("isDigit" . "Char -> Bool")
 '("isOctDigit" . "Char -> Bool")
 '("isHexDigit" . "Char -> Bool")
 '("isAlphanum" . "Char -> Bool")
 '("digitToInt" . "Char -> Int")
 '("intToDigit" . "Int -> Char")
 '("toUpper"                  . "Char -> Char")
 '("toLower"                  . "Char -> Char")
 '("ord"                     . "Char -> Int")
 '("chr"                     . "Int  -> Char")
 '("readLitChar"             . "ReadS Char")
 '("sshowLitChar"               . "Char -> ShowS")
 '("lexLitChar"          . "ReadS String")
 ;; headers/Monad.hs
 '("unless"           . "(Monad m) => Bool -> m () -> m ()")
 '("ap"               . "(Monad m) => m (a -> b) -> m a -> m b")
 '("liftM"            . "(Monad m) => (a -> b) -> (m a -> m b)")
 '("liftM2"           . "(Monad m) => (a -> b -> c) -> (m a -> m b -> m c)")
 '("liftM3"           . "(Monad m) => (a -> b -> c -> d) -> (m a -> m b -> m c -> m d)")
 '("liftM4"           . "(Monad m) => (a -> b -> c -> d -> e) -> (m a -> m b -> m c -> m d -> m e)")
 '("liftM5"           . "(Monad m) => (a -> b -> c -> d -> e -> f) -> (m a -> m b -> m c -> m d -> m e -> m f)")
 ;; headers/IO.hs
; '("try"            . "IO a -> IO (Either IOError a)")
; '("bracket"        . "IO a -> (a -> IO b) -> (a -> IO c) -> IO c")
; '("bracket_"        . "IO a -> (a -> IO b) -> IO c -> IO c")
 ;; Directory
; '("createDirectory"  . "FilePath -> IO ()")
; '("removeDirectory"  . "FilePath -> IO ()")
; '("removeFile"  . "FilePath -> IO ()")
; '("renameDirectory"  . "FilePath -> FilePath -> IO ()")
; '("renameFile"  . "FilePath -> FilePath -> IO ()")
; '("getDirectoryContents"  . "FilePath -> IO [FilePath]")
; '("getCurrentDirectory"  . "IO FilePath")
; '("setCurrentDirectory"  . "FilePath -> IO ()")
; '("doesFileExist" . "FilePath -> IO Bool")
; '("doesDirectoryExist" . "FilePath -> IO Bool")
; '("getPermissions" . "FilePath -> IO Permissions")
; '("setPermissions" . "FilePath -> Permissions -> IO ()")
; '("getModificationTime" . "FilePath -> IO ClockTime")
 ;; System
; '("getArgs"  . "IO [String]")
; '("getProgName"  . "IO String")
; '("getEnv"         . "String -> IO String")
; '("system"         . "String -> IO ExitCode")
; '("exitWith"    . "ExitCode -> IO a")
 ;; headers/Time.hs
 '("getClockTime"            . "IO ClockTime")
 '("addToClockTime"          . "TimeDiff     -> ClockTime -> ClockTime")
 '("diffClockTimes"          . "ClockTime    -> ClockTime -> TimeDiff")
 '("toCalendarTime"          . "ClockTime    -> IO CalendarTime")
 '("toUTCTime"               . "ClockTime    -> CalendarTime")
 '("toClockTime"             . "CalendarTime -> ClockTime")
 '("calendarTimeToString"    . "CalendarTime -> String")
 '("formatCalendarTime" . "TimeLocale -> String -> CalendarTime -> String")
 '("show2" . "Int -> String")
 '("show2'" . "Int -> String")
 '("show3" . "Int -> String")
 ;; headers/Locale.hs
 '("defaultTimeLocale" . "TimeLocale ")
 ;; headers/Random.hs
 '("random"    . "(Integer,Integer) -> Integer -> [Integer]")
 '("randomIO"  . "(Integer,Integer) -> IO [Integer]")
 )
"alist of prelude functions and their types.")

;@cindex curry-doc-strategy-ids

(defvar curry-doc-strategy-ids
 (list
  '("par"  . "Done -> Done -> Done ; [infixr 0]")
  '("seq"  . "Done -> Done -> Done ; [infixr 1]")

  '("using"      . "a -> Strategy a -> a ; [infixl 0]")
  '("demanding"  . "a -> Done -> a ; [infixl 0]")
  '("sparking"   . "a -> Done -> a ; [infixl 0]")

  '(">||" . "Done -> Done -> Done ; [infixr 2]")
  '(">|" .  "Done -> Done -> Done ; [infixr 3]")
  '("$||" . "(a -> b) -> Strategy a -> a -> b ; [infixl 6]")
  '("$|"  . "(a -> b) -> Strategy a -> a -> b ; [infixl 6]")
  '(".|"  . "(b -> c) -> Strategy b -> (a -> b) -> (a -> c) ; [infixl 9]")
  '(".||" . "(b -> c) -> Strategy b -> (a -> b) -> (a -> c) ; [infixl 9]")
  '("-|"  . "(a -> b) -> Strategy b -> (b -> c) -> (a -> c) ; [infixl 9]")
  '("-||" . "(a -> b) -> Strategy b -> (b -> c) -> (a -> c) ; [infixl 9]")

  '("Done" . "type Done = ()")
  '("Strategy" . "type Strategy a = a -> Done")

  '("r0"    . "Strategy a")
  '("rwhnf" . "Eval a => Strategy a")
  '("rnf" . "Strategy a")
  '("NFData" . "class Eval a => NFData a where rnf :: Strategy a")
  '("NFDataIntegral" ."class (NFData a, Integral a) => NFDataIntegral a")
  '("NFDataOrd" . "class (NFData a, Ord a) => NFDataOrd a")

  '("markStrat" . "Int -> Strategy a -> Strategy a")

  '("seqPair" . "Strategy a -> Strategy b -> Strategy (a,b)")
  '("parPair" . "Strategy a -> Strategy b -> Strategy (a,b)")
  '("seqTriple" . "Strategy a -> Strategy b -> Strategy c -> Strategy (a,b,c)")
  '("parTriple" . "Strategy a -> Strategy b -> Strategy c -> Strategy (a,b,c)")

  '("parList"  . "Strategy a -> Strategy [a]")
  '("parListN"  . "(Integral b) => b -> Strategy a -> Strategy [a]")
  '("parListNth"  . "Int -> Strategy a -> Strategy [a]")
  '("parListChunk"  . "Int -> Strategy a -> Strategy [a]")
  '("parMap"  . "Strategy b -> (a -> b) -> [a] -> [b]")
  '("parFlatMap"  . "Strategy [b] -> (a -> [b]) -> [a] -> [b]")
  '("parZipWith"  . "Strategy c -> (a -> b -> c) -> [a] -> [b] -> [c]")
  '("seqList"  . "Strategy a -> Strategy [a]")
  '("seqListN"  . "(Integral a) => a -> Strategy b -> Strategy [b]")
  '("seqListNth"  . "Int -> Strategy b -> Strategy [b]")

  '("parBuffer"  . "Int -> Strategy a -> [a] -> [a]")

  '("seqArr"  . "(Ix b) => Strategy a -> Strategy (Array b a)")
  '("parArr"  . "(Ix b) => Strategy a -> Strategy (Array b a)")

  '("fstPairFstList"  . "(NFData a) => Strategy [(a,b)]")
  '("force"  . "(NFData a) => a -> a ")
  '("sforce"  . "(NFData a) => a -> b -> b")
  )
"alist of strategy functions and their types as defined in Strategies.lhs.")

(defvar curry-doc-user-defined-ids nil
 "alist of functions and strings defined by the user.")

;@node Test membership,  , Prelude types, Constants and Variables
;@subsection Test membership

;@cindex curry-doc-is-of
(defsubst curry-doc-is-of (fn types)
  "Check whether FN is one of the functions in the alist TYPES and return the type."
  (assoc fn types) )

;@node Install as minor mode, Menubar Support, Constants and Variables, top
;@section Install as minor mode

;; Put this minor mode on the global minor-mode-alist.
(or (assq 'curry-doc-mode (default-value 'minor-mode-alist))
    (setq-default minor-mode-alist
                  (append (default-value 'minor-mode-alist)
                          '((curry-doc-mode curry-doc-minor-mode-string)))))

;; In emacs 19.29 and later, and XEmacs 19.13 and later, all messages are
;; recorded in a log.  Do not put curry-doc messages in that log since
;; they are Legion.

;@cindex curry-doc-message

(defmacro curry-doc-message (&rest args)
  (if (fboundp 'display-message)
      ;; XEmacs 19.13 way of preventing log messages.
      ;(list 'display-message '(quote no-log) (apply 'list 'format args))
      ;; XEmacs 19.15 seems to be a bit different
      (list 'display-message '(quote message) (apply 'list 'format args))
    (list 'let (list (list 'message-log-max 'nil))
          (apply 'list 'message args))))


;@node Menubar Support, Curry Doc Mode, Install as minor mode, top
;@section Menubar Support

; a dummy definition needed for xemacs (I know, it's horrible :-(
(if (and (string-match "XEmacs" emacs-version)
	 (not (functionp 'define-key-after)))
  (defun define-key-after (map seq con name)))

;@cindex curry-doc-install-keymap

(defun curry-doc-install-keymap ()
  "Install a menu for `curry-doc' as a submenu of `Pakcs'."
 (interactive)
 ; define a keymap `curry-doc-keymap' for the derive menu
 (if nil ; (keymapp curry-doc-keymap)
       nil
     (setq curry-doc-keymap (make-sparse-keymap))
     (define-key curry-doc-keymap [visit]
       '("Visit FTP home site" . curry-doc-visit-home))
     (define-key curry-doc-keymap [submit]
       '("Submit bug report" . curry-doc-submit-bug-report))
     (define-key curry-doc-keymap [dummy]
       '("---" . nil))
     (define-key curry-doc-keymap [make-index]
       '("Make global fct index" . curry-doc-make-global-fct-index))
     (define-key curry-doc-keymap [global-types-on]
       '("Toggle display of global types" . curry-doc-show-global-types))
     (define-key curry-doc-keymap [strategy-on]
       '("Toggle display of strategy ids" . curry-doc-show-strategy))
     (define-key curry-doc-keymap [user-defined-on]
       '("Toggle display of user defined ids" . curry-doc-show-user-defined))
     (define-key curry-doc-keymap [prelude-on]
       '("Toggle display of prelude functions" . curry-doc-show-prelude))
     (define-key curry-doc-keymap [reserved-ids-on]
       '("Toggle display of reserved ids" . curry-doc-show-reserved))
     (define-key curry-doc-keymap [curry-doc-on]
       '("Toggle curry-doc mode" . curry-doc-mode))
  )

 ; add the menu to the pakcs menu as last entry
 (cond 
  ((eq major-mode 'pakcs-mode)
   (let ((pakcsmap (lookup-key pakcs-mode-map [menu-bar Pakcs])))
	 (if (and (not curry-doc-xemacs-p) ; XEmacs has problems here
		  (not (lookup-key pakcsmap [curry-doc])))
	     (define-key-after pakcsmap [curry-doc] (cons "Curry-doc" curry-doc-keymap)
	       [Curry-doc mode]))
     ; add shortcuts for these commands
     (define-key pakcs-mode-map "\C-c\e/" 'curry-doc-check-active) ; for testing 
     (define-key pakcs-mode-map "\C-c\C-o" 'curry-doc-mode) 
     (if (not curry-doc-xemacs-p) 
	 (define-key pakcs-mode-map [C-S-M-mouse-3] 'curry-doc-ask-mouse-for-type))))
  ((eq major-mode 'curry-mode)
   ; add shortcuts for these commands
   (local-set-key "\C-c\e/" 'curry-doc-check-active) ; for testing 
   (local-set-key "\C-c\C-o" 'curry-doc-mode) 
   (if (not curry-doc-xemacs-p)
       (local-set-key [C-S-M-mouse-3] 'curry-doc-ask-mouse-for-type)) ) ))


;@node Curry Doc Mode, Switch it on or off, Menubar Support, top
;@section Curry Doc Mode

;@cindex curry-doc-mode

;;;###autoload
(defun curry-doc-mode (&optional prefix)
  "Enter curry-doc-mode for showing fct types in the echo area (see variable docstring)."
  (interactive "P")

  ;; Make sure it's on the post-command-idle-hook if defined, otherwise put
  ;; it on post-command-hook.  The former first appeared in Emacs 19.30.
  (setq curry-doc-mode
  	 (if prefix
  	     (or (listp prefix);; C-u alone
  		 (> (prefix-numeric-value prefix) 0))
  	   (not curry-doc-mode)))

  (and curry-doc-mode-hook
       curry-doc-mode
       (run-hooks 'curry-doc-mode-hook))

  ;; ToDo: replace binding of `post-command-idle-hook' by `run-with-idle-timer'
  (and curry-doc-mode
       (not (memq 'curry-doc-mode-print-current-symbol-info 
		  (if (boundp 'post-command-idle-hook)
		     post-command-idle-hook
		   post-command-hook)))
       (add-hook (if (boundp 'post-command-idle-hook)
		     'post-command-idle-hook
		   'post-command-hook)
		 'curry-doc-mode-print-current-symbol-info))

  (and (not curry-doc-mode)
       (memq 'curry-doc-mode-print-current-symbol-info 
	     (if (boundp 'post-command-idle-hook)
			post-command-idle-hook
		   post-command-hook))
       (remove-hook (if (boundp 'post-command-idle-hook)
			'post-command-idle-hook
		   'post-command-hook)
		 'curry-doc-mode-print-current-symbol-info))

  (and curry-doc-mode
       curry-doc-show-global-types
       (progn
	 (setq curry-doc-minor-mode-string " Curry-DOC")
	 (curry-doc-make-global-fct-index))  ; build type index for global fcts
       (setq curry-doc-minor-mode-string " Curry-Doc"))

  (if curry-doc-mode
      (curry-doc-install-keymap))

  (and (interactive-p)
       (if curry-doc-mode
           (message "curry-doc-mode is enabled")
         (message "curry-doc-mode is disabled")))
  curry-doc-mode)

;;@cindex curry-doc-show-global-types

;;;;###autoload
;(defun curry-doc-show-global-types (&optional prefix)
;  "*If non-nil, then enable display of global types in `curry-doc-mode'."
;  (interactive "P")
;  ;; toggle mode or set it based on prefix value
;  (setq curry-doc-show-global-types
;	(if prefix
;	    (>= (prefix-numeric-value prefix) 0)
;	  (not curry-doc-show-global-types)))

;  (cond (curry-doc-show-global-types
;	 ;; set mode string to reflect value of `curry-doc-show-global-types'
;	 (setq curry-doc-minor-mode-string " Curry-DOC")
;	 ;; build index (note: this can be quite expensive)
;	 (curry-doc-make-global-fct-index))
;	(t
;	 (setq curry-doc-minor-mode-string " Curry-Doc")) ) )


(defmacro curry-doc-toggle-var (id prefix)
  ;; toggle variable or set it based on prefix value
  (setq id
	(if prefix
	    (>= (prefix-numeric-value prefix) 0)
	  (not id))) )

;@cindex curry-doc-show-global-types
(defun curry-doc-show-global-types (&optional prefix)
  "Turn on global types information in `curry-doc-mode'."
  (interactive "P")
  (curry-doc-toggle-var curry-doc-show-global-types prefix)
  (if curry-doc-show-global-types
      (setq curry-doc-minor-mode-string " Curry-DOC")
    (setq curry-doc-minor-mode-string " Curry-Doc")) )

;@cindex curry-doc-show-reserved
(defun curry-doc-show-reserved (&optional prefix)
  "Toggle the automatic display of a doc string for reserved ids."
  (interactive "P")
  (curry-doc-toggle-var curry-doc-show-reserved prefix))

;@cindex curry-doc-show-prelude
(defun curry-doc-show-prelude (&optional prefix)
  "Toggle the automatic display of a doc string for reserved ids."
  (interactive "P")
  (curry-doc-toggle-var curry-doc-show-prelude prefix))

;@cindex curry-doc-show-strategy
(defun curry-doc-show-strategy (&optional prefix)
  "Toggle the automatic display of a doc string for strategy ids."
  (interactive "P")
  (curry-doc-toggle-var curry-doc-show-strategy prefix))

;@cindex curry-doc-show-user-defined
(defun curry-doc-show-user-defined (&optional prefix)
  "Toggle the automatic display of a doc string for user defined ids."
  (interactive "P")
  (curry-doc-toggle-var curry-doc-show-user-defined prefix))

;@node Switch it on or off, Check, Curry Doc Mode, top
;@section Switch it on or off

;@cindex turn-on-curry-doc-mode

;;;###autoload
(defun turn-on-curry-doc-mode ()
  "Unequivocally turn on curry-doc-mode (see variable documentation)."
  (interactive)
  (curry-doc-mode 1))

;@cindex  turn-off-curry-doc-mode

;;;###autoload
(defun turn-off-curry-doc-mode ()
  "Unequivocally turn off curry-doc-mode (see variable documentation)."
  (interactive)
  (curry-doc-mode 0))

;@node Check, Top level function, Switch it on or off, top
;@section Check

;@cindex curry-doc-check-active

(defun curry-doc-check-active ()
 "Check whether the print function is hooked in. 
Should be the same as the value of `curry-doc-mode' but alas currently it 
is not."
 (interactive)
 (message 
  (if (memq 'curry-doc-mode-print-current-symbol-info 
	    (if (boundp 'post-command-idle-hook)
		post-command-idle-hook
	      post-command-hook))
      "curry-doc is ACTIVE"
    "curry-doc is not ACTIVE \(Use C-u C-c C-o to turn it on\)")))

;@node Top level function, Mouse interface, Check, top
;@section Top level function

;@cindex curry-doc-mode-print-current-symbol-info
;; This is the function hooked into the elisp command engine
(defun curry-doc-mode-print-current-symbol-info ()
 "Print the type of the symbol under the cursor. 

This function is hooked into the `post-command-idle-hook' to print the type
automatically if `curry-doc-mode' is turned on. It can also be called 
directly to ask for the type of a function."
  (interactive)
  (and curry-doc-mode
       (not executing-kbd-macro)
       ;; Having this mode operate in the minibuffer makes it impossible to
       ;; see what you're doing.
       (not (eq (selected-window) (minibuffer-window)))
       ; take a nap
       (sit-for curry-doc-idle-delay)
       ; good morning! read the word under the cursor for breakfast
       (let ((current-symbol (curry-doc-get-current-word)) ); (curry-doc-current-symbol)) )
             ; (current-fnsym  (curry-doc-fnsym-in-current-sexp)))
	 (curry-doc-show-type current-symbol)) ))

;	 ; ToDo: find surrounding fct
;         (cond ((eq current-symbol current-fnsym)
;                (curry-doc-show-type current-fnsym))
;               (t
;                (or nil ; (curry-doc-print-var-docstring current-symbol)
;                    (curry-doc-show-type current-fnsym)))))))


;@node Mouse interface, Print fctsym, Top level function, top
;@section Mouse interface for interactive query

;@cindex curry-doc-ask-mouse-for-type
(defun curry-doc-ask-mouse-for-type (event)
 "Read the identifier under the mouse and echo its type.
This uses the same underlying function `curry-doc-show-type' as the hooked
function. Only the user interface is different."
 (interactive "e")
 (save-excursion
   (select-window (posn-window (event-end event)))
   (goto-char (posn-point (event-end event)))
   (curry-doc-show-type )))
 

;@node Print fctsym, Movement, Mouse interface, top
;@section Print fctsym

;@menu
;* Show type::			
;* Aux::			
;* Global fct type::		
;* Local fct type::		
;@end menu

;@node Show type, Aux, Print fctsym, Print fctsym
;@subsection Show type

;@cindex curry-doc-show-type

;;;###autoload
(defun curry-doc-show-type (&optional symbol)
  "Show the type of the function near point.
For the function under point, show the type in the echo area.
This information is extracted from the `curry-doc-prelude-types' alist of prelude functions and their types, or from the local functions in the current buffer."
  (interactive)
  (let* ((sym (or symbol (curry-doc-get-current-word))) 
	; (curry-doc-current-symbol))); (curry-doc-fnsym-in-current-sexp)))
        (printit t)
        (i-am-prelude nil)
        (i-am-fct nil)
        (type nil)
	(is-reserved (curry-doc-is-of sym curry-doc-reserved-ids))
	(is-prelude  (curry-doc-is-of sym curry-doc-prelude-types))
	(is-strategy (curry-doc-is-of sym curry-doc-strategy-ids))
	(is-user-defined (curry-doc-is-of sym curry-doc-user-defined-ids))
	(is-prelude  (curry-doc-is-of sym curry-doc-prelude-types)))
   (cond
	  ;; if printed before do not print it again
          ((string= sym (car curry-doc-last-data))
           (setq printit nil)
           (setq type (cdr curry-doc-last-data)))
	  ;; if reserved id (i.e. Curry keyword
	  ((and curry-doc-show-reserved
	       is-reserved)
	   (setq type (cdr is-reserved))
           (setcdr curry-doc-last-data type))
	  ;; if built-in function get type from docstring
          ((and (not (null curry-doc-show-prelude))
		is-prelude)
           (setq type (cdr is-prelude)) ; (cdr (assoc sym curry-doc-prelude-types)))
	   (if (= 2 (length type)) ; horrible hack to remove bad formatting
	       (setq type (car (cdr type))))
	   (setq i-am-prelude t)
	   (setq i-am-fct t)
           (setcdr curry-doc-last-data type))
	  ((and curry-doc-show-strategy
	       is-strategy)
	   (setq i-am-fct t)
	   (setq type (cdr is-strategy))
           (setcdr curry-doc-last-data type))
	  ((and curry-doc-show-user-defined
	       is-user-defined)
	   ; (setq i-am-fct t)
	   (setq type (cdr is-user-defined))
           (setcdr curry-doc-last-data type))
          (t
	   (let ( (x (curry-doc-get-and-format-fct-type sym)) )
	     (if (null x)
		 (setcdr curry-doc-last-data nil) ; if not found reset last data
	       (setq type (car x))
	       (setq i-am-fct (string= "Variables" (cdr x)))
	       (if (and curry-doc-show-global-types (null type))
		   (setq type (curry-doc-get-global-fct-type sym)))
	       (setcdr curry-doc-last-data type)))) )
    ;; ToDo: encode i-am-fct info into alist of types
    (and type
         printit
	 ; drop `::' if it's not a fct
	 (let ( (str (cond ((and i-am-fct (not curry-doc-chop-off-fctname))
			    (format "%s :: %s" sym type))
			   (t 
			    (format "%s" type)))) )
	   (if i-am-prelude
	       (add-text-properties 0 (1- (length str)) '(face bold) str))
	   (curry-doc-message str)))) )


;; ToDo: define your own notion of `near' to find surrounding fct
;(defun curry-doc-fnsym-in-current-sexp ()
;  (let* ((p (point))
;         (sym (progn
;		(forward-word -1)
;;                (while (and (forward-word -1) ; (curry-doc-forward-sexp-safe -1)
;;                            (> (point) (point-min))))
;                (cond ((or (= (point) (point-min))
;                           (memq (or (char-after (point)) 0)
;                                 '(?\( ?\"))
;                           ;; If we hit a quotation mark before a paren, we
;                           ;; are inside a specific string, not a list of
;                           ;; symbols.
;                           (eq (or (char-after (1- (point))) 0) ?\"))
;                       nil)
;                      (t (condition-case nil
;                             (read (current-buffer))
;                           (error nil)))))))
;    (goto-char p)
;    (if sym
;	(format "%s" sym)
;      sym) ) )

;;    (and (symbolp sym)
;;         sym)))

;@node Aux, Global fct type, Show type, Print fctsym
;@subsection Aux

;; ToDo: handle open brackets to decide if it's a wrapped type

;@cindex curry-doc-grab-line
(defun curry-doc-grab-line (fct-and-pos)
 "Get the type of an \(FCT POSITION\) pair from the current buffer."
; (if (null fct-and-pos)
;     "" ; fn is not a local fct
  (let ( (str ""))
   (goto-char (cdr fct-and-pos))
   (beginning-of-line)
   ;; search for start of type (phsp give better bound?)
   (if (null (search-forward "::" (+ (point) curry-doc-search-distance) t))
       ""
     (setq str (curry-doc-grab))        ; leaves point at end of line
     (while (curry-doc-wrapped-type-p)  ; while in a multi-line type expr
       (forward-line 1)
       (beginning-of-line)
       (skip-chars-forward " \t")
       (setq str (concat str (curry-doc-grab))))
     (curry-doc-string-nub-ws           ; squeeze string
      (if curry-doc-chop-off-context    ; no context 
	  (curry-doc-chop-off-context str)
	str)))))
  ; (concat (car fct-and-pos) "::" (curry-doc-string-nub-ws str))))

;@cindex curry-doc-wrapped-type-p
(defun curry-doc-wrapped-type-p ()
 "Check whether the type under the cursor is wrapped over several lines.
The cursor must be at the end of a line, which contains the type.
Currently, only the following is checked:
If this line ends with a `->' or the next starts with an `->' it is a 
multi-line type \(same for `=>'\).
`--' comments are ignored.
ToDo: Check for matching parenthesis!. "
 (save-excursion
   (let ( (here (point))
	  (lim (progn (beginning-of-line) (point)))
	  ; (foo "")
	  (res nil)
	  )
   (goto-char here)
   (search-backward "--" lim t) ; skip over `--' comment 
   (skip-chars-backward " \t")
   (if (bolp)                   ; skip empty lines
      (progn
       (forward-line 1)
       (end-of-line)
       (setq res (curry-doc-wrapped-type-p)))
   (forward-char -1)
   ; (setq foo (concat foo (char-to-string (preceding-char)) (char-to-string (following-char))))
   (if (or (and (or (char-equal (preceding-char) ?-) (char-equal (preceding-char) ?=))
		(char-equal (following-char) ?>)) ; (or -!> =!>
	   (char-equal (following-char) ?,))      ;     !,)
       (setq res t)
     (forward-line)
     (let ( (here (point))
	    (lim (progn (end-of-line) (point)))
	    )
       (goto-char here)
       (skip-chars-forward " \t")
       (if (looking-at "--")  ; it is a comment line
	   (progn
	     (forward-line 1)
	     (end-of-line)
	     (setq res (curry-doc-wrapped-type-p)))
	 (forward-char 1)
	 ; (setq foo (concat foo (char-to-string (preceding-char)) (char-to-string (following-char))))
	 ; (message "|%s|" foo)
	 (if (and (or (char-equal (preceding-char) ?-) (char-equal (preceding-char) ?=))
		  (char-equal (following-char) ?>)) ; -!> or =!>
	     (setq res t))))))
   res)))

;@cindex curry-doc-grab
(defun curry-doc-grab ()
 "Return the text from point to the end of the line, chopping off comments.
Leaves point at end of line."
 (let* ( (str (buffer-substring-no-properties (point) (progn (end-of-line) (point))))
	 (i (string-match "--" str)) )
   (if (null i)
       str
     (substring str 0 i))))

;@cindex curry-doc-string-nub-ws
(defun curry-doc-string-nub-ws (str)
  "Replace all sequences of whitespaces in STR by just one whitespace.
ToDo: Also eliminate leading and trainling whitespace."
 (interactive)
 (let (
       (res str)
       (i 0)
       ) 
  (setq i (string-match "\\(\\s-+\\)" res i))
  (while (not (null i))
    (setq res (replace-match " " t t res))
    (setq i (string-match "\\(\\s-+\\)" res (1+ i))) )
  res) )

; ToDo: make this more efficient!!
;(defun curry-doc-string-nub-ws (str)
;  "Replace all sequences of whitespaces in STR by just one whitespace."
;  (let ( (res "") 
;	 (l (length str))
;	 (i 0)
;	 (j 0)
;	 (in-ws nil))
;   (while (< i l)
;     (let* ( (c (string-to-char (substring str i (1+ i))))
;	    (is-ws (eq (char-syntax c) ? )) )
;       (if (not (and in-ws is-ws))
;	     (setq res (concat res (char-to-string c))))
;       (setq in-ws is-ws)
;       (setq i (1+ i))))
;   res))

;@cindex curry-doc-chop-off-context
(defun curry-doc-chop-off-context (str)
 "Eliminate the contex in a type represented by the string STR."
 (let ((i (string-match "=>" str)) ) 
   (if (null i)
       str
     (substring str (+ i 2)))))

;@cindex curry-doc-get-imenu-info
(defun curry-doc-get-imenu-info (obj kind)
  "Returns a string describing OBJ of KIND \(Variables, Types, Data\)."
  (cond ((or (eq major-mode 'pakcs-mode)
             ;; GEM: Curry Mode does not work with Curry Doc
             ;;      under XEmacs 20.x
             (and (eq major-mode 'curry-mode)
                  (not (and curry-doc-xemacs-p
                            (string-match "^20" emacs-version)))))
	 (let* ( (imenu-info-alist (cdr (assoc kind imenu--index-alist)))
		 ; (names (mapcar (lambda (x) (car x)) imenu-info-alist))
		 (x (assoc obj imenu-info-alist))
	       )
	     (if x
		 (curry-doc-grab-line x)
	       nil)) )
	  (t
	   ; (error "Cannot get local functions in %s mode, sorry" major-mode))) )
	   nil)))

;@node Global fct type, Local fct type, Aux, Print fctsym
;@subsection Global fct type

;; ToDo:
;;  - modular way of defining a mapping of module name to file
;;  - use a path to search for file (not just current directory)

;@cindex curry-doc-imported-list

(defun curry-doc-imported-list (outer-file)
  "Return a list of the imported modules in OUTER-FILE."
  (interactive "fName of outer `include' file: ") ;  (buffer-file-name))
  (let ((imported-file-list (list outer-file))
        start)
    (save-excursion
      (switch-to-buffer (find-file-noselect outer-file))
      (widen)
      (goto-char (point-min))
      (while (re-search-forward "^\\s-*import\\s-+" nil t)
        (skip-chars-forward " \t")
        (setq start (point))
        (end-of-line)
        (skip-chars-backward " \t")
	(let ( (file (concat (buffer-substring start (point)) ".hs")) )
	  (if (file-exists-p file)
	      (setq imported-file-list
		    (cons file imported-file-list))))
	(let ( (file (concat (buffer-substring start (point)) ".lhs")) )
	  (if (file-exists-p file)
	      (setq imported-file-list
		    (cons file imported-file-list))))
      )
      (nreverse imported-file-list)
      ;(message imported-file-list)
)))

;; ToDo: generalise this to "Types" etc (not just "Variables")

;@cindex curry-doc-rescan-files

(defun curry-doc-rescan-files (filelist)
 "Does an `imenu' rescan on every file in FILELIST and returns the fct-list.
This function switches to and potentially loads many buffers."
   (mapcar (lambda (f)
	     (switch-to-buffer (find-file-noselect f))
	     (imenu--make-index-alist)
	     (let ( (fn-alist (cdr (assoc "Variables" imenu--index-alist)) ) )
	       (cons f
		     (mapcar (lambda (x)
			       `(,(car x) . ,(curry-doc-grab-line x)) )
			     fn-alist)) ) )
   filelist ) )

;@cindex curry-doc-make-global-fct-index

(defun curry-doc-make-global-fct-index ()
 "Scan imported files for types of global fcts and update `curry-doc-index'."
 (interactive)
 (let* ( (this-buffer (current-buffer))
	 (this-file (buffer-file-name))
	 (x (curry-doc-rescan-files (curry-doc-imported-list this-file) )) )
   (switch-to-buffer this-buffer)
   ;; curry-doc-index is buffer local => switch-buffer before setq
   (setq curry-doc-index x) ) )

;; ToDo: use a separate munge-type function to format type concisely

;@cindex curry-doc-get-global-fct-type

(defun curry-doc-get-global-fct-type (&optional sym)
 "Get type for function symbol SYM by examining `curry-doc-index'."
  (interactive) ;  "fName of outer `include' file: \nsFct:")
  (save-excursion
  ; (switch-to-buffer "*scratch*")
  ; (goto-char (point-max))
  ;; Produces a list of fct-type alists
;  (if (null sym)
;      (setq sym (progn (forward-word -1) (read (current-buffer)))))
  (or sym
      (current-word))
  (let* ( (fn sym) ; (format "%s" sym))
	  (fal curry-doc-index)
	  (res "") )
    (while (not (null fal))
      (let* ( (l (car fal))
	      (f (car l))
	      (x (assoc fn (cdr l))) )
	(if (not (null x))
	    (let* ( (ty (cdr x)) ; the type as string
		    (idx (string-match "::" ty))
		    (str (if (null idx)
			     ty
			   (substring ty (+ idx 2)))) )
	      (setq res (format "[%s] %s" f str))))
	  (setq fal (cdr fal))))
    res))) ; (message res)) )

;@node Local fct type,  , Global fct type, Print fctsym
;@subsection Local fct type

;@cindex curry-doc-get-and-format-fct-type

(defun curry-doc-get-and-format-fct-type (fn)
 "Get the type and kind of FN by checking local and global functions."
 (save-excursion
   (save-match-data
     (let ((docstring "")
	   (doc nil)
	   )
       ; is it a local function?
       (setq docstring (curry-doc-get-imenu-info fn "Variables"))
       (if (not (null docstring))
		; (string-match (format "^%s\\s-+::\\s-+\\(.*\\)$" fn) docstring))
	   (setq doc `(,docstring . "Variables"))) ; `(,(match-string 1 docstring) . "Variables") ))
       ; is it a type declaration?
       (setq docstring (curry-doc-get-imenu-info fn "Types"))
       (if (not (null docstring))
		; (string-match (format "^\\s-*type\\s-+%s.*$" fn) docstring))
	     (setq doc `(,docstring . "Types"))) ; `(,(match-string 0 docstring) . "Types")) )
       (if (not (null docstring))
		; (string-match (format "^\\s-*data.*%s.*$" fn) docstring))
	 (setq doc `(,docstring . "Data"))) ; (setq doc `(,(match-string 0 docstring) . "Data")) )
       ; return the result
       doc ))))


;@node Movement, Bug Reports, Print fctsym, top
;@section Movement
; Functions for moving in text and extracting the current word under the cursor

; prbly nukable

;; forward-sexp calls scan-sexps, which returns an error if it hits the
;; beginning or end of the sexp.  This returns nil instead.
(defun curry-doc-forward-sexp-safe (&optional count)
  "Move forward across one balanced expression (sexp).
With argument, do it that many times.  Negative arg -COUNT means
move backward across COUNT balanced expressions.
Return distance in buffer moved, or nil."
  (or count (setq count 1))
  (condition-case err
      (- (- (point) (progn
                      (let ((parse-sexp-ignore-comments t))
                        (forward-sexp count))
                      (point))))
    (error nil)))

;; Do indirect function resolution if possible.
;(defun curry-doc-symbol-function (fsym)
;  (let ((defn (and (fboundp fsym)
;                   (symbol-function fsym))))
;    (and (symbolp defn)
;         (condition-case err
;             (setq defn (indirect-function fsym))
;           (error (setq defn nil))))
;    defn))

;; HWL: currently unused; this is taken from eldoc

(defun curry-doc-current-symbol ()
  (let ((c (char-after (point))))
    (and c
         (memq (char-syntax c) '(?w ?_))
         (current-word))))

;; HWL: my attempt at more efficient (current-word)

;@cindex curry-doc-is-id-char-at
(defsubst curry-doc-is-id-char-at (x)
 (let ( (c (char-syntax (char-after x))) )
   (or (eq c ?w) (eq c ?_))) )

;; NB: this function is called from within the hooked print function;
;;     therefore this function must not fail, otherwise the function will
;;     be de-installed;
;;     if no word under the cursor return an empty string
;@cindex curry-doc-get-current-word
(defun curry-doc-get-current-word ()
 "Return the word under the cursor, or empty string if no word found."
 ; (interactive)
 (if (bobp)
     ""
 (let ((x (1- (point)))
       (beg)
       (end)
       )
 ; go back to first non-word char 
 (while (and (> x (point-min)) (curry-doc-is-id-char-at x))  ; (not (bobp))
   (setq x (1- x)) )
 (if (= x (point-min))
     (setq beg x)
   (setq beg (1+ x)))
 (setq x (1+ x))
 (while (and (< x (point-max)) (curry-doc-is-id-char-at x)) ; (not (eobp))
   (setq x (1+ x)) )
 (setq end x)
 (buffer-substring-no-properties beg end))))

;@node Bug Reports, Visit home site, Movement, top
;@section Bug Reports

;@cindex curry-doc-submit-bug-report
; send a bug report
(defun curry-doc-submit-bug-report ()
  "Send email to the maintainer of `curry-doc'."
  (interactive)
  ;; In case we can't find reporter...
  (condition-case err
      (progn
        (require 'reporter)
	(and (y-or-n-p "Do you really want to submit a bug report? ")
        (reporter-submit-bug-report
	 curry-doc-maintainer                               ; address
	 (concat "curry-doc.el " curry-doc-version)       ; package
	 curry-doc-varlist                                  ; varlist
         nil nil                                        ; pre-/post-hooks
        "I have detected the following strange behaviour/bug in `curry-doc':\n")))
    ;; ...fail gracefully.
    (error
     (beep)
     (message "Sorry, reporter.el not found."))))

;@node Visit home site, Index, Bug Reports, top
;@section Visit home site

;@cindex curry-doc-visit-home

(defun curry-doc-visit-home ()
 "Jump to the main FTP site for `curry-doc'."
 (interactive)
 (if curry-doc-xemacs-p
    (require 'efs)
  (require 'ange-ftp))
 (require 'dired)
 (dired-other-window curry-doc-ftp-site))

;@appendix

;@node Index, Token, Visit home site, top
;@section Index

;@index
;* curry-doc-ask-mouse-for-type::
;* curry-doc-check-active::
;* curry-doc-chop-off-context::
;* curry-doc-get-and-format-fct-type::
;* curry-doc-get-current-word::
;* curry-doc-get-global-fct-type::
;* curry-doc-get-imenu-info::
;* curry-doc-grab::
;* curry-doc-grab-line::
;* curry-doc-imported-list::
;* curry-doc-install-keymap::
;* curry-doc-is-id-char-at::
;* curry-doc-is-of::
;* curry-doc-make-global-fct-index::
;* curry-doc-message::
;* curry-doc-mode::
;* curry-doc-mode-print-current-symbol-info::
;* curry-doc-prelude-types::
;* curry-doc-rescan-files::
;* curry-doc-reserved-ids::
;* curry-doc-show-global-types::
;* curry-doc-show-prelude::
;* curry-doc-show-reserved::
;* curry-doc-show-strategy::
;* curry-doc-show-type::
;* curry-doc-show-user-defined::
;* curry-doc-strategy-ids::
;* curry-doc-string-nub-ws::
;* curry-doc-submit-bug-report::
;* curry-doc-visit-home::
;* curry-doc-wrapped-type-p::
;* turn-off-curry-doc-mode::
;* turn-on-curry-doc-mode::
;@end index

;@node Token,  , Index, top
;@section Token

(provide 'curry-doc)

;;; curry-doc.el ends here
