Release Notes for Curry2Go
==========================

Release notes for version 1.2.0 (November 1, 2023)
---------------------------------------------------

  * Base libraries extended by including libraries for encapsulated search
    so that set functions can be used without installing packages.
    The new libraries are: `Control.Search.SetFunctions`(implementing
    set functions), `Control.Search.AllValues` (implementing a strong
    encapsulation as I/O operations), `Control.Search.Unsafe`
    (implementing strong encapsulation as non I/O operations, but this
    method has a non-declarative behavior), and `Control.Search.SearchTree`
    (implementing search trees which are mainly used in KiCS2 to implement
    encapsulation).


Release notes for version 1.1.0 (February 17, 2023)
---------------------------------------------------

* add debug/trace mode (activated by command `:set +debug`)
  in order to show the individual computation steps and tasks
  (for the fair search strategy)
* Update CPM (with new resource URLs)


Release notes for version 1.0.1 (February 3, 2022)
--------------------------------------------------

* implementation of packages `socket` and `bindata` completed
* new name scheme to avoid name clashes in translated entity names
* raising failures and non-determinism in I/O actions added


Release notes for version 1.0.0 (October 8, 2021)
-------------------------------------------------

Complete distribution, containing the compiler to Go,
the REPL and the Curry Package Manager.

-------------------------------------------------------------------------

