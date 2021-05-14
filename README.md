curry2go
========

This package contains the implementation of a compiler
and run-time system to compile and run Curry programs as Go programs.


Installation:
-------------

An installation of Go (version 1.13 to 1.16) is necessary,
which can be downloaded from <https://golang.org/>.
The environment variable `GO111MODULE` has to be
either `auto` (default in versions 1.13 to 1.15) or `off`,
because the Curry2Go compiler uses the GOPATH build mode.
With version 1.16 the variable defaults to `on` and
has to be set manually, which can be done with:

    > go env -w GO111MODULE=auto


Since the Curry2Go compiler is implemented in Curry,
you need another Curry compiler for the initial
installation, e.g., [PAKCS](https://www.informatik.uni-kiel.de/~pakcs/)
(if you use another Curry compiler, adapt the `Makefile`).
Then, simply run

    > make

This copies the directory `gocurry` to a workspace directory of Go.
Workspace directories can be set using the environment variable `GOPATH`.
If the variable is not set, the workspace directory `$HOME/go`
is assumed. Thus, the `make` command copies the directory `gocurry`
as follows:

    > mkdir -p ~/go/src
    > cp -r gocurry ~/go/src/

Furthermore, the Curry2Go compiler is installed as executable
`bin/curry2goc` and a simple interactive execution environment
(a basic "REPL") as executable `bin/curry2goi`.
However, it is not necessary to use these executables directly.
Instead, the make process installs in the `bin` directory of this package
a script `curry2go` (and an alias `curry` for this script)
so that you should include the directory `bin` of this
package into your path for convenient usage of `curry2go`.


Using the interactive environment
---------------------------------

Start `curry2go` and look into the command `:help`.


Using the Curry2Go compiler:
----------------------------

After installation with `cypm install`, the compiler can be
invoked from a shell with the command:

    > curry2goc [options] <program>

To see the list of current options, execute

    > curry2goc --help

Examples:
---------

Some example programs can be found in the directory `examples`.
For instance, to compile the program `Rev.curry` and
evaluate the `main` function of this program, go into
the `examples` directory and execute

    > curry2goc -r Rev


Remarks:
--------

The default search strategy is "fair search", i.e., non-deterministic
branches are evaluated by different
[goroutines](https://tour.golang.org/concurrency/1).
The actual number of CPUs used to execute these routines
can be influenced by setting the environment variable `GOMAXPROCS`.
This variable limits the number of operating system threads
that can execute Go code simultaneously.
For instance, by setting

    > export GOMAXPROCS=2

before invoking Curry2Go enforces the limit to two CPUs.


Package contents:
----------------

* `benchmarks`: Benchmarks to evaluate the compiler
* `examples`:   Example programs
* `gocurry`:    Run-time system and primitives implemented in Go
* `lib`:        The base libraries of the distribution (basically a copy
                of the Curry package base)
* `src`:        The source code of the compiler
* `scripts`:    Scripts installed into `bin` by the Makefile
