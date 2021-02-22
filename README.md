curry2go
========

This package contains the implementation of a compiler
and run-time system to compile and run Curry programs as Go programs.


Installation:
-------------

An installation of Go is necessary, which can be downloaded
from <https://golang.org/>.

The directory `gocurry` has to be copied to a workspace directory of Go.
Workspace directories can be set using the environment variable `GOPATH`.
If the variable is not set, the workspace directory `$HOME/go`
is assumed. Thus, in the simplest case, it is sufficient
to copy the directory `gocurry` as follows:

    > mkdir -p ~/go/src
    > cp -r gocurry ~/go/src/

All this is automized by a Makefile. Thus, simply run

    > make

This copies the required Go files and installs the compiler
as executable `$HOME/.cpm/bin/curry2go`.
Therefore, you should include the directory `$HOME/.cpm/bin`
into your path for convenient usage of the compiler.
Furthermore, a simple interactive environment for the compiler
(a basic REPL) is generated as executable `$HOME/.cpm/bin/c2go`.


Using the Compiler:
-------------------

After installation with `cypm install`, the compiler can be
invoked from a shell with the command:

    > curry2go [options] <program>

To see the list of current options, execute

    > curry2go --help

Examples:
---------

Some example programs can be found in the directory `examples`.
For instance, to compile the program `Rev.curry` and
evaluate the `main` function of this program, go into
the `examples` directory and execute

    > curry2go -r Rev


Using the interactive environment
---------------------------------

Start `c2go` and look into the command `:help`.


Package contents
----------------

* `benchmarks`: Benchmarks to evaluate the compiler
* `examples`:   Example programs
* `gocurry`:    Run-time system and primitives implemented in Go
* `src`:        The source code of the compiler
