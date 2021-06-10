Installation of Curry2Go
========================

Requirements
------------

An installation of Go (version 1.13 to 1.16) is necessary,
which can be downloaded from <https://golang.org/>.
The environment variable `GO111MODULE` has to be
either `auto` (default in versions 1.13 to 1.15) or `off`,
because the Curry2Go compiler uses the GOPATH build mode.
With version 1.16 the variable defaults to `on` and
has to be set manually, which can be done with:

    > go env -w GO111MODULE=auto


Quick installation into directory /opt/Curry2Go
-----------------------------------------------

    > curl -sSL https://www.informatik.uni-kiel.de/~mh/curry2go/download.sh | sh

Since this installs the Curry2Go system in directory `/opt/Curry2Go`,
you must allowed to run the `sudo` command.
Then add `/opt/Curry2Go/bin` to your path to start the Curry2Go REPL
by the command

    > curry2go

Note that this installation is _not relocatable_, i.e., it cannot
be moved into some other directory. If you want to have
a Curry2Go system in some other directory, this system
can be used to install another Curry2Go system by bootstrapping,
see <https://en.wikipedia.org/wiki/Bootstrapping_(compilers)>,
which needs some more time, as described below.


Quick installation into directory /tmp/Curry2Go
-----------------------------------------------

If you want to install Curry2Go without giving `sudo` permissions,
you can install into the temporary directory `/tmp/Curry2Go` by

    > curl -sSL https://www.informatik.uni-kiel.de/~mh/curry2go/download.sh | sh -s - -t

Then add `/tmp/Curry2Go/bin` to your path to start the Curry2Go REPL
by the command

    > curry2go

Note that this installation is _not relocatable_, i.e., it cannot
be moved into some other directory.


Installation of Curry2Go into some other directory
--------------------------------------------------

In order to install a local Curry2Go system in directory `C2GDIR`
(which needs more time since the complete Curry2Go system will be
compiled on the local machine), add an argument to the above command:

    > curl ... | sh -s - -d C2GDIR

This installation does not require `sudo` since
an initial Curry2Go compiler is installed into `/tmp/Curry2Go`
which is afterwards used to compile and install another Curry2Go system
in directory `C2GDIR`.
After this installation, add `C2GDIR/bin` to your path to start
the Curry2Go REPL by the command

    > curry2go

Note that this also installs a local version of the Curry Package Manager CPM
into `~/.cpm/bin/cypm` (which is compiled with the local Curry2Go system).
If this CPM version should not be installed (which might be reasonable
if CPM is already installed), add the argument `--nocypm`, i.e.,

    > curl ... | sh -s - -d C2GDIR --nocypm


Installation with an existing Curry system and CPM
--------------------------------------------------

If you have already another Curry system (e.g., PAKCS) and CPM
installed (i.e., the executable `cypm` must be in your path),
you can install Curry2Go with this Curry system. To do so,
run the following commands to clone the Curry2Go repository
and install Curry2Go (where the definition of the parameter
`CURRYSYSTEM` must be set according to the installation of
the Curry system):

    > git clone https://git.ps.informatik.uni-kiel.de/curry/curry2go.git
    > cd curry2go
    > make CURRYSYSTEM=/usr/local/pakcs/bin/pakcs
    > make bootstrap

Then add `.../curry2go/bin` to your path to start the Curry2Go REPL
by the command

    > curry2go

------------------------------------------------------------------------------