Installation of Curry2Go
========================

Requirements
------------

An installation of Go (version 1.13 or newer) is necessary,
which can be downloaded from <https://golang.org/>.


Quick installation
------------------

    > curl -sSL https://www-ps.informatik.uni-kiel.de/curry2go/download.sh | sh

This installs the Curry2Go system into the local directory `Curry2Go`
which is created by the installation. Then add `.../Curry2Go/bin` to
your path to start the Curry2Go REPL by the command

    > curry2go

If you do not use packages but only the base libraries contained
in the distribution, add option `-n` or `--nocypm` for faster startup:

    > curry2go -n

In order to install into a (non-existing!) directory `C2GDIR`, add an argument:

    > curl -sSL https://www-ps.informatik.uni-kiel.de/curry2go/download.sh | sh -s - -d C2GDIR

Then add `C2GDIR/bin` to your path to start the Curry2Go REPL by the command

    > curry2go


Installation with building the Curry front end
----------------------------------------------

The distribution of Curry2Go contains a binary of the Curry front end.
If this does not work on the local machine (due to incompatible
libraries), one can perform an installation which downloads
the Curry front end from its repository and build it from the sources.
To do so, the option `--frontend` should be added, e.g.,

    > curl ... | sh -s - --frontend

Note that the front-end build requires
[Haskell stack v2.x](http://www.haskellstack.org/).


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
    > make CURRYSYSTEM=/usr/local/pakcs/bin/pakcs bootstrap

Then add `.../curry2go/bin` to your path to start the Curry2Go REPL
by the command

    > curry2go

------------------------------------------------------------------------------
