Auxiliary Tools for Curry2Go
============================

This directory contains auxiliary definitions to adapt programming tools
for Curry2Go.

Currently it contains:

* `emacs`: Emacs mode for editing Curry programs
* `gedit`: Curry mode for gedit
* `rlwrap.words`: list of word completions for `rlwrap`


Note for rlwrap:
----------------

Version 0.43 of rlwrap contains a bug which erases the prompt:

https://github.com/hanslub42/rlwrap/issues/109

To avoid this bug in this version, put the following line into the
`~/.inputrc` file:

    set enable-bracketed-paste off
