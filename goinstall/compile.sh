#!/bin/sh -x

GODIR=.curry/curry2go-1.0.0

# Compile the Curry2Go compiler
cp goinstall/Compiler.go $GODIR
go build .curry/curry2go-1.0.0/Compiler.go
mv Compiler bin/curry2goc

# Compile the Curry2Go REPL
cp goinstall/REPL.go $GODIR
go build .curry/curry2go-1.0.0/REPL.go
mv REPL bin/curry2goi
