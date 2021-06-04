package main

import "gocurry"
import "./Curry2Go/REPL"

func main(  )(  ){
    node := Curry2GoREPL.Curry2GoREPL__CREATE_main( new( gocurry.Node ) )
    gocurry.Evaluate( node, false, false, gocurry.FS, 0, 0 )
}

