package main

import "gocurry"
import "curry2go/Curry2Go/REPL"

func main(  )(  ){
    node := Curry2Go_DOT_REPL.Curry2Go_DOT_REPL__CREATE_main( new( gocurry.Node ) )
    gocurry.Evaluate( node, false, false, false, gocurry.FS, 0, 0, 0,
                      false, []string{  } )
}

