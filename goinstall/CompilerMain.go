package main

import "gocurry"
import "curry2go/Curry2Go/Main"

func main(  )(  ){
    node := Curry2Go_DOT_Main.Curry2Go_DOT_Main__CREATE_main( new( gocurry.Node ) )
    gocurry.Evaluate( node, false, false, gocurry.FS, 0, 0, 0 )
}

