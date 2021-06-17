package main

import "gocurry"
import "gocurry/Curry2Go/Main"

func main(  )(  ){
    node := Curry2GoMain.Curry2GoMain__CREATE_main( new( gocurry.Node ) )
    gocurry.Evaluate( node, false, false, gocurry.FS, 0, 0 )
}

