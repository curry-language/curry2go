package main

import "gocurry"
import "./CPM/Main"

func main(  )(  ){
    node := CPMMain.CPMMain__CREATE_main( new( gocurry.Node ) )
    gocurry.Evaluate( node, false, false, gocurry.FS, 0, 0 )
}

