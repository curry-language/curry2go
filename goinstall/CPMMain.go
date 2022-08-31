package main

import "gocurry"
import "curry2go/CPM/Main"

func main(  )(  ){
    node := CPM_DOT_Main.CPM_DOT_Main__CREATE_main( new( gocurry.Node ) )
    gocurry.Evaluate( node, false, false, false, gocurry.FS, 0, 0, 0,
                      false, []string{  } )
}

