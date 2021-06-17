package DataIORef

import "gocurry"
import "curry2go/Prelude"

var names []string = []string{"IORef"}

func IORefCreate(root *gocurry.Node, args ...*gocurry.Node)(*gocurry.Node){
    return gocurry.ConstCreate(root, 0, 1, &names[0], args...)
}

func ExternalData_IORef_newIORef(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    gocurry.IOCreate(root, IORefCreate(root.NewNode(), x1))
}

func ExternalData_IORef_prim_readIORef(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := x1.GetChild(0)
    
    gocurry.IOCreate(root, x2)
}

func ExternalData_IORef_prim_writeIORef(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)
    
    x1.SetChild(0, x2)
    
    gocurry.IOCreate(root, Prelude.Prelude__CREATE_LbRb(root.NewNode()))
}
