package DataIORef

import "gocurry"
import "../../Prelude"

func ExternalData_IORef_newIORef(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    gocurry.IOCreate(root, DataIORef_IORefCreate(root.NewNode(), x1))
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
    
    gocurry.IOCreate(root, Prelude.Prelude_LbRbCreate(root.NewNode()))
}
