package Data_DOT_IORef

import "gocurry"
import "curry2go/Prelude"

var names []string = []string{"IORef"}

func IORefCreate(root *gocurry.Node, args ...*gocurry.Node)(*gocurry.Node){
    return gocurry.ConstCreate(root, 0, 1, &names[0], args...)
}

func ExternalDataDot_IORefDot_newIORef(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    gocurry.IOCreate(root, IORefCreate(root.NewNode(), x1))
}

func ExternalDataDot_IORefDot_primUs_readIORef(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := x1.GetChild(0)
    
    gocurry.IOCreate(root, x2)
}

func ExternalDataDot_IORefDot_primUs_writeIORef(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)
    
    x1.SetChild(0, x2)
    
    gocurry.IOCreate(root, Prelude.Prelude__CREATE_Lb_Rb_(root.NewNode()))
}
