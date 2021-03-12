package SystemIOUnsafe

import "gocurry"
import "../../../Prelude"


func ExternalSystem_IO_Unsafe_unsafePerformIO(task *gocurry.Task){
    root := task.GetControl()
    action := root.GetChild(0)
    
    if(!action.IsHnf()){
        task.ToHnf(action)
        return
    }
    
    gocurry.RedirectCreate(root, action.GetChild(0))
}

func ExternalSystem_IO_Unsafe_spawnConstraint(task *gocurry.Task){
    panic("Not yet implemented")
}

func ExternalSystem_IO_Unsafe_prim_isVar(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    if(x1.IsFree() && !task.IsBound(x1)){
        Prelude.Prelude_TrueCreate(root)
    } else{
        Prelude.Prelude_FalseCreate(root)
    }
}

func ExternalSystem_IO_Unsafe_prim_identicalVar(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)
    
    if(!x1.IsFree() || task.IsBound(x1)){
        Prelude.Prelude_FalseCreate(root)
        return
    }
    
    if(!x2.IsFree() || task.IsBound(x2)){
        Prelude.Prelude_FalseCreate(root)
        return
    }
    
    if(x1 == x2){
        Prelude.Prelude_TrueCreate(root)
    } else{
        Prelude.Prelude_FalseCreate(root)
    }
}

func ExternalSystem_IO_Unsafe_prim_isGround(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    if(x1.IsFree()){
        Prelude.Prelude_FalseCreate(root)
        return
    }
    
    if(x1.GetNumChildren() == 0){
        Prelude.Prelude_TrueCreate(root)
        return
    }
    
    if(x1.GetNumChildren() == 1){
        SystemIOUnsafe_prim_isGroundCreate(root, x1.GetChild(0))
        return
    }
    
    node := Prelude.Prelude_AndAndCreate(root, SystemIOUnsafe_prim_isGroundCreate(root.NewNode(), x1.GetChild(0)), root.NewNode())
    
    for i := 0; i < x1.GetNumChildren() - 1; i ++{
        Prelude.Prelude_AndAndCreate(node.GetChild(1), SystemIOUnsafe_prim_isGroundCreate(root.NewNode(), x1.GetChild(i)), root.NewNode())
        node = node.GetChild(1)
    }
    
    node.SetChild(1, SystemIOUnsafe_prim_isGroundCreate(root.NewNode(), x1.GetChild(x1.GetNumChildren() - 1)))
}

func ExternalSystem_IO_Unsafe_compareAnyTerm(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)
    
    if(x1.IsFree() && x2.IsFree()){
        if(x1 == x2){
            Prelude.Prelude_EQCreate(root)
            return
        }else {
            if(x1.GetName() < x2.GetName()){
                Prelude.Prelude_LTCreate(root)
            } else{
                Prelude.Prelude_GTCreate(root)
            }
            return
        }
    } else if(x1.IsFree()){
        Prelude.Prelude_LTCreate(root)
        return
    } else if(x2.IsFree()){
        Prelude.Prelude_GTCreate(root)
        return
    }
    
    if(x1.GetConstructor() < x2.GetConstructor()){
        Prelude.Prelude_LTCreate(root)
    } else if(x1.GetConstructor() > x2.GetConstructor()){
        Prelude.Prelude_GTCreate(root)
    } else{
        for i := range(x1.Children){
            SystemIOUnsafe_compareAnyTermCreate(root, x1.GetChild(i), x2.GetChild(i))
            ExternalSystem_IO_Unsafe_compareAnyTerm(task)
            if(root.GetConstructor() != 1){
                return   
            }
        }
        Prelude.Prelude_EQCreate(root)
    }
}

func ExternalSystem_IO_Unsafe_prim_showAnyTerm(task *gocurry.Task){
    panic("Not yet implemented")
}

func ExternalSystem_IO_Unsafe_prim_showAnyQTerm(task *gocurry.Task){
    panic("Not yet implemented")
}

func ExternalSystem_IO_Unsafe_prim_readsAnyUnqualifiedTerm(task *gocurry.Task){
    panic("Not yet implemented")
}

func ExternalSystem_IO_Unsafe_prim_readsAnyQTerm(task *gocurry.Task){
    panic("Not yet implemented")
}

func ExternalSystem_IO_Unsafe_showAnyExpression(task *gocurry.Task){
    panic("Not yet implemented")
}

func ExternalSystem_IO_Unsafe_showAnyQExpression(task *gocurry.Task){
    panic("Not yet implemented")
}

func ExternalSystem_IO_Unsafe_prim_readsAnyQExpression(task *gocurry.Task){
    panic("Not yet implemented")
}












