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
    // TODO: make concurrent
    root := task.GetControl()
    constraint := root.GetChild(0)
    
    if(!constraint.IsHnf()){
        task.ToHnf(constraint)
        return
    }
    
    if(constraint.GetConstructor() == 0){
        gocurry.ExemptCreate(root)
    } else{
        gocurry.RedirectCreate(root, root.GetChild(1))
    }
}

func ExternalSystem_IO_Unsafe_prim_isVar(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    if(x1.IsFree() && !task.IsBound(x1)){
        Prelude.Prelude__CREATE_True(root)
    } else{
        Prelude.Prelude__CREATE_False(root)
    }
}

func ExternalSystem_IO_Unsafe_prim_identicalVar(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)
    
    if(!x1.IsFree() || task.IsBound(x1)){
        Prelude.Prelude__CREATE_False(root)
        return
    }
    
    if(!x2.IsFree() || task.IsBound(x2)){
        Prelude.Prelude__CREATE_False(root)
        return
    }
    
    if(x1 == x2){
        Prelude.Prelude__CREATE_True(root)
    } else{
        Prelude.Prelude__CREATE_False(root)
    }
}

func ExternalSystem_IO_Unsafe_prim_isGround(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    if(x1.IsFree()){
        Prelude.Prelude__CREATE_False(root)
        return
    }
    
    if(x1.GetNumChildren() == 0){
        Prelude.Prelude__CREATE_True(root)
        return
    }
    
    if(x1.GetNumChildren() == 1){
        SystemIOUnsafe__CREATE_prim_isGround(root, x1.GetChild(0))
        return
    }
    
    node := Prelude.Prelude__CREATE_AndAnd(root, SystemIOUnsafe__CREATE_prim_isGround(root.NewNode(), x1.GetChild(0)), root.NewNode())
    
    for i := 0; i < x1.GetNumChildren() - 1; i ++{
        Prelude.Prelude__CREATE_AndAnd(node.GetChild(1), SystemIOUnsafe__CREATE_prim_isGround(root.NewNode(), x1.GetChild(i)), root.NewNode())
        node = node.GetChild(1)
    }
    
    node.SetChild(1, SystemIOUnsafe__CREATE_prim_isGround(root.NewNode(), x1.GetChild(x1.GetNumChildren() - 1)))
}

func ExternalSystem_IO_Unsafe_compareAnyTerm(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)
    
    if(x1.IsFree() && x2.IsFree()){
        if(x1 == x2){
            Prelude.Prelude__CREATE_EQ(root)
            return
        }else {
            if(x1.GetName() < x2.GetName()){
                Prelude.Prelude__CREATE_LT(root)
            } else{
                Prelude.Prelude__CREATE_GT(root)
            }
            return
        }
    } else if(x1.IsFree()){
        Prelude.Prelude__CREATE_LT(root)
        return
    } else if(x2.IsFree()){
        Prelude.Prelude__CREATE_GT(root)
        return
    }
    
    if(x1.GetConstructor() < x2.GetConstructor()){
        Prelude.Prelude__CREATE_LT(root)
    } else if(x1.GetConstructor() > x2.GetConstructor()){
        Prelude.Prelude__CREATE_GT(root)
    } else{
        for i := range(x1.Children){
            SystemIOUnsafe__CREATE_compareAnyTerm(root, x1.GetChild(i), x2.GetChild(i))
            ExternalSystem_IO_Unsafe_compareAnyTerm(task)
            if(root.GetConstructor() != 1){
                return   
            }
        }
        Prelude.Prelude__CREATE_EQ(root)
    }
}

func ExternalSystem_IO_Unsafe_prim_showAnyTerm(task *gocurry.Task){
    panic("System.IO.Unsafe.showAnyTerm: Not yet implemented")
}

func ExternalSystem_IO_Unsafe_prim_readsAnyUnqualifiedTerm(task *gocurry.Task){
    panic("System.IO.Unsafe.readsAnyUnqualifiedTerm: Not yet implemented")
}

func ExternalSystem_IO_Unsafe_showAnyExpression(task *gocurry.Task){
    panic("System.IO.Unsafe.showAnyExpression: Not yet implemented")
}












