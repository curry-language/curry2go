package System_DOT_IO_DOT_Unsafe

import "gocurry"
import "curry2go/Prelude"


func ExternalSystemDot_IODot_UnsafeDot_unsafePerformIO(task *gocurry.Task){
    root := task.GetControl()
    action := root.GetChild(0)
    
    if(!action.IsHnf()){
        task.ToHnf(action)
        return
    }
    
    gocurry.RedirectCreate(root, action.GetChild(0))
}

func ExternalSystemDot_IODot_UnsafeDot_spawnConstraint(task *gocurry.Task){
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

func ExternalSystemDot_IODot_UnsafeDot_primUs_isVar(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    if(x1.IsFree() && !task.IsBound(x1)){
        Prelude.Prelude__CREATE_True(root)
    } else{
        Prelude.Prelude__CREATE_False(root)
    }
}

func ExternalSystemDot_IODot_UnsafeDot_primUs_identicalVar(task *gocurry.Task){
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

func ExternalSystemDot_IODot_UnsafeDot_primUs_isGround(task *gocurry.Task){
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
        System_DOT_IO_DOT_Unsafe__CREATE_primUs_isGround(root, x1.GetChild(0))
        return
    }
    
    node := Prelude.Prelude__CREATE_And_And_(root, System_DOT_IO_DOT_Unsafe__CREATE_primUs_isGround(root.NewNode(), x1.GetChild(0)), root.NewNode())
    
    for i := 0; i < x1.GetNumChildren() - 1; i ++{
        Prelude.Prelude__CREATE_And_And_(node.GetChild(1), System_DOT_IO_DOT_Unsafe__CREATE_primUs_isGround(root.NewNode(), x1.GetChild(i)), root.NewNode())
        node = node.GetChild(1)
    }
    
    node.SetChild(1, System_DOT_IO_DOT_Unsafe__CREATE_primUs_isGround(root.NewNode(), x1.GetChild(x1.GetNumChildren() - 1)))
}

func ExternalSystemDot_IODot_UnsafeDot_compareAnyTerm(task *gocurry.Task){
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
            System_DOT_IO_DOT_Unsafe__CREATE_compareAnyTerm(root, x1.GetChild(i), x2.GetChild(i))
            ExternalSystemDot_IODot_UnsafeDot_compareAnyTerm(task)
            if(root.GetConstructor() != 1){
                return   
            }
        }
        Prelude.Prelude__CREATE_EQ(root)
    }
}

func ExternalSystemDot_IODot_UnsafeDot_primUs_showAnyTerm(task *gocurry.Task){
    panic("System.IO.Unsafe.showAnyTerm: Not yet implemented")
}

func ExternalSystemDot_IODot_UnsafeDot_primUs_readsAnyUnqualifiedTerm(task *gocurry.Task){
    panic("System.IO.Unsafe.readsAnyUnqualifiedTerm: Not yet implemented")
}

func ExternalSystemDot_IODot_UnsafeDot_showAnyExpression(task *gocurry.Task){
    panic("System.IO.Unsafe.showAnyExpression: Not yet implemented")
}












