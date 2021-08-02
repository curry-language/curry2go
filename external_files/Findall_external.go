package ControlFindall

import "gocurry"
import "curry2go/Prelude"

var names = []string{"ExternalControl_Findall_oneValue", "evalSomeVal", "evalIsFail", "evalToList"}

func ExternalControl_Findall_allValues(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    // create a copy of the argument and task
    copy_x1 := gocurry.DeepCopy(x1)
    new_task := gocurry.CreateTask(gocurry.NfCreate(root.NewNode(), copy_x1), task.GetId())
    new_task.SetFingerprint(task.GetFingerprint())
    new_task.SetParents(task.GetParents())
    
    // start evaluation of the new task
    queue := make(chan gocurry.Task, 1)
    result_chan := make(chan *gocurry.Node, 0)
    gocurry.EvaluateTask(new_task, queue, result_chan)
    
    // return function evaluating results
    root.SetTr(task.GetId(), evalToList(task.NewNode(), result_chan))
}

// Helper function for allValues.
// Constructs a list with the results demand-driven.
func evalToList(root *gocurry.Node, result_chan chan *gocurry.Node)(*gocurry.Node){
    resultFunc := func (task *gocurry.Task){
        root := task.GetControl()
        node, ok := <- result_chan
        
        if ok{
            Prelude.Prelude__CREATE_Col(root, node, evalToList(root.NewNode(), result_chan))
        } else{
            Prelude.Prelude__CREATE_LSbRSb(root)
        }
    }
    
    return gocurry.FuncCreate(root, resultFunc, &names[3], 0, -1)
}

func ExternalControl_Findall_someValue(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    // call oneValue on argument and parse result
    oneVal := gocurry.FuncCreate(root.NewNode(), ExternalControl_Findall_oneValue, &names[0], 1, -1, x1)
    gocurry.FuncCreate(root, evalSomeVal, &names[1], 1, 0, oneVal)
}

// Helper function for someValue.
// Parses result of oneValue.
func evalSomeVal(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    if(x1.GetConstructor() == 0){
        gocurry.ExemptCreate(root)
    }else{
        gocurry.RedirectCreate(root, x1.GetChild(0))
    }
}

func ExternalControl_Findall_oneValue(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    // create a copy of the argument and the task
    copy_x1 := gocurry.DeepCopy(x1)
    new_task := gocurry.CreateTask(gocurry.NfCreate(root.NewNode(), copy_x1), task.GetId())
    new_task.SetFingerprint(task.GetFingerprint())
    new_task.SetParents(task.GetParents())
    
    // start evaluation the new task
    queue := make(chan gocurry.Task, 1)
    result_chan := make(chan *gocurry.Node, 0)
    gocurry.EvaluateTask(new_task, queue, result_chan)
    
    // test for and return result
    node, ok := <- result_chan
    if ok{
        root.SetTr(task.GetId(), Prelude.Prelude__CREATE_Just(task.NewNode(), node))
    } else{
        root.SetTr(task.GetId(), Prelude.Prelude__CREATE_Nothing(task.NewNode()))
    }
}

func ExternalControl_Findall_isFail(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    // call oneValue on argument and parse result
    oneVal := gocurry.FuncCreate(root.NewNode(), ExternalControl_Findall_oneValue, &names[0], 1, -1, x1)
    gocurry.FuncCreate(root, evalIsFail, &names[2], 1, 0, oneVal)
}

// Helper function for isFail.
// Parses result of oneValue.
func evalIsFail(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    if(x1.GetConstructor() == 0){
        Prelude.Prelude__CREATE_True(root)
    } else{
        Prelude.Prelude__CREATE_False(root)
    }
}

func ExternalControl_Findall_rewriteAll(task *gocurry.Task){
    panic("Control.Findall.rewriteAll: not yet implemented")
}

func ExternalControl_Findall_rewriteSome(task *gocurry.Task){
    panic("Control.Findall.rewriteSome: not yet implemented")
}
