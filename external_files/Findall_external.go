package ControlFindall

import "gocurry"
import "../../Prelude"

func ExternalControl_Findall_allValues(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    copy_x1 := gocurry.DeepCopy(x1)
    new_task := gocurry.CreateTask(gocurry.NfCreate(root.NewNode(), copy_x1), task.GetId())
    queue := make(chan gocurry.Task, 1)
    result_chan := make(chan *gocurry.Node, 0)
    gocurry.EvaluateTask(new_task, queue, result_chan)
    
    evalToList(root, result_chan)
}

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
    
    name := "evalToList"
    return gocurry.FuncCreate(root, resultFunc, &name, 0, -1)
}

func ExternalControl_Findall_someValue(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    copy_x1 := gocurry.DeepCopy(x1)
    new_task := gocurry.CreateTask(gocurry.NfCreate(root.NewNode(), copy_x1), task.GetId())
    queue := make(chan gocurry.Task, 1)
    result_chan := make(chan *gocurry.Node, 0)
    gocurry.EvaluateTask(new_task, queue, result_chan)
    
    node, ok := <- result_chan
    if ok{
        gocurry.RedirectCreate(root, node)
    } else{
        gocurry.ExemptCreate(root)
    }
}

func ExternalControl_Findall_oneValue(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    copy_x1 := gocurry.DeepCopy(x1)
    new_task := gocurry.CreateTask(gocurry.NfCreate(root.NewNode(), copy_x1), task.GetId())
    queue := make(chan gocurry.Task, 1)
    result_chan := make(chan *gocurry.Node, 0)
    gocurry.EvaluateTask(new_task, queue, result_chan)
    
    node, ok := <- result_chan
    if ok{
        Prelude.Prelude__CREATE_Just(root, node)
    } else{
        Prelude.Prelude__CREATE_Nothing(root)
    }
}

func ExternalControl_Findall_isFail(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    copy_x1 := gocurry.DeepCopy(x1)
    new_task := gocurry.CreateTask(gocurry.NfCreate(root.NewNode(), copy_x1), task.GetId())
    queue := make(chan gocurry.Task, 1)
    result_chan := make(chan *gocurry.Node, 0)
    gocurry.EvaluateTask(new_task, queue, result_chan)
    
    _, ok := <- result_chan
    if ok{
        Prelude.Prelude__CREATE_False(root)
    } else{
        Prelude.Prelude__CREATE_True(root)
    }
}

func ExternalControl_Findall_rewriteAll(task *gocurry.Task){
    panic("Control.Findall.rewriteAll: not yet implemented")
}

func ExternalControl_Findall_rewriteSome(task *gocurry.Task){
    panic("Control.Findall.rewriteSome: not yet implemented")
}
