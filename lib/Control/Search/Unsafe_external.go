package Control_DOT_AllValues

import "gocurry"
import "curry2go/Prelude"

var names = []string{"ExternalControl_AllValues_oneValue", "evalToList"}

func ExternalControlDot_SearchDot_UnsafeDot_allValues(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    // create a copy of the argument and task
    copy_x1 := gocurry.DeepCopy(x1)
    new_task := gocurry.CreateTask(gocurry.NfCreate(root.NewNode(), copy_x1), task.GetId())
    new_task.SetFingerprint(task.GetFingerprint())
    new_task.SetParents(task.GetParents())
    
    // start evaluation of the new task
    queue := make(chan gocurry.Task, 1)
    result_chan := make(chan gocurry.Task, 0)
    gocurry.EvaluateTask(new_task, queue, result_chan)
    
    // return function evaluating results
    root.SetTr(task.GetId(), evalToList(task.NewNode(), result_chan))
}

// Helper function for allValues.
// Constructs a list with the results demand-driven.
func evalToList(root *gocurry.Node, result_chan chan gocurry.Task)(*gocurry.Node){
    resultFunc := func (task *gocurry.Task){
        root := task.GetControl()
        result, ok := <- result_chan
        
        if ok{
            Prelude.Prelude__CREATE_Col_(root, result.GetControl(), evalToList(root.NewNode(), result_chan))
        } else{
            Prelude.Prelude__CREATE_LSb_RSb_(root)
        }
    }
    
    return gocurry.FuncCreate(root, resultFunc, &names[3], 0, -1)
}

func ExternalControlDot_SearchDot_UnsafeDot_oneValue(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    // create a copy of the argument and the task
    copy_x1 := gocurry.DeepCopy(x1)
    new_task := gocurry.CreateTask(gocurry.NfCreate(root.NewNode(), copy_x1), task.GetId())
    new_task.SetFingerprint(task.GetFingerprint())
    new_task.SetParents(task.GetParents())
    
    // start evaluation the new task
    queue := make(chan gocurry.Task, 1)
    result_chan := make(chan gocurry.Task, 0)
    gocurry.EvaluateTask(new_task, queue, result_chan)
    
    // test for and return result
    result, ok := <- result_chan
    if ok{
        root.SetTr(task.GetId(), Prelude.Prelude__CREATE_Just(task.NewNode(), result.GetControl()))
    } else{
        root.SetTr(task.GetId(), Prelude.Prelude__CREATE_Nothing(task.NewNode()))
    }
}

func ExternalControlDot_SearchDot_UnsafeDot_rewriteAll(task *gocurry.Task){
    panic("Control.AllValues.rewriteAll: not yet implemented")
}

func ExternalControlDot_SearchDot_UnsafeDot_rewriteSome(task *gocurry.Task){
    panic("Control.AllValues.rewriteSome: not yet implemented")
}
