package gocurry

import "fmt"
import "strconv"
import "bufio"
import "os"
import "strings"
import "sort"

// Text containing help information
const helpText = "General commands:\n" +
                 "  <return>:  evaluate a single program step\n" +
                 "  (a)bort:   abort the program\n" +
                 "  depth <n>: set printing depth to <n> (-1 = infinit)\n" +
                 "  (e)val:    evaluate the control of the current task to normal form\n" +
                 "  (f)ail:    fail the current task\n" +
                 "  (g)o <n>:  execute <n> steps (-1, omit <n> = run untill a result is found)\n" +
                 "  (h)elp:    display help text\n" +
                 "  (t)ask:    display information about the current task\n" +
                 "Fair search commands:\n" +
                 "  <cmd> <n>:    execute the general command <cmd> on all specified tasks\n" +
                 "  all <cmd>:    executes the general command <cmd> on every task\n" +
                 "  ensemble:     if true general commands are executed on every task\n" +
                 "  hide_results: hide already printed results in ensemble view\n" +
                 "  list:         list all task ids currently in use\n" +
                 "  pull:         check if new tasks are available\n" +
                 "  select <n>:   select task <n> (in single task view or execution)\n" +
                 "  view:         switch between single task and ensemble view\n"

// Type representing commands for tasks
type DebugCmd uint8
const(
    DebugStep = iota
)

// Type representing actions taken by tasks
type DebugAction uint8
const(
    DebugInit = iota
    DebugCall
    DebugSplit
    DebugSwitch
    DebugResult
)

// Type representing an event from a task
type DebugEvent struct{
    action DebugAction
    text string
}

// Type representing a task and its debugging variables
type DebugData struct{
    last_event DebugEvent
    task *Task
    event_chan chan DebugEvent
    cmd_chan chan DebugCmd
    done bool
}

// Prints information about a task
func printTask(task *Task){
    fmt.Printf("Task %d:\n", task.id)
    fmt.Printf("  Parents: %v\n", task.parents)
    fmt.Printf("  Fingerprint: %v\n", task.fingerprint)
    fmt.Printf("  Bindings:\n")
    for _, v := range(debug_varList){
        node, ok := v.GetTr(task.id, task.parents)
        if(ok){
            fmt.Printf("    %s -> %s\n", ShowResult(v), ShowResult(node))
        }
    }
}

func showDataEvent(data *DebugData) string{
    if(data.last_event.action == DebugResult){
        return "Result: " + ShowPartialNode(data.task.control, printing_depth)
    } else if(data.last_event.action == DebugCall){
        return "Call: " + ShowPartialNode(data.task.control, printing_depth)
    }
    
    return ""
}

// Returns a sorted list of the keys of the debugging map
func getIds() []int{
    // create a return slice
    ids := make([]int, len(debug_map))
    
    // loop over the map to get all the keys
    i := 0
    for n := range(debug_map){
        ids[i] = n
        i++
    }
    
    // sort the keys
    sort.Ints(ids)
    
    // return the sorted keys
    return ids
}

// Returns a list containing all values of the debugging map
func getDebugValues() []*DebugData{
    // create a return slice
    values := make([]*DebugData, len(debug_map))
    
    // loop over the map to get all the values
    i := 0
    ids := getIds()
    for _, id := range(ids){
        values[i] = debug_map[id]
        i++
    }
    
    // return the values
    return values
}

// Parses a string containing a task id
// and returns the curresponding task data and id
func parseTask(arg string) (*DebugData, int){
    // parse the number
    n, err := strconv.Atoi(arg)
                
    if(err != nil){
        fmt.Println("  " + err.Error())
        return nil, -1
    }
    
    // retrieve the task data from the debugging map
    d, ok := debug_map[n]
    if(!ok){
        fmt.Printf("  Task %d does not exist.\n", n)
        return nil, n
    }
    
    // return data and number
    return d, n
}

var apply_chan = make(chan DebugData, 0) // channel for tasks to register for debugging
var debug_varList = make([]*Node, 0) // list if nodes representing variables
var debug_map = make(map[int]*DebugData) // mapping of task ids to their debugging data
var printing_depth = -1

func debugLoop(result_chan chan *Node, fair_search bool){
    var ensemble = fair_search
    var first = true
    var view = fair_search
    var hide_results = false
    var update_data = true
    var cur_task = 0
    var run_steps = 0
    var data []*DebugData
    var reader = bufio.NewReader(os.Stdin)
    var input string
    
    // loop until done
    for{
        // pull new tasks from the apply channel
        ApplyLoop:
        for{
            select{
            case data := <- apply_chan:
                // set first flag
                first = false
                
                // pull first event and save data in the debugging map
                data.last_event = <- data.event_chan
                debug_map[data.task.id] = &data
            default:
                // wait until the first task was found
                if(first){
                    continue
                }
                
                // break if no tasks are waiting
                break ApplyLoop
            }
        }
        
        // fair search specific conditions
        if(fair_search){
            // check if all tasks are done
            done := true
            for _, d := range getDebugValues(){
                done = done && d.done
            }
            
            // jump to ResultLoop if search appears done
            if(done){
                goto ResultLoop
            }
        }
        
        // get input from user
        CmdLoop:
        for{
            if(update_data){
                // get the selected task data
                if(ensemble){
                    data = getDebugValues()
                } else{
                    data = []*DebugData{debug_map[cur_task]}
                }
            }    
            
            // display latest events
            if(view){
                // print last events of all tasks
                ids := getIds()
                for i := 0; i < len(ids); i++{
                    d := debug_map[ids[i]]
                    
                    // check if keep_results is active
                    if(hide_results){
                        // skip already displayed results
                        if(d.done){
                            continue
                        }
                    }
                    
                    // print last event
                    fmt.Printf("Task %d: " + showDataEvent(d) + "\n", d.task.id)
                    
                    // check if task is inactive
                    if(d.last_event.action == DebugResult){
                        d.done = true
                    }
                }
            } else{
                // print last event of current task
                fmt.Print(showDataEvent(debug_map[cur_task]) + " ")
                
                // check if task is inactive
                if(debug_map[cur_task].last_event.action == DebugResult){
                    debug_map[cur_task].done = true
                }
            }
            
            // check if go command is active
            if(run_steps != 0){
                // send step command to tasks in data
                done := true
                for _, d := range data{
                    if(d.last_event.action == DebugResult){
                        continue
                    }
                    
                    done = false
                    d.cmd_chan <- DebugStep
                }
                
                // if all tasks are done in fair search, stop early
                if(done && fair_search){
                    run_steps = 0
                    update_data = true
                } else{
                    fmt.Println("")
                    
                    // update step counter
                    if(run_steps > 0){
                        run_steps -= 1
                        
                        // test if go command is done
                        if(run_steps == 0){
                            update_data = true
                        }
                    }
                    
                    // skip rest of CmdLoop
                    break
                }
            }
            
            // print usage info
            fmt.Print("(RET|h)? ")
            
            // get input from user
            input, _ = reader.ReadString('\n')
            input_split := strings.Fields(input)
            
            // check for special inputs
            if(len(input_split) != 0){
                if(input_split[0] == "all"){
                    // check if fair search is active
                    if(!fair_search){
                        fmt.Println("  'all' modifier is only available during fair search.")
                        continue
                    }
                    
                    // parse all modifier
                    data = getDebugValues()
                    input_split = input_split[1:]
                } else{
                    // check if input is a number
                    _, err := strconv.Atoi(input_split[0])
                    
                    if(err == nil){
                        // check if fair search is active
                        if(!fair_search){
                            fmt.Println("  Executing specific tasks is only available during fair search.")
                            continue
                        }
                        
                        // execute step on specified tasks
                        data = data[:0]
                        for _, arg := range(input_split){
                            // get specified task
                            d, _ := parseTask(arg)
                            
                            if(d == nil){
                                continue CmdLoop
                            }
                            
                            // append task to data
                            data = append(data, d)
                        }
                        
                        input_split = input_split[:0]
                    }
                }
            }
            
            // check if input was return
            if(len(input_split) == 0){
                // send step command to selected tasks
                for _, d := range(data){
                    // skip already finished tasks
                    if(d.last_event.action == DebugResult){
                        continue
                    }
                    
                    // send step command
                    d.cmd_chan <- DebugStep
                }
                
                // break cmd loop
                break CmdLoop
            }
            
            // split input into command and arguments
            command := input_split[0]
            args := input_split[1:]
            
            switch command{
            case "a", "abort":
                return
            case "depth":
                // check if an argument was provided
                if(len(args) == 0){
                    fmt.Println("  'depth' expects a number as argument")
                }
            
                // parse argument
                n, err := strconv.Atoi(args[0])
                
                if(err != nil){
                    fmt.Println("  " + err.Error())
                    continue
                }
                
                printing_depth = n
            case "e", "eval":
                // check if task ids were provided
                if(len(args) != 0){
                    // check if fair search is active
                    if(!fair_search){
                        fmt.Println("  Evaluating specific tasks is only available during fair search.")
                        continue
                    }
                    
                    // set data to selected tasks
                    data = data[:0]
                    for _, arg := range(args){
                        // get specified task
                        d, _ := parseTask(arg)
                        
                        if(d == nil){
                            continue CmdLoop
                        }
                        
                        // append task to data
                        data = append(data, d)
                    }
                }
                
                // save old counter values
                old_chCount := <- choiceCount
                choiceCount <- old_chCount
                old_freeCount := <- freeCount
                freeCount <- old_freeCount
                old_taskCount := <- taskCount
                taskCount <- old_taskCount
                
                // evaluate every task
                for _, d := range(data){
                    // skip already evaluated tasks
                    if(d.last_event.action == DebugResult){
                        continue
                    }
                    
                    // copy control
                    copy_control := DeepCopy(d.task.GetControl())
                    
                    // copy task
                    copy_task := CreateTask(NfCreate(copy_control.NewNode(), copy_control), d.task.GetId())
                    copy_task.SetFingerprint(d.task.GetFingerprint())
                    copy_task.SetParents(d.task.GetParents())
                    
                    // evaluate task
                    queue := make(chan Task, 1000000)
                    toHnf(copy_task, queue, false)
                    
                    // print result
                    fmt.Printf("  Task %d: " + ShowResult(copy_task.control) + "\n", d.task.id)
                }
                
                // reset counters
                <- taskCount
                <- freeCount
                <- choiceCount
                taskCount <- old_taskCount
                freeCount <- old_freeCount
                choiceCount <- old_chCount
                
                continue
            case "f", "fail":
                // check if task ids were provided
                if(len(args) != 0){
                    // check if fair search is active
                    if(!fair_search){
                        fmt.Println("  Failing specific tasks is only available during fair search.")
                        continue
                    }
                    
                    // set data to selected tasks
                    data = data[:0]
                    for _, arg := range(args){
                        // get specified task
                        d, _ := parseTask(arg)
                        
                        if(d == nil){
                            continue CmdLoop
                        }
                        
                        // append task to data
                        data = append(data, d)
                    }
                }
                
                // fail the selected tasks
                for _, d := range(data){
                    // skip if task is done
                    if(d.last_event.action == DebugResult){
                        continue
                    }
                
                    // set task control to exempt
                    d.task.control = ExemptCreate(d.task.NewNode())
                    
                    // send step command to task
                    d.cmd_chan <- DebugStep
                }
                break CmdLoop
            case "g", "go":
                // check if arguments were provided
                if(len(args) == 0){
                    // run forever
                    run_steps = -1
                } else{
                    // parse argument
                    n, err := strconv.Atoi(args[0])
                    
                    if(err != nil){
                        fmt.Println("  " + err.Error())
                        continue
                    }
                    
                    // run n steps
                    run_steps = n
                }
                
                // check if task ids were provided
                if(len(args) > 1){
                    // check if fair search is active
                    if(!fair_search){
                        fmt.Println("  Running specific tasks is only available during fair search.")
                        run_steps = 0
                        continue
                    }
                    
                    // set data to selected tasks
                    data = data[:0]
                    args = args[1:]
                    for _, arg := range(args){
                        // get specified task
                        d, _ := parseTask(arg)
                        
                        if(d == nil){
                            run_steps = 0
                            continue CmdLoop
                        }
                        
                        // append task to data
                        data = append(data, d)
                    }
                    
                    // keep the same data over multiple steps
                    update_data = false
                }
            case "h", "help":
                // print help text
                fmt.Print(helpText)
            case "t", "task":
                // check if task ids were provided
                if(len(args) != 0){
                    // check if fair search is active
                    if(!fair_search){
                        fmt.Println("  Printing specific tasks is only available during fair search.")
                        continue
                    }
                    
                    // print all selected tasks
                    for _, arg := range(args){
                        // get specified task
                        d, _ := parseTask(arg)
                        
                        if(d == nil){
                            continue
                        }
                        
                        // print task info
                        printTask(d.task)
                    }
                    continue
                }
                
                // print info of selected tasks
                for _, d := range(data){
                    printTask(d.task)
                }
            case "ensemble":
                // check if fair search is active
                if(!fair_search){
                    fmt.Println("  Command only available during fair search.")
                    continue
                }
                
                // toggle 'ensemble' flag
                ensemble = !ensemble
            case "hide_results":
                if(!fair_search){
                    fmt.Println("  Command only available during fair search.")
                    continue
                }
                
                hide_results = !hide_results
            case "list":
                // check if fair search is active
                if(!fair_search){
                    fmt.Println("  Command only available during fair search.")
                    continue
                }
                
                // print all task ids currently in use
                for k := range debug_map{
                    fmt.Println(k)
                }
            case "pull":
                // check if fair search is active
                if(!fair_search){
                    fmt.Println("  Command only available during fair search.")
                    continue
                }
                // move to apply loop
                goto ApplyLoop
            case "select":
                // check if fair search is active
                if(!fair_search){
                    fmt.Println("  Command only available during fair search.")
                    continue
                }
                
                // check if id was provided
                if(len(args) == 0){
                    fmt.Println("  Select expects a task id as argument.")
                    continue
                }
                
                // parse argument
                _, n := parseTask(args[0])
                
                // switch current task
                cur_task = n
            case "view":
                // check if fair search is active
                if(!fair_search){
                    fmt.Println("  Command only available during fair search.")
                    continue
                }
                
                // toggle 'view' flag
                view = !view
            default:
                // print invalid command message
                fmt.Println("  Invalid Command: " + command)
            }
        }
        
        // pull result channel to check if the search has finished
        ResultLoop:
        for{
            select{
            case _, ok := <- result_chan:
                if(!ok){
                    return
                }
            default:
                break ResultLoop
            }
        }
        
        // pull events from tasks
        for _, d := range(data){
            if(d.last_event.action == DebugResult){
                if(!fair_search){
                    // wait for the search to finish or switch tasks
                    select{
                    case d.last_event = <- d.event_chan:
                    default:
                        goto ResultLoop
                    }
                    
                    break
                }
                
                // skip already finished task in fair search
                continue
            }
            
            // pull events from task
            for{
                d.last_event = <- d.event_chan
                
                // handle split action
                if(d.last_event.action == DebugSplit){
                    
                    // check for fair search
                    if(fair_search){
                        // print split event in single task view for the current task
                        if(!view && cur_task == d.task.parents[len(d.task.parents) - 1]){
                            fmt.Println(d.last_event.text)
                        }
                        
                        // remap tasks
                        debug_map[d.task.id] = d
                        delete(debug_map, d.task.parents[len(d.task.parents) - 1])
                        
                        // change current task
                        if(cur_task == d.task.parents[len(d.task.parents) - 1]){
                            cur_task = d.task.id
                        }
                    } else{
                        // print split event
                        fmt.Println(d.last_event.text)
                    }
                    
                    // pull next event
                    continue
                }
                
                // continue with next task
                break
            }
        }
    }
}

func toHnfDebug(task *Task, queue chan Task, bfs bool, cmd_chan chan DebugCmd, event_chan chan DebugEvent){
    
    // defer error handler
    defer errorHandlerDebug(task, queue, bfs, cmd_chan, event_chan)
    
    var skip = false
    
    for{
        // lock control node
        control_lock := &task.control.lock
        control_lock.Lock()
        
        // check if task result map exists
        if(task.control.tr != nil){
            // test task result map for already computed results
            node, ok := task.control.GetTr(task.id, task.parents)
            
            if(ok){
                control_lock.Unlock()
                
                // if a parent exists, update it with the computed result
                if(len(task.stack) > 0){
                    // get and lock parent
                    parent := task.stack[len(task.stack) - 1]
                    parent.lock.Lock()
                
                    // check if update has to be in task result map or in place
                    if(node.ot > parent.ot){
                        // create copy of parent with new owner task
                        new_node := LockedCopyNode(parent)
                        new_node.ot = node.ot
                        
                        // update the children to the computed result
                        if(new_node.IsFcall() && new_node.int_value >= 0){
                            new_node.Children[new_node.int_value] = node
                        } else{
                            for i := range(new_node.Children){
                                if(new_node.GetChild(i) == task.control){
                                    new_node.Children[i] = node
                                }
                            }
                        }
                        
                        // create task result map if necessary
                        if(parent.tr == nil){
                            parent.tr = make(map[int]*Node)
                        }
                        
                        // update task result map of parent
                        parent.tr[node.ot] = new_node
                    } else{
                        // update children of parent in place
                        if(parent.IsFcall() && parent.int_value >= 0){
                            parent.Children[parent.int_value] = node
                        } else{
                            for i := range(parent.Children){
                                if(parent.GetChild(i) == task.control){
                                    parent.Children[i] = node
                                }
                            }
                        }
                    }
                    
                    // unlock parent
                    parent.lock.Unlock()
                }
                
                // move to already computed result
                task.control = node
                continue
            }
        }
        
        // return if evaluation is done
        if(len(task.stack) == 0 && (task.control.IsHnf() || task.control.LockedIsPartial())){
            control_lock.Unlock()
            
            // log result
            event_chan <- DebugEvent{DebugResult, ""}
            
            return
        }
        
        control_lock.Unlock()
        
        switch task.control.node_type{
        case FCALL:
            // log call
            event_chan <- DebugEvent{DebugCall, ""}
        case CONSTRUCTOR, INT_LITERAL, FLOAT_LITERAL, CHAR_LITERAL, REDIRECT, CHOICE, EXEMPT:
            skip = true
        }
        
        if(!skip){
            <- cmd_chan
        } else{
            skip = false
        }
        
        control_lock.Lock()
        
        // evaluate node depending on the node type
        switch task.control.node_type {
        case FCALL:
            // test for partial function call
            if(task.control.LockedIsPartial()){

                // if the stack is not empty, move to parent
                task.PopStack()

                // unlock control node
                control_lock.Unlock()
                continue
            }

            // test if an argument is demanded
            if (task.control.int_value >= 0){

                // get demanded child
	            child := task.control.GetChild(task.control.int_value)
                
                // if the child needs to be evaluated, put it in control
                if (!child.IsHnf()){
                    task.MoveTo(task.control.int_value)
                    control_lock.Unlock()
                    continue
                }
            }

            // call the function
            task.control.function(task)
            
            // move to parent
            task.PopStack()

            // unlock control node
            control_lock.Unlock()
        case CHOICE:
            // unlock control node
            control_lock.Unlock()
            
            // test if stack is empty
            if (len(task.stack) == 0){

                // get fingerprint information
                branch, ok := task.fingerprint[task.control.int_value]

                // test if choice id in fingerprint
                if(ok){
                    // select the branch
                    task.control = task.control.Children[branch]
                }else{   
                    // get and increase task count
                    count := <- taskCount
                    taskCount <- count + 2
                    
                    old_id := task.id
                    
                    // update task
                    task.parents = append(task.parents, task.id)
                    task.id = count + 1
                
                    // create new task
                    var new_task Task
                    new_task.id = count + 2
                    new_task.control = task.control.Children[1]
                    new_task.parents = make([]int, len(task.parents))
                    copy(new_task.parents, task.parents)
                    new_task.fingerprint = make(map[int]int)
                    for k,v := range task.fingerprint {
                        new_task.fingerprint[k] = v
                    }

                    // set fingerprints
                    task.fingerprint[task.control.int_value] = 0
                    new_task.fingerprint[task.control.int_value] = 1
                    
                    // move to new control
                    task.control = task.control.Children[0]
                    
                    // change task on bfs
                    if(bfs){
                        queue <- *task
                        *task = <- queue
                    }
                    
                    event_chan <- DebugEvent{DebugSplit,
                        fmt.Sprintf("Splitting task %d into tasks %d & %d.\nContinuing with task %d.",
                            old_id, task.id, new_task.id, task.id)}

                    // append new task to queue
                    queue <- new_task
                }
            } else{
                // lock parent
                parent := task.stack[len(task.stack) - 1]
                lock := &parent.lock
                lock.Lock()
                
                // test fingerprint
                branch, ok := task.fingerprint[task.control.int_value]
                            
                // use memoization if possible
                if(ok){
                    
                    if(task.id > parent.ot){  
                        // create copy of parent
                        new_node := LockedCopyNode(parent)
                        new_node.ot = task.id
                        
                        // replace choice with branch
                        if(new_node.IsFcall() && new_node.int_value >= 0){
                            new_node.Children[new_node.int_value] = task.control.Children[branch]
                        } else{
                            for i := range(new_node.Children){
                                if(new_node.GetChild(i) == task.control){
                                    new_node.Children[i] = task.control.Children[branch]
                                }
                            }
                        }
                        
                        // create task result map for parent
                        if (parent.tr == nil){
                            parent.tr = make(map[int]*Node)
                        }
                        
                        // update task result map of parent
                        parent.tr[task.id] = new_node
                        task.PopStack()
                        
                        lock.Unlock()
                        continue
                    } else{
                        // replace choice with branch
                        if(parent.IsFcall() && parent.int_value >= 0){
                            parent.Children[parent.int_value] = task.control.Children[branch]
                        } else{
                            for i := range(parent.Children){
                                if(parent.GetChild(i) == task.control){
                                    parent.Children[i] = task.control.Children[branch]
                                }
                            }
                        }
                        
                        // change control to parent
                        task.PopStack()
                        lock.Unlock()
                        continue
                    }
                }
                
                // move to parent if pulltabbing-step has already been performed by another task
                if(parent.IsChoice()){
                    task.PopStack()
                    lock.Unlock()
                    continue
                }
                
                // prevent pulltab-step from exiting task result maps
                if(task.control.ot > parent.ot){
                    task.PopStack()
                    lock.Unlock()
                    continue
                }
                
                // test for IO functions (NondetErrors)
                if(parent.GetName() == "bindIO" || parent.GetName() == "catch"){
                    err_node := ConstCreate(task.control.NewNode(), 3, 1, &runtime_names[7], StringCreate(task.control.NewNode(), "Non-determinism in I/O actions occurred!"))
                    if(task.CatchError(err_node)){
                        lock.Unlock()
                        continue
                    }
                    lock.Unlock()
                    panic(err_node)
                }

                // perform a pulltabbing-step
                pullTab(task.control, task.stack[len(task.stack) - 1])

                // move to parent
                task.PopStack()
                lock.Unlock()
            }
        case EXEMPT:
            // unlock control node
            control_lock.Unlock()
            
            // try to throw fail error
            err_node := task.control.NewNode()
            if(task.CatchError(err_node)){
                ConstCreate(err_node, 2, 1, &runtime_names[6], StringCreate(task.control.NewNode(), "IO action failed"))
                continue
            }
            
            event_chan <- DebugEvent{DebugResult, ""}
            
            return
        case REDIRECT:
            // unlock control node
            control_lock.Unlock()
            
            task.control = task.control.EliminateRedirect()
        case CONSTRUCTOR, INT_LITERAL, FLOAT_LITERAL, CHAR_LITERAL:
            // unlock control node
            control_lock.Unlock()
            
            // pop stack
            task.PopStack()
        }
    }
}

// Handles panics thrown by prim_error
func errorHandlerDebug(task *Task, queue chan Task, bfs bool, cmd_chan chan DebugCmd, event_chan chan DebugEvent){
    // get error
    if err := recover(); err != nil{
    
        // parse error
        var err_node *Node
        var err_msg string
        
        switch err.(type){
        case string:
            err_msg = err.(string)
            err_node = ConstCreate(new(Node), 0, 1, &runtime_names[5], StringCreate(new(Node), err_msg))
        case *Node:
            err_node = err.(*Node)
            err_msg = ShowResult(err_node)
        case error:
            err_msg = err.(error).Error()
            err_node = ConstCreate(new(Node), 0, 1, &runtime_names[5], StringCreate(new(Node), err_msg))
        }
        
        // try to catch error and continue computation
        if(task.CatchError(err_node)){
            toHnfDebug(task, queue, bfs, cmd_chan, event_chan)
            return
        }
        
        // debug print
        if(error_depth == 0){
            // print control if nothing else is printed
            fmt.Println("Error evaluating: " + ShowResult(task.control))
        
            fmt.Println("Hint: set option 'errdepth' to get more contextual information.")
        } else {
            // print stack
            if(len(task.stack) > 0){
                fmt.Printf("Stack     : " + showNode(task.stack[0]))
                for i := 1; i < len(task.stack); i++{
                    fmt.Printf(" -> " + showNode(task.stack[i]))
                }
                fmt.Println("")
            }
            
            // print expression from control up
            if(error_depth >= len(task.stack) || error_depth < 0){
                fmt.Println("Expression: " + ShowResult(task.stack[0]))
            } else { // error_depth > 0
                fmt.Println("Expression: ..." + ShowResult(task.stack[len(task.stack) - error_depth]))
            }
        }
        
        // throw error again
        panic(err_msg)
    }
}