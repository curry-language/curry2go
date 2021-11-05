package gocurry

import "sync"
import "fmt"
import "strconv"
import "time"

////// Definitions of types

// Enumeration of node types
type NodeType uint8
const(
    FCALL NodeType = iota
    CONSTRUCTOR           
    CHOICE                
    INT_LITERAL           
    FLOAT_LITERAL         
    CHAR_LITERAL
    REDIRECT
    EXEMPT
)

// Enumeration of search strategies
type SearchStrat uint8
const(
    DFS SearchStrat = iota
    BFS
    FS
)

// Struct representing a graph node
type Node struct{
    Children []*Node       // successors of the node
    node_type NodeType     // kind of node
    int_value int          // int_literal, constructor, demanded_arg, int_value
    float_literal float64
    char_literal rune
    function func(*Task)   // code of function nodes
    arity int              // number of arguments expected by function/constr.
    name *string           // reference to name of function/constructor
    lock sync.Mutex        // lock for synchronisation in fair search
    tr map[int]*Node       // task result map
    ot int                 // identifier of owner task
}

// Struct representing an icurry task
type Task struct{
    id int
    control *Node
    stack []*Node
    parents []int
    fingerprint map[int]int
}

////// Declarations for choice-id generation

// Variable to generate unique ids for choice-nodes
var choiceCount chan int

// Returns the next unique choice-identifier
func NextChoiceId() int {
    count := <- choiceCount
    choiceCount <- count + 1
    return count
}

////// Declarations for task-id generation

// Channel saving number of created tasks
var taskCount chan int

// Declarations for free-name generation
var freeCount chan int

func nextFree() *string{
    i := <- freeCount
    i += 1
    freeCount <- i
    var name = "Free" + strconv.Itoa(i)
    return &name
}

////// Global variables

// array of names used to create nodes
var runtime_names []string = []string{"IO", "toNf", "ArgsToNf", "[]", ":", "IOError", "FailError", "NondetError"}

// number of stack nodes used to build expressions in error messages
var error_depth int

// time since system start
var startTime time.Time

func Uptime() time.Duration{
    return time.Since(startTime)
}

// setup
func init(){
    startTime = time.Now()
}

////// Task evaluation functions

// Evaluates a task to head normal form.
// Returns when the control node is an
// exempt node or in hnf or an
// error is caught by catch.
// task is the task to evaluate.
func toHnf(task *Task, queue chan Task, bfs bool){
    
    // defer error handler
    defer errorHandler(task, queue, bfs)
    
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
            return
        }
        
        //PrintResult(task.control)
        //fmt.Println("")
        //printDebug(task.control, task)
        //fmt.Println("\n")
        
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

// Evaluates its child to normalform
func toNf(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    // check task result map
    if(x1.tr != nil){
        node, ok := x1.GetTrLock(task.id, task.parents)
        
        if(ok){
            if(node.ot > root.ot){
                // create copy of root
                new_node := LockedCopyNode(root)
                new_node.ot = node.ot
                new_node.Children[0] = node
                
                // create task result map for root
                if(root.tr == nil){
                    root.tr = make(map[int]*Node)
                }
                
                // update task result map of root
                root.tr[node.ot] = new_node
                return
            }else{
                // update root in place
                root.Children[0] = node
            }
            return
        }
    }
    
    // if x1 is free redirect to it
    if(x1.IsFree()){
        RedirectCreate(root, x1)
        return
    }
    
    // return if argument is in normalform
    if (len(x1.Children) == 0 || x1.IsPartial()){
        RedirectCreate(root, x1)
        return
    }
    
    // Evaluate arguments to normalform with nfArgs
    root.function = nfArgs
    root.int_value = 0
    root.name = &runtime_names[2]
    root.Children = root.Children[:0]
    
    // wrap children with toNf
    for i := range(x1.Children){
        root.Children = append(root.Children, NfCreate(root.NewNode(), x1.Children[i]))
    }
    
    // save current constructor as function argument
    root.Children = append(root.Children, x1)
    root.arity = len(root.Children)
}

// Helper function to evaluate all
// children of a node to normalform.
func nfArgs(task *Task){
    root := task.GetControl()
    
    // if every argument has been evaluated to normalform return original constructor
    if (root.int_value == len(root.Children)-2 || root.int_value < 0){
        for i := 0; i < len(root.Children) - 1; i++{
            if(root.Children[i].IsFree() && task.IsBound(root.Children[i])){
                root.int_value = -1
                task.ToHnf(root.Children[i])
                return
            }
        }
    
        x1 := root.Children[len(root.Children) - 1]
        root.node_type = x1.node_type
        root.int_value = x1.int_value
        root.name = x1.name
        root.ot = x1.ot
        root.Children = root.Children[:len(root.Children) - 1]
        root.arity -= 1
        return
    }
    
    // evaluate next argument to normalform
    root.int_value += 1
}

////// Declarations for search strategies

// Evaluates a graph.
// root is the root node of the graph to be evaluated.
// If interactive is set, the user can decide to stop or continue
// the search after every result.
// search_strat is the search strategy to be used.
// max_results is the maximum number of results to be computed.
// max_tasks is the maximum number of goroutines that can be used in a concurrent execution.
// err_depth is the maximum number of nodes to move up the stack when prining expressions
// in error messages.
func Evaluate(root *Node, interactive, onlyHnf bool , search_strat SearchStrat, max_results, max_tasks, err_depth int){

    error_depth = err_depth

    // initialize channels
    choiceCount = make(chan int, 1)
    choiceCount <- 0
    
    taskCount = make(chan int, 1)
    taskCount <- 0
    
    freeCount = make(chan int, 1)
    freeCount <- 0
    
    queue := make(chan Task, 1000000) // queue for tasks
    result_chan := make(chan *Node, 0) // channel receiving results of search
    done_chan := make(chan bool, 0) // channel signaling end of search

    // create a task for the root node
    var first_task Task
    if(onlyHnf){
        first_task.control = root
    } else{
        first_task.control = NfCreate(new(Node), root)
    }
    first_task.fingerprint = make(map[int]int)    

    // write task to the queue
    queue <- first_task

    // call function implementing the search strategy
    switch search_strat{
    case DFS:
        go singleRoutineSearch(queue, result_chan, false)
    case BFS:
        go singleRoutineSearch(queue, result_chan, true)
    case FS:
        go fsTaskHandler(queue, result_chan, done_chan, max_tasks)
    }

    // loop until done
    number_results := 0
    for result := range result_chan{
        // print result
        PrintResult(result)
        number_results += 1

        // interactive mode
        if(interactive){
            // print question
            fmt.Print("More values? (y|n) ")
            
            // get answer
            for{
                var answer string
                fmt.Scanf("%s", &answer)

                if(answer[0] == 'y'){
                    // continue with search
                    break
                } else if (answer[0] == 'n') {
                    // stop search
                    return
                } else{
                    // print error message
                    fmt.Printf("Invalid anwer: %s\n", answer)
                    fmt.Println("Please answer with 'y' or 'n' (yes|no).")
                }
            }
        }
        
        // finish if the maximum number of results has been computed
        if(max_results != 0 && number_results >= max_results){
            return
        }
    }
}

// Evaluation loop for a search strategy executing
// in a single routine.
func singleRoutineSearch(queue chan Task, result_chan chan *Node, bfs bool){
    // set first task as current task
    task := <- queue

    // loop until done
    for{
        // perform evaluation to hnf
        toHnf(&task, queue, bfs)
        
        // write result if one was computed
        if (!task.control.IsExempt()){
            result_chan <- task.control
        }
        
        // continue with next task or finish
        select{
        case task = <- queue:
        
        default:
            close(result_chan)
            return
        }
    }
}


// Handles the evaluation of a task in a fair search.
// task is the task to evaluate.
func fsRunner(task Task, queue chan Task, result_chan chan *Node, done_chan chan bool){
    // perform evaluation to hnf
    toHnf(&task, queue, false)
        
    // write result if one was computed
    if(!task.control.IsExempt()){
        result_chan <- task.control   
    }
    
    // finish
    done_chan <- false
}

// Manages the fair search.
// Keeps track of the number of tasks running.
// Reads tasks from the queue and starts them
// when the max number of tasks has not been
// reached yet.
// max_tasks is the maximum number of task that
// can be evaluated concurrently.
func fsTaskHandler(queue chan Task, result_chan chan *Node, done_chan chan bool, max_tasks int){
    // number of tasks currently in use
    used_tasks := 0

    // loop until done
    for{
        // read a task from the queue or a signal from done_chan
        select{
        case t := <- queue:
            // if the max number of tasks is running, wait till one finishes
            if(max_tasks != 0 && used_tasks == max_tasks){
                <- done_chan
                go fsRunner(t, queue, result_chan, done_chan)
                continue
            }

            // start a new task
            used_tasks += 1
            go fsRunner(t, queue, result_chan, done_chan)
        case <- done_chan:
            // test if the queue is really empty
            select{
            case t := <- queue:
                // start a new task
                go fsRunner(t, queue, result_chan, done_chan)
                continue
            default:
                // delete task
                used_tasks -= 1

                // end search if all tasks are done
                if(used_tasks == 0){
                    close(result_chan)
                    return
                }
            }
        }
    }
}

////// Declarations of other helpful functions

// Handles panics thrown by prim_error
func errorHandler(task *Task, queue chan Task, bfs bool){
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
            toHnf(task, queue, bfs)
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

// Performs a pulltabbing step.
// A copy of root is made for each choice of choice_node
// where every reference to choice_node is replaced
// with the corresponding alternative.
// root is set to a choice with
// the copies as alternatives.
func pullTab(choice_node, root *Node){

    // create a slice for the new children
    new_children := make([]*Node, 2)

    // create two copies of root
    new_children[0] = LockedCopyNode(root)
    new_children[1] = LockedCopyNode(root)
    
    child1 := choice_node.Children[0]
    child2 := choice_node.Children[1]

    // replace references to choice_node
    if(root.IsFcall() && root.int_value >= 0){
        new_children[0].SetChild(root.int_value, child1)
        new_children[1].SetChild(root.int_value, child2)
    } else{
        for i := range root.Children{
            if(root.GetChild(i) == choice_node){
                new_children[0].SetChild(i, child1)
                new_children[1].SetChild(i, child2)
            }
        }
    }

    // set root to a choice node
    root.node_type = CHOICE
    root.int_value = choice_node.int_value
    root.Children = new_children
}

// Creates a copy of a node.
// node will be locked.
// Adds args to the children of the node.
// Choice nodes will get a new choice-id.
// Returns a pointer to the copy.
func CopyNode(node *Node, args ...*Node) *Node{

    node.lock.Lock()

    new_node := LockedCopyNode(node, args...)

    node.lock.Unlock()

    // return new node
    return new_node
}

// Creates a copy of a node.
// Adds args to the Children of the node.
// Choice nodes will get a new choice-id.
// Returns a pointer to the copy.
func LockedCopyNode(node *Node, args ...*Node) *Node{

    // create new node
    var new_node *Node = new(Node)

    // copy contents
    new_node.node_type = node.node_type

    new_node.Children = make([]*Node, len(node.Children))
    copy(new_node.Children, node.Children)

    new_node.int_value = node.int_value
    new_node.float_literal = node.float_literal
    new_node.char_literal = node.char_literal
    new_node.function = node.function

    new_node.arity = node.arity
    new_node.name = node.name
    new_node.ot = node.ot

    // create new choice-identifier for choice nodes
    if(node.node_type == CHOICE){
        new_node.int_value = NextChoiceId()
    }

    // append args to children
    new_node.Children = append(new_node.Children, args...)

    // return new node
    return new_node
}
