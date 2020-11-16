package gocurry

import "sync"
import "fmt"

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

// Struct representing a graphnode
type Node struct{
    Children []*Node 
    node_type NodeType
    int_value int          // int_literal, constructor, demanded_arg, int_value
    float_literal float64
    char_literal rune
    function func(*Task)
    number_args int
    name string
    evaluated bool
    lock sync.Mutex
    tr map[int]*Node
}

// Struct representing an icurry task
type Task struct{
    id int
    control *Node
    stack []*Node
    parents []int
    fingerprint map[int]int
    notHnf bool
}

// Variable to generate unique ids for choice-nodes
var nextId = 0

// Returns the next unique choice-identifier
func NextChoiceId() int {
    nextId += 1
    return nextId
}

// Channel saving number of created tasks
var taskCount chan int

// Returns the current number of created tasks
func GetTaskCount() int{
    count := <- taskCount
    taskCount <- count
    return count
}

// Increases the number of created tasks by 2
func UpTaskCount(){
    count := <- taskCount
    count += 2
    taskCount <- count
}

// channel to queue tasks
var queue = make(chan Task, 1000000)

// channel to pass results
var result_chan = make(chan *Node, 0)

// channel to signal ending of search
var done_chan = make(chan bool, 0)

// Handles panics thrown by prim_error
func errorHandler(task *Task){
    // get error
    if err := recover(); err != nil{
        // test for catch call in stack
        for i := len(task.stack) - 1; i >= 0; i--{
            // if there is a catch continue
            if(task.stack[i].IsFcall() && task.stack[i].name == "Prelude_catch"){
                // move back to catch call
                task.control = task.stack[i]
                task.stack = task.stack[:i]
                
                // set first argument of catch to error
                task.control.SetChild(0, Prelude_IOErrorCreate(new(Node), StringCreate(new(Node), err.(string))))
                return
            }
        }
        // throw error again
        panic(err)
    }
}

// Performs an evaluation step on the root
// of the task.
func evalStep(task *Task){

    // lock control node for switch
    control_lock := &task.control.lock
    control_lock.Lock()

    // evaluate control node of the task
    switch task.control.node_type{
    case FCALL:
        
        // test task result map for already computed results
        node, ok := task.control.GetTr(task.id, task.parents)
        
        if(ok){
            
            // go to already computed result
            task.control = node
            control_lock.Unlock()
            return
        } else{
            // create task result map for control
            if(task.control.tr == nil){
                task.control.tr = make(map[int]*Node)
            }
        }
    
        //test for partial function call
        if(task.control.LockedIsPartial()){

            // if the stack is not empty, move to parent
            if(len(task.stack) > 0){
                task.control = task.stack[len(task.stack) - 1]
                task.stack = task.stack[:len(task.stack) - 1]
            }

            // unlock control node
            control_lock.Unlock()
            return
        }

        // test if an argument is demanded
        if (task.control.int_value >= 0){

            // get demanded child
	        child := task.control.GetChild(task.control.int_value)
	        
	        // check task result map of child
	        node, ok := child.GetTr(task.id, task.parents)
	        
	        if(ok){
	        
	            // create copy of control with result in task result map
	            new_node := LockedCopyNode(task.control)
	            new_node.Children[task.control.int_value] = node.EliminateRedirect()
	            
	            // move to new node
	            task.control.tr[task.id] = new_node
	            task.control = new_node
	            
	            control_lock.Unlock()
	            return
	        }
            
            // if the child needs to be evaluated, put it in control
            if (!child.IsHNF()){
                task.stack = append(task.stack, task.control)
                task.control = child
                task.notHnf = true
                control_lock.Unlock()
                return
            }
        }

        // create copy of control in task result map
        new_node := LockedCopyNode(task.control)
        
        // move to new node
        task.control.tr[task.id] = new_node
        task.control = new_node

        // call the function
        defer errorHandler(task)
        task.control.function(task)

        // if the stack is not empty, move to parent
        if(len(task.stack) > 0){
            task.control = task.stack[len(task.stack) - 1]
            task.stack = task.stack[:len(task.stack) - 1]
        }

        // unlock control node
        control_lock.Unlock()
    case EXEMPT:
        // unlock control node
        control_lock.Unlock()
        return
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
                // get task count
                count := GetTaskCount()
            
                // create new task
                var new_task Task
                new_task.id = count + 2
                new_task.control = task.control.Children[1]
                new_task.parents = make([]int, len(task.parents))
                copy(new_task.parents, task.parents)
                new_task.parents = append(new_task.parents, task.id)
                new_task.fingerprint = make(map[int]int)
                for k,v := range task.fingerprint {
                    new_task.fingerprint[k] = v
                }

                // set fingerprints
                task.fingerprint[task.control.int_value] = 0
                new_task.fingerprint[task.control.int_value] = 1

                // set control in the old task to the first child
                task.parents = append(task.parents, task.id)
                task.control = task.control.Children[0]
                task.id = count + 1
                
                // increase task count
                UpTaskCount()

                // append new task to queue
                queue <- new_task
            }
        } else{
            // lock parent
            parent := task.stack[len(task.stack) - 1]
            lock := &parent.lock
            lock.Lock()

            // move to parent if pulltabbing-step has already been performed by another task
            if(parent.IsChoice()){
                task.control = parent
                task.stack = task.stack[:len(task.stack) - 1]
                lock.Unlock()
                return
            }
            
            // test fingerprint
            branch, ok := task.fingerprint[task.control.int_value]
                        
            // use memoization if possible
            if(ok && parent.IsFcall()){
                // create copy of parent
                new_node := LockedCopyNode(parent)
                
                // replace choice with branch
                if(new_node.IsFcall() && new_node.int_value >= 0){
                    new_node.Children[new_node.int_value] = task.control.Children[branch]
                } else{
                    for i := range(new_node.Children){
                        node, _ := new_node.GetChild(i).GetTr(task.id, task.parents)
                        
                        if(node.EliminateRedirect() == task.control){
                            new_node.Children[i] = task.control.Children[branch]
                        }
                    }
                }
                
                // create task result map for parent
                if (parent.tr == nil){
                    parent.tr = make(map[int]*Node)
                }
                
                // update task result map of parent and move to new node
                parent.tr[task.id] = new_node
                task.control = new_node
                task.stack = task.stack[:len(task.stack) - 1]
                
                lock.Unlock()
                return
            }

            // perform a pulltabbing-step
            pullTab(task.control, task.stack[len(task.stack) - 1], task.id, task.parents)

            // move to parent
            task.control = task.stack[len(task.stack) - 1]
            task.stack = task.stack[:len(task.stack) - 1]
            lock.Unlock()
            return
        }
    case REDIRECT:
        control_lock.Unlock()

        // follow redirect
        node := task.control.EliminateRedirect()
        
        // replace redirect in parent
        if(len(task.stack) > 0){
            parent := task.stack[len(task.stack) - 1]
            
            for i := range parent.Children{
                if(parent.Children[i] == task.control){
                    parent.Children[i] = node
                }
            }
        }
        
        // move to node
        task.control = node
    case CONSTRUCTOR:
        // unlock control node
        control_lock.Unlock()

        // test if a function was expected
        if(task.notHnf){
            // move to parent if possible
            if(len(task.stack) > 0){
                task.control = task.stack[len(task.stack) - 1]
                task.stack = task.stack[:len(task.stack) - 1]
                return
            }
            
            // otherwise, start evaluation to nf
            task.notHnf = false
        }

        // test if Children need to be evaluated
        for i := range task.control.Children{
            // get child
            child := task.control.GetChild(i)

            // set control to child if necessary
            if(!child.IsNF() && !child.IsPartial()){
                task.stack = append(task.stack, task.control)
                task.control = task.control.Children[i]
                return
            }
        }

        // evaluate control node
        task.control.evaluated = true

        // if the stack is not empty, move to parent
        if(len(task.stack) > 0){
            task.control = task.stack[len(task.stack) - 1]
            task.stack = task.stack[:len(task.stack) - 1]
        }
    case INT_LITERAL, FLOAT_LITERAL, CHAR_LITERAL:
        // unlock node
        control_lock.Unlock()

        // set node to evaluated
        task.control.evaluated = true
        
        // move to parent if possible
        if(len(task.stack) > 0){
            task.control = task.stack[len(task.stack) - 1]
            task.stack = task.stack[:len(task.stack) - 1]
        }
    }
}

// Evaluates a graph to normal form.
// root is the root node of the graph to be evaluated.
// If interactive is set, the user can decide to stop or continue
// the search after every result.
// search_strat is the search strategy to be used.
// max_results is the maximum number of results to be computed.
// max_tasks is the maximum number of threads that can be used in a concurrent execution.
func Evaluate(root *Node, interactive bool, search_strat SearchStrat, max_results int, max_tasks int){

    // initialize task count
    taskCount = make(chan int, 1)
    taskCount <- 0

    // create a task for the root node
    var first_task Task
    first_task.control = root
    first_task.fingerprint = make(map[int]int)    

    // write task to the queue
    queue <- first_task

    // call function implementing the search strategy
    switch search_strat{
    case DFS:
        go dfs()
    case BFS:
        go bfs()
    case FS:
        go fsTaskHandler(max_tasks)
    }

    // loop until done
    number_results := 0
    for result := range result_chan{
        // print result
        printResult(result)
        fmt.Println("")
        number_results += 1

        // interactive mode
        if(interactive){
            // print question
            fmt.Println("Find another result?(y|n)")
            
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

// Implementation of a depth-first search to evaluate a task.
func dfs(){
    // set first task as current task
    cur_task := <- queue

    // loop until done
    for{
        //printDebug(cur_task.control, cur_task.id, cur_task.parents)
        //fmt.Println()
        //printResult(cur_task.control)
        //fmt.Println("\n")

        // perform a step
        evalStep(&cur_task)

        // test the task state
        if(cur_task.control.evaluated){
            // return result if control is evaluated
            result_chan <- cur_task.control

            // select next task from queue
            select{
            case cur_task = <-queue:
            
            default:
                // if the queue is empty, end search
                close(result_chan)
                return
            }   
        } else if (cur_task.control.node_type == EXEMPT){
            // move to next task in queue on an exempt node
            select{
            case cur_task = <-queue:

            default:
                // if the queue is empty, end search
                close(result_chan)
                return
            }    
        }
    }
}

// Implementation of a breadth-first search to evaluate a task.
func bfs(){
    // loop until done
    for{
        // try to select a task from the queue
        select{
        case t := <-queue:

            // perform a step
            evalStep(&t)

            // test the task state
            if (t.control.evaluated && len(t.stack) == 0) {
                // return result if control is evaluated
                result_chan <- t.control

                continue
            } else if (t.control.node_type == EXEMPT){
                // continue without the task on an exempt node
                continue
            }
            // add task back to end of queue
            queue <- t
        default:
            // return if queue is empty
            close(result_chan)
            return
        }   
    }    
}

// Handles the evaluation of a task in a fair search.
// task is the task to start the evaluation on.
func fsRoutine(task Task){
    // loop until done
    for{
        // perform an evaluation step
        evalStep(&task)
        
        // lock control node
        task.control.lock.Lock()
        
        // test the task state
        if(task.control.evaluated && len(task.stack) == 0){
            // return result if control is evaluated
            result_chan <- task.control
            task.control.lock.Unlock()
            break
        } else if(task.control.node_type == EXEMPT){
            // finish if control is an exempt node
            task.control.lock.Unlock()
            break
        }
        
        // unlock control node
        task.control.lock.Unlock()
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
func fsTaskHandler(max_tasks int){
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
                go fsRoutine(t)
                continue
            }

            // start a new task
            used_tasks += 1
            go fsRoutine(t)
        case <- done_chan:
            // test if the queue is really empty
            select{
            case t := <- queue:
                // start a new task
                go fsRoutine(t)
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

// Performs a pulltabbing step.
// A copy of root is made for each choice of choice_node
// where every reference to choice_node is replaced
// with the corresponding alternative.
// root is set to a choice with
// the copies as alternatives.
func pullTab(choice_node, root *Node, id int, parents []int){

    // create a slice for the new children
    new_children := make([]*Node, 2)
    
    root, _ = root.GetTr(id, parents)

    // create two copies of root
    
    new_children[0] = LockedCopyNode(root)
    new_children[1] = LockedCopyNode(root)

    // replace references to choice_node
    if(root.IsFcall() && root.int_value >= 0){
        new_children[0].SetChild(root.int_value, choice_node.GetChild(0))
        new_children[1].SetChild(root.int_value, choice_node.GetChild(1))
    } else{
        for i := range root.Children{
            node,_ := root.GetChild(i).GetTr(id, parents)
            if(node.EliminateRedirect() == choice_node){
                new_children[0].SetChild(i, choice_node.GetChild(0))
                new_children[1].SetChild(i, choice_node.GetChild(1))
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

    new_node.number_args = node.number_args
    new_node.name = node.name
    new_node.evaluated = node.evaluated

    // create new choice-identifier for choice nodes
    if(node.node_type == CHOICE){
        new_node.int_value = NextChoiceId()
    }

    // append args to children
    new_node.Children = append(new_node.Children, args...)

    // return new node
    return new_node
}
