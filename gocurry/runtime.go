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
    int_literal int
    float_literal float64
    char_literal rune
    constructor int
    function func(*Task)
    demanded_args []int
    number_args int
    choice_id int
    name string
    evaluated bool
    lock sync.Mutex
}

// Struct representing an icurry task
type Task struct{
    control *Node
    stack []*Node
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

        // test if any demanded argument needs to be evaluated
        for i := range task.control.demanded_args {

            // get demanded child
            child_id := task.control.demanded_args[i]
	        child := task.control.GetChild(child_id)
            
            // if the child needs to be evaluated, put it in control
            if (!child.IsHNF()){
                task.stack = append(task.stack, task.control)
                task.control = child
                task.notHnf = true
                control_lock.Unlock()
                return
            }
        }

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
            branch, ok := task.fingerprint[task.control.choice_id]

            // test if choice id in fingerprint
            if(ok){
                // select the branch
                task.control = task.control.Children[branch]
            }else{   
                // create new task
                var new_task Task
                new_task.control = task.control.Children[1]
                new_task.fingerprint = make(map[int]int)
                for k,v := range task.fingerprint {
                    new_task.fingerprint[k] = v
                }

                // set fingerprints
                task.fingerprint[task.control.choice_id] = 0
                new_task.fingerprint[task.control.choice_id] = 1

                // set control in the old task to the first child
                task.control = task.control.Children[0]

                // append new task to queue
                queue <- new_task
            }
        } else{
            // lock parent
            lock := &task.stack[len(task.stack) - 1].lock
            lock.Lock()

            // move to parent if pulltabbing-step has already been performed by another task
            if(task.stack[len(task.stack) - 1].IsChoice()){
                task.control = task.stack[len(task.stack) - 1]
                task.stack = task.stack[:len(task.stack) - 1]
                lock.Unlock()
                return
            }

            // perform a pulltabbing-step
            pullTab(task.control, task.stack[len(task.stack) - 1])

            // move to parent
            task.control = task.stack[len(task.stack) - 1]
            task.stack = task.stack[:len(task.stack) - 1]
            lock.Unlock()
            return
        }
    case REDIRECT:
        // change control to child
        control_lock.Unlock()
        task.control = task.control.Children[0]
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
func pullTab(choice_node, root *Node){

    // create a slice for the new children
    new_children := make([]*Node, 2)

    // create two copies of root
    new_children[0] = LockedCopyNode(root)
    new_children[1] = LockedCopyNode(root)

    // replace references to choice_node
    for i := range root.Children{
        if(root.GetChild(i) == choice_node){
            new_children[0].SetChild(i, choice_node.GetChild(0))
            new_children[1].SetChild(i, choice_node.GetChild(1))
        }
    }

    // set root to a choice node
    root.node_type = CHOICE
    root.choice_id = choice_node.choice_id
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

    new_node.int_literal = node.int_literal
    new_node.float_literal = node.float_literal
    new_node.char_literal = node.char_literal
    new_node.function = node.function

    new_node.demanded_args = make([]int, len(node.demanded_args))
    copy(new_node.demanded_args, node.demanded_args)

    new_node.number_args = node.number_args
    new_node.constructor = node.constructor
    new_node.name = node.name
    new_node.evaluated = node.evaluated

    // create new choice-identifier for choice nodes
    if(node.node_type == CHOICE){
        new_node.choice_id = NextChoiceId()
    }

    // append args to children
    new_node.Children = append(new_node.Children, args...)

    // return new node
    return new_node
}
