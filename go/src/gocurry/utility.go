package gocurry

import "fmt"
import "regexp"
import "runtime"
import "strings"
import "strconv"
import "time"

////// Functions to create specific types of nodes

// Sets root to a constructor node.
// arity is the number of arguments the constructor expects.
// constructor is the number given to the constructor by icurry.
// name is the constructor name as a string for printing.
// Every pointer in args is added to the constructor children.
// Returns a pointer to the updated root.
func ConstCreate(root *Node, constructor, arity int, name *string, args ...*Node)(*Node){
    root.int_value = constructor
    root.name = name
    root.Children = args
    root.arity = arity
    root.node_type = CONSTRUCTOR
    return root
}

// Sets root to a function node.
// function is the function to be called when the node is evaluated.
// name is the function name as a string for printing.
// arity are the total number of arguments the function expects.
// demanded_args is a integers representing the position of an argument which have to be evaluated.
// Every pointer in args is added to the function children.
// Returns a pointer to the updated root.
func FuncCreate(root *Node, function func(*Task), name *string, arity int, demanded_args int, args ...*Node)(*Node){
    root.function = function
    root.Children = args
    root.arity = arity
    root.name = name
    root.int_value = demanded_args
    root.node_type = FCALL
    return root
}

// Sets root to a redirection node, pointing to x1.
// Returns a pointer to the updated root.
func RedirectCreate(root, x1 *Node)(*Node){
    root.Children = root.Children[:0]
    root.Children = append(root.Children, x1)
    root.node_type = REDIRECT
    return root
}

// Sets root to a choice node with x1 and x2 as possible choices.
// Returns a pointer to the updated root.
func ChoiceCreate(root, x1, x2 *Node)(*Node){
    root.Children = root.Children[:0]
    root.int_value = NextChoiceId()
    root.Children = append(root.Children, x1, x2)
    root.node_type = CHOICE
    return root
}

// Sets root to an exempt node.
// Returns a pointer to the updated root.
func ExemptCreate(root *Node)(*Node){
    root.Children = root.Children[:0]
    root.node_type = EXEMPT
    return root
}

// Sets root to an integer literal node with value value.
// Returns a pointer to the updated root.
func IntLitCreate(root *Node, value int)(*Node){
    root.Children = root.Children[:0]
    root.int_value = value
    root.node_type = INT_LITERAL
    return root
}

// Sets root to an float literal node with value value.
// Returns a pointer to the updated root.
func FloatLitCreate(root *Node, value float64)(*Node){
    root.Children = root.Children[:0]
    root.float_literal = value
    root.node_type = FLOAT_LITERAL
    return root
}

// Sets root to an char literal node with value value.
// Returns a pointer to the updated root.
func CharLitCreate(root *Node, value rune)(*Node){
    root.Children = root.Children[:0]
    root.char_literal = value
    root.node_type = CHAR_LITERAL
    return root
}

// Sets root to a node representing a free variable.
// Returns a pointer to the updated root.
func FreeCreate(root *Node)(*Node){
    root.Children = root.Children[:0]
    root.int_value = -1
    root.node_type = CONSTRUCTOR
    
    // add initial variables
    if(len(initialVars) < cap(initialVars)){
        root.name = &varNames[len(initialVars)]
        initialVars = append(initialVars, root)
    } else{
        root.name = nextFree()
    }
    
    // save variables for debug mode
    if(debug_mode){
        debug_varList = append(debug_varList, root)
    }
    
    return root
}

// Sets root to an IO node with a child.
// Returns a pointer to the updated root.
func IOCreate(root *Node, child *Node)(*Node){
    ConstCreate(root, 0, 1, &runtime_names[0], child)
    return root
}

// Sets root to a function that evaluates
// the node arg to normalform.
// Returns a pointer to the updated root.
func NfCreate(root, arg *Node) *Node{
    return FuncCreate(root, toNf, &runtime_names[1], 1, 0, arg)  
}

// Converts a Go string to a Curry String.
// root is the node receiving the first character.
// str is the Go string.
// Returns a pointer to the updated root.
func StringCreate(root *Node, str string)(*Node){

    // next node to write to
    cur_node := root

    runes := []rune(str)

    // go through the string
    for i := 0; i < len(runes); i++{
        // create a : at cur_node
        ConstCreate(cur_node, 1, 2, &runtime_names[4], CharLitCreate(root.NewNode(), runes[i]), root.NewNode())

        // move to next node
        cur_node = cur_node.GetChild(1)
    }
    
    // set last element to []
    ConstCreate(cur_node, 0, 0, &runtime_names[3])
    return root
}

// Converts a Curry String to a Go byte list.
// root is the entry to the representation of Curry's character list.
func ReadString(root *Node)(string){

    var builder strings.Builder

    // next character to read
    cur_node := root    

    // go through the string
    for{
        // finish on []
        if(cur_node.GetConstructor() == 0){
            break
        }

        // append next character to buffer
        builder.WriteRune(cur_node.GetChild(0).GetChar())

        // move to the next character
        cur_node = cur_node.GetChild(1)
    }

    return builder.String()
}

// Reads a Curry list and turns it
// into a go slice.
// root is the entry to the Curry list.
func ReadList(root *Node)(result []*Node){
    
    cur_node := root
    
    for{
        // finish on []
        if(cur_node.GetConstructor() == 0){
            return
        }

        // append next element to result
        result = append(result, cur_node.GetChild(0))

        // move to the next element
        cur_node = cur_node.GetChild(1)
    }
}

// Sets root to an IntLiteral with the
// Go major version number.
// Or -1 if the version is not
// of form goMaj.Min.
func GoMajVer(root *Node){
    version := runtime.Version()
    
    match,_ := regexp.MatchString("go(\\d+).(\\d+)", version)
    
    if(match){
        majMin := strings.Split(version, ".")
        maj,_ := strconv.Atoi(majMin[0][2:])
        IntLitCreate(root, maj)
    } else{
        IntLitCreate(root, -1)
    }
}

// Sets root to an IntLiteral with the
// Go minor version number.
// Or -1 if the version is not
// of form goMaj.Min.
func GoMinVer(root *Node){
    version := runtime.Version()
    
    match,_ := regexp.MatchString("go(\\d+).(\\d+)", version)
    
    if(match){
        majMin := strings.Split(version, ".")
        min,_ := strconv.Atoi(majMin[1])
        IntLitCreate(root, min)
    } else{
        IntLitCreate(root, -1)
    }
}


// Returns a copy of node.
// Recursively copies all children.
func DeepCopy(node *Node) *Node{
    node.lock.Lock()
    new_node := node.NewNode()
    
    // copy children
    for i := range(node.Children){
        new_child := DeepCopy(node.Children[i])
        new_node.Children = append(new_node.Children, new_child)
    }
    
    // copy node values
    new_node.node_type = node.node_type
    new_node.int_value = node.int_value
    new_node.float_literal = node.float_literal
    new_node.char_literal = node.char_literal
    new_node.function = node.function
    new_node.arity = node.arity
    new_node.name = node.name
    new_node.ot = node.ot
    if(node.tr != nil){
        new_node.tr = make(map[int]*Node)
        for k,v := range(node.tr){
            new_node.tr[k] = v
        }
    }
    node.lock.Unlock()
    return new_node
}

////// Methods on nodes

// Returns the first Node that is not
// a redirection node.
func (node *Node) EliminateRedirect() *Node{
    for{
        if(node.IsRedirect()){
            node = node.Children[0]
        } else{
            break
        }
    }
    return node
}

func (node *Node) GetChar() rune{
    return node.char_literal
}

func (node *Node) GetChild(index int) *Node{
    // eliminate redirection nodes
    for{
        if(node.Children[index].IsRedirect()){
            node.Children[index] = node.Children[index].Children[0]
        } else{
            break
        }
    }

    // return child
    return node.Children[index]
}

func (node *Node) GetChoiceId() int{
    return node.int_value
}

func (node *Node) GetConstructor() int{
    return node.int_value
}

func (node *Node) GetDemandedArgs() int{
    return node.int_value
}

func (node *Node) GetFloat() float64{
    return node.float_literal
}

func (node *Node) GetFunction() func(*Task){
    return node.function
}

func (node *Node) GetInt() int{
    return node.int_value
}

func (node *Node) GetName() string{
    return *node.name
}

func (node *Node) GetArity() int{
    return node.arity
}

func (node *Node) GetNumChildren() int{
    return len(node.Children)
}

func (node *Node) GetOt() int{
    return node.ot
}

// Searches the task result map of node for entries
// with the key id or any key from parents.
// If a matching entry is found it and true are returned.
// Otherwise node and false are returned.
func (node *Node) GetTr(id int, parents []int) (*Node, bool){
    // read node from task result map
    node2, ok := node.tr[id]
    
    if(ok){
        return node2, true
    }
    
    // read entries for parents
    for i := range parents{
        node2, ok = node.tr[parents[i]]
        
        if(ok){
            return node2, true
        }
    }
    
    return node, false
}

func (node *Node) GetTrLock(id int, parents []int) (*Node, bool){
    node.lock.Lock()
    res, ok := node.GetTr(id, parents)
    node.lock.Unlock()
    return res, ok
}

func (node *Node) GetType() NodeType{
    return node.node_type
}

func (node *Node) IsCharLit() bool{
    return (node.node_type == CHAR_LITERAL)
}

func (node *Node) IsChoice() bool{
    return (node.node_type == CHOICE)
}

func (node *Node) IsConst() bool{
    return (node.node_type == CONSTRUCTOR)
}

func (node *Node) IsError() bool{
    return (node.GetName() == "IOError" || node.GetName() == "UserError" || node.GetName() == "FailError" || node.GetName() == "NondetError")
}

func (node *Node) IsExempt() bool{
    return (node.node_type == EXEMPT)
}

func (node *Node) IsFcall() bool{
    return (node.node_type == FCALL)
}

func (node *Node) IsFree() bool{
    return (node.node_type == CONSTRUCTOR && node.int_value == -1)
}

func (node *Node) IsFloatLit() bool{
    return (node.node_type == FLOAT_LITERAL)
}

func (node *Node) LockedIsHnf() bool {
    if (node.IsConst() || node.IsIntLit() || node.IsFloatLit() || node.IsCharLit()){
        return true
    } else if(node.IsFcall()){
        return node.LockedIsPartial()
    }else{
        return false
    }
}

func (node *Node) IsHnf() bool {
    if (node.IsConst() || node.IsIntLit() || node.IsFloatLit() || node.IsCharLit()){
        return true
    } else if(node.IsFcall()){
        return node.IsPartial()
    }else{
        return false
    }
}

func (node *Node) IsIntLit() bool{
    return (node.node_type == INT_LITERAL)
}

func (node *Node) IsPartial() bool{
    node.lock.Lock()
    if(node.node_type == FCALL || node.node_type == CONSTRUCTOR){
        result := len(node.Children) < node.arity
        node.lock.Unlock()
        return result
    }
    node.lock.Unlock()
    return false
}

func (node *Node) IsRedirect() bool{
    return (node.node_type == REDIRECT)
}

func (node *Node) LockedIsPartial() bool{
    if(node.node_type == FCALL || node.node_type == CONSTRUCTOR){
        return len(node.Children) < node.arity
    }
    return false
}

// Returns a new Node with the same owner task as node.
func (node *Node) NewNode() *Node{
    new_node := new(Node)
    new_node.ot = node.ot
    return new_node
}

func (node *Node) SetChild(index int, child *Node){
    node.Children[index] = child
}

func (node *Node) SetIntVal(value int){
    node.int_value = value
}

func (node *Node) SetArity(value int){
    node.arity = value
}

func (node *Node) SetOt(ot int){
    node.ot = ot
}

func (node *Node) SetTr(id int, val *Node){
    if(node.tr == nil){
        node.tr = make(map[int]*Node)   
    }

    node.tr[id] = val
}

func (node *Node) SetTrLock(id int, val *Node){
    node.lock.Lock()
    node.SetTr(id, val)
    node.lock.Unlock()
}

////// Functions for tasks

// Returns a new task with id as task id
// and control as control node.
// Initializes the fingerprint.
func CreateTask(control *Node, id int) *Task{
    task := new(Task)
    task.id = id
    task.control = control
    task.fingerprint = make(map[int]int)
    return task
}

// Evaluates the task using fair search.
// Results are written to result_chan.
// When the search finishes result_chan will be closed.
func EvaluateTask(task *Task, queue, result_chan chan Task){
    queue <- *task
    done_chan := make(chan bool, 0)
    go fsTaskHandler(queue, result_chan, done_chan, 0)
}

////// Methods on tasks


func (task *Task) CatchError(err *Node) bool{
    // search for catch function on stack
    for i := len(task.stack) - 1; i >= 0; i--{
        if(task.stack[i].IsFcall()){
            if(task.stack[i].GetName() == "catch"){
                // set control to catch with the error
                task.control = task.stack[i]
                task.stack = task.stack[:i]
                
                task.control.SetChild(0, err)
                return true
            }   
        }
    }
    
    return false
}

func (task *Task) GetControl() *Node{
    return task.control
}

func (task *Task) GetId() int{
    return task.id
}

func (task *Task) GetFingerprint() map[int]int{
    return task.fingerprint
}

func (task *Task) GetParents() []int{
    return task.parents
}

// Get task specific results from choice
// and free nodes.
func (task *Task) FollowTask(node *Node) *Node{
    for{
        if(node.IsChoice()){
            branch, ok := task.fingerprint[node.GetChoiceId()]
            
            if(!ok){
                return node
            }
            
            node = node.Children[branch]
        } else if(node.IsRedirect()){
            node = node.Children[0]
        } else if(node.IsFree()){
            node2, ok := node.GetTr(task.id, task.parents)
            
            if(!ok){
                return node
            }
            
            node = node2
        } else{
            return node
        }
    }
}


// Tests if variable is bound in task.
func (task *Task) IsBound(variable *Node) bool{
    _, ok := variable.GetTrLock(task.id, task.parents)
    return ok
}

// Puts the current control node onto the stack
// and sets the control node to the specified child.
func (task *Task) MoveTo(index int){
    task.stack = append(task.stack, task.control)
    task.control = task.control.Children[index]
}

// Returns a new Node with task as owner task.
func (task *Task) NewNode() *Node{
    node := new(Node)
    node.ot = task.id
    return node
}

// Do not share a child of the control node.
func (task *Task) NoShare(index int){
    task.control.Children[index] = CopyNode(task.control.Children[index])
}

func (task *Task) SetFingerprint(finger map[int]int){
    for k,v := range(finger){
        task.fingerprint[k] = v
    }
}

// Sets the parents of task to
// a copy of the slice parents.
func (task *Task) SetParents(parents []int){
    if(len(task.parents) < len(parents)){
        task.parents = make([]int, len(parents))
    } else{
        task.parents = task.parents[:len(parents)]
    }
    copy(task.parents, parents)
}

// Used in external functions to 
// move evaluation to Node node.
func (task *Task) ToHnf(node *Node){
    task.stack = append(task.stack, task.control)
    task.stack = append(task.stack, node)
}

// Takes the top entry from the stack
// and sets it as the task control node.
func (task *Task) PopStack(){
    if(len(task.stack) > 0){
        task.control = task.stack[len(task.stack) - 1]
        task.stack = task.stack[:len(task.stack) - 1]
    }
}

/////// Benchmark

// Calls Evaluate count times and
// prints the average execution time.
func Benchmark(node *Node, count int, onlyHnf bool, search_strat SearchStrat, max_results, max_tasks, err_depth int){

    var sum float64 = 0

    for i := 0; i < count; i++{

        root := CopyNode(node)

        start := time.Now()

        Evaluate(root, false, false, onlyHnf, search_strat, max_results, max_tasks, err_depth, false, nil)

        end := time.Now()
        dif := end.Sub(start).Seconds()
        sum += dif
    }

    avg := sum / float64(count)

    fmt.Printf("Average time: %fs\n", avg)
}
