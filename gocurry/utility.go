package gocurry

import "fmt"
import "bytes"
import "time"
import "regexp"
import "runtime"
import "strings"
import "strconv"
import "go/parser"
import "go/token"
import "go/ast"
import "unicode"

////// Functions to create specific types of nodes

// Sets root to a constructor node.
// num_args is the number of arguments the constructor expects.
// constructor is the number given to the constructor by icurry.
// name is the constructor name as a string for printing.
// Every pointer in args is added to the constructor children.
// Returns a pointer to the updated root.
func ConstCreate(root *Node, constructor, num_args int, name *string, args ...*Node)(*Node){
    root.number_args = 0
    root.Children = root.Children[:0]
    root.int_value = constructor
    root.name = name
    root.Children = append(root.Children, args...)
    root.number_args = num_args
    root.node_type = CONSTRUCTOR
    return root
}

// Sets root to a function node.
// function is the function to be called when the node is evaluated.
// name is the function name as a string for printing.
// number_args are the total number of arguments the function expects.
// demanded_args is a integers representing the position of an argument which have to be evaluated.
// Every pointer in args is added to the function children.
// Returns a pointer to the updated root.
func FuncCreate(root *Node, function func(*Task), name *string, number_args int, demanded_args int, args ...*Node)(*Node){
    root.number_args = 0
    root.Children = root.Children[:0]
    root.function = function
    root.number_args = number_args
    root.name = name
    root.Children = append(root.Children, args...)
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
    root.node_type = INT_LITERAL
    root.int_value = value
    return root
}

// Sets root to an float literal node with value value.
// Returns a pointer to the updated root.
func FloatLitCreate(root *Node, value float64)(*Node){
    root.Children = root.Children[:0]
    root.node_type = FLOAT_LITERAL
    root.float_literal = value
    return root
}

// Sets root to an char literal node with value value.
// Returns a pointer to the updated root.
func CharLitCreate(root *Node, value rune)(*Node){
    root.Children = root.Children[:0]
    root.node_type = CHAR_LITERAL
    root.char_literal = value
    return root
}

// Sets root to a node representing a free variable.
// Returns a pointer to the updated root.
func FreeCreate(root *Node)(*Node){
    root.Children = root.Children[:0]
    root.node_type = CONSTRUCTOR
    root.name = nextFree()
    root.int_value = -1
    return root
}

// Sets root to an IO node with a child.
// Returns a pointer to the updated root.
func IOCreate(root *Node, child *Node)(*Node){
    ConstCreate(root, 0, 1, &runtime_names[0], child)
    return root
}

// Sets root to a function that evaluates
// its argument to normalform.
// Returns a pointer to the updated root.
func NfCreate(root, arg *Node) *Node{
    return FuncCreate(root, toNf, &runtime_names[1], 1, 0, arg)  
}

// Converts a Go string to a Curry String.
// root is the node receiving the first character.
// str is the Go string.
// Returns a pointer to root.
func StringCreate(root *Node, str string)(*Node){

    // next node to write to
    cur_node := root

    runes := []rune(str)

    // go through the string
    for i := 0; i < len(runes); i++{
        // create a : at cur_node
        ConstCreate(cur_node, 1, 2, &runtime_names[4], CharLitCreate(new(Node), runes[i]), new(Node))

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

    var b bytes.Buffer // buffer to create result string

    // next character to read
    cur_node := root    

    // go through the string
    for{
        // finish on []
        if(cur_node.GetConstructor() == 0){
            break
        }

        // append next character to buffer
        b.WriteString(string(cur_node.GetChild(0).GetChar()))

        // move to the next character
        cur_node = cur_node.GetChild(1)
    }

    return b.String()
}

// Reads a Curry list and turns it
// into a go slice.
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

// Reads an unqualified term.
// root is the root node of the read term.
// term is the term to be read.
// modules is a list of paths to
// go files, to extract data constructors from.
func ReadUQTerm(root *Node, term string, modules []string)(*Node, *Node){
    var constructors [][]string
    
    // extract constructor names from modules
    for i := range(modules){
        // parse file
        fset := token.NewFileSet()
        pFile, err := parser.ParseFile(fset, modules[i], nil, 0)

        if(err != nil){
            panic(err.Error())
        }
        
        // filter file for declarations of constructor name arrays
        ast.FilterFile(pFile, func (name string)bool{
            if strings.Contains(name, "_names"){
                if(name != "func_names"){
                    return true
                }
            }
            return false
        })
        
        // extract constructor names from array declarations
        for _, decl := range(pFile.Decls){
            var names []string
            
            // traverse declaration and extract names from literals
            ast.Inspect(decl, func(n ast.Node) bool {
                lit, ok := n.(*ast.BasicLit)
                if(ok){
                    names = append(names, strings.Trim(lit.Value, "\""))
                }
                return true
            })
            
            // add names to name table
            constructors = append(constructors, names)
        }
    }
    
    
    // parse term
    node, offset := ParseTerm(root, []rune(term), constructors)
    
    // create rest string
    retString := string([]rune(term)[offset+1:])
    
    // return result
    return node, StringCreate(root.NewNode(), retString)
}

func isWhitespace(r rune) bool{
    return (r == ' ' || r == '\t' || r == '\n' || r == '\r')
}

// Parses a term.
func ParseTerm(root *Node, term []rune, constructors [][]string)(*Node, int){
    // remove leading whitespace
    if(isWhitespace(term[0])){
        start := 1
        
        for ; start < len(term); start++{
            if(!isWhitespace(term[start])){
                break
            }
        }
        
        term = term[start:]
    }
    
    // parse numbers
    if(unicode.IsDigit(term[0])){
        return ParseNumber(root, term)
    }
    
    // parse character literals
    if(term[0] == '\''){
        end := 1
        for ; end < len(term); end++{
            if(term[end] == '\''){
                break
            }
        }
        if(end < len(term)){
            if(term[end + 1] == '\''){
                end += 1
            }
        }
        
        char, _ := ParseChar(term[1:end])
        return CharLitCreate(root, char), end
    }
    
    // parse string literals
    if(term[0] == '"'){
        result := ""
        end := 0
        for i := 1; i < len(term); i++{
            // check for escape sequences
            if(term[i] == '\\'){
                if(unicode.IsDigit(term[i+1])){
                    num_end := i + 1
                    for u := i + 1; u < len(term); u++{
                        if(unicode.IsDigit(term[u])){
                            num_end = u
                        } else{
                            break
                        }
                    }
                    
                    number, _ :=  strconv.Atoi(string(term[i+1:num_end+1]))
                    result += string(rune(number))
                    i = num_end
                    continue
                }
                
                char, l := ParseChar(term[i:i+5])
                result += string(char)
                i += l - 1
                continue
            }
            
            // end on '"'
            if(term[i] == '"'){
                end = i
                break
            }
            
            result += string(term[i])
        }
        
        return StringCreate(root, result), end
    }
    
    // parse lists
    if(term[0] == '['){
        return ParseList(root, term, constructors)
    }
    
    // parse tupels
    if(term[0] == '('){
        return ParseTupel(root, term, constructors)
    }
    
    // parse constructors
    return ParseConstructor(root, term, constructors)
}

func ParseNumber(root *Node, term []rune)(*Node, int){
    isFloat := false
    end := 1
    for ; end < len(term); end++{
        // read until the digits end
        if(!unicode.IsDigit(term[end])){
            if(!isFloat){
                // test if float
                if(term[end] == '.'){
                    isFloat = true
                    continue
                }
            }
            
            break
        }
    }
    
    // return float
    if(isFloat){
        num, err := strconv.ParseFloat(string(term[:end]), 64)
            
        if(err != nil){
            panic(err.Error())
        }
        
        return FloatLitCreate(root, num), end-1
    }
    
    // return int
    num, err := strconv.Atoi(string(term[:end]))
                
    if(err != nil){
        panic(err.Error())
    }
    
    return IntLitCreate(root, num), end-1
}

func ParseString(str string)(result string){
    runes := []rune(str)
    
    for i := 0; i < len(runes); i++{
        // check for escape sequences
        if(runes[i] == '\\'){
            if(unicode.IsDigit(runes[i+1])){
                end := i+1
                for u := i + 1; u < len(runes); u++{
                    if(unicode.IsDigit(runes[u])){
                        end = u
                    } else{
                        break
                    }
                }
                
                number, _ :=  strconv.Atoi(string(runes[i+1:end+1]))
                result += string(rune(number))
                i = end
                continue
            }
            
            char, l := ParseChar(runes[i:i+5])
            result += string(char)
            i += l - 1
            continue
        }
        
        // add rune to result
        result += string(runes[i])
    }

    return
}

func ParseChar(char []rune) (rune, int){
    // return single rune
    if(len(char) == 1){
        return char[0], 1
    }
    
    // condense escape sequence
    if(char[0] == '\\'){
        // handle ascii numbers
        if(unicode.IsDigit(char[1])){
            i, err := strconv.Atoi(string(char[1:]))
            if(err != nil){
                panic("Error parsing char: '" + string(char)  + "' " + err.Error())
            }
            
            return rune(i), len(char)
        }
        
        str := string(char[1:])
        switch {
        case strings.HasPrefix(str, "a"):
            return '\a', 2
        case strings.HasPrefix(str, "b"):
            return '\b', 2
        case strings.HasPrefix(str, "\\"):
            return '\\', 2
        case strings.HasPrefix(str, "t"):
            return '\t', 2
        case strings.HasPrefix(str, "n"):
            return '\n', 2
        case strings.HasPrefix(str, "f"):
            return '\f', 2
        case strings.HasPrefix(str, "r"):
            return '\r', 2
        case strings.HasPrefix(str, "v"):
            return '\v', 2
        case strings.HasPrefix(str, "'"):
            return '\'', 2
        case strings.HasPrefix(str, "\""):
            return '"', 2
        case strings.HasPrefix(str, "NUL"):
            return 0, 4
        case strings.HasPrefix(str, "SOH"):
            return 1, 4
        case strings.HasPrefix(str, "STX"):
            return 2, 4
        case strings.HasPrefix(str, "ETX"):
            return 3, 4
        case strings.HasPrefix(str, "EOT"):
            return 4, 4
        case strings.HasPrefix(str, "ENQ"):
            return 5, 4
        case strings.HasPrefix(str, "ACK"):
            return 6, 4
        case strings.HasPrefix(str, "SO"):
            return 14, 3
        case strings.HasPrefix(str, "SI"):
            return 15, 3
        case strings.HasPrefix(str, "DLE"):
            return 16, 4
        case strings.HasPrefix(str, "DC1"):
            return 17, 4
        case strings.HasPrefix(str, "DC2"):
            return 18, 4
        case strings.HasPrefix(str, "DC3"):
            return 19, 4
        case strings.HasPrefix(str, "DC4"):
            return 20, 4
        case strings.HasPrefix(str, "NAK"):
            return 21, 4
        case strings.HasPrefix(str, "SYN"):
            return 22, 4
        case strings.HasPrefix(str, "ETB"):
            return 23, 4
        case strings.HasPrefix(str, "CAN"):
            return 24, 4
        case strings.HasPrefix(str, "EM"):
            return 25, 3
        case strings.HasPrefix(str, "SUB"):
            return 26, 4
        case strings.HasPrefix(str, "ESC"):
            return 27, 4
        case strings.HasPrefix(str, "FS"):
            return 28, 3
        case strings.HasPrefix(str, "GS"):
            return 29, 3
        case strings.HasPrefix(str, "RS"):
            return 30, 3
        case strings.HasPrefix(str, "US"):
            return 31, 3
        case strings.HasPrefix(str, "DEL"):
            return 127, 4
        }
    }
    
    // throw error
    panic("Cannot parse char: '" + string(char) + "'")
}

func ParseConstructor(root *Node, term []rune, constructors [][]string)(*Node, int){
    isTerminal := func(r rune)(bool){
        return(r == ')' || r == ']' || r == ',')
    }
    
    // parse root constructor
    end := 0
    for ; end < len(term); end++{
        if(isWhitespace(term[end]) || isTerminal(term[end])){
            break
        }
    }
    
    name := string(term[0:end])
    number := FindConstructor(name, constructors)
    
    if(number < 0){
        panic("Could not parse constructor: " + name)
    }
    
    // parse children
    var children []*Node
    for ; end < len(term); end++{
        // ignore whitespace
        if(isWhitespace(term[end])){
            continue
        }
        
        // break on terminals
        if(isTerminal(term[end])){
            break
        }
        
        // parse non constructors
        if(term[end] == '(' || term[end] == '[' || term[end] == '"' || term[end] == '\'' || unicode.IsDigit(term[end])){
            child, offset := ParseTerm(root.NewNode(), term[end:], constructors)
            children= append(children, child)
            end += offset
            continue
        }
        
        // parse constructor
        const_end := end
        for ; const_end < len(term); const_end++{
            if(isWhitespace(term[const_end]) || isTerminal(term[const_end])){
                break
            }
        }
        const_name := string(term[end:const_end])
        const_number := FindConstructor(const_name, constructors)
        
        if(const_number < 0){
            panic("Could not parse constructor: " + const_name)
        }
        
        children = append(children, ConstCreate(root.NewNode(), const_number, 0, &const_name))
        
        end = const_end - 1
    }
    
    // create root constructor
    ConstCreate(root, number, len(children), &name, children...)
    
    return root, end - 1
}

func FindConstructor(name string, constructors [][]string)(int){
    // search for constructor
    for i := range(constructors){
        for u := range(constructors[i]){
            if (name == constructors[i][u]){
                // return constructor number
                return u
            }
        }
    }
    
    // return -1 if constructor can not be found
    return -1
}

func ParseList(root *Node, term []rune, constructors [][]string)(*Node, int){
    // parse list
    var elemList []*Node
    end := 1
    for i:= 1; i < len(term); i++{
        // break on ']'
        if(term[i] == ']'){
            end = i
            break
        }
        
        // ignore','
        if(term[i] == ','){
            continue
        }
        
        // ignore whitespace
        if(isWhitespace(term[i])){
            continue
        }
        
        elem, offset := ParseTerm(root.NewNode(), term[i:], constructors)
            
        if(elem == nil){
            end = i
            break
        }
        
        elemList = append(elemList, elem)
        i += offset
    }
    
    // return empty list
    if(len(elemList) == 0){
        return ConstCreate(root, 0, 0, &runtime_names[3]), end
    }
    
    // start list on root
    ConstCreate(root, 1, 2,&runtime_names[4], elemList[0], root.NewNode())
    node := root.Children[1]
    
    // add all elements to the list
    for i := 1; i < len(elemList); i++{
        ConstCreate(node, 1, 2, &runtime_names[4], elemList[i], root.NewNode())
        node = node.Children[1]
    }
    
    // finish and return list
    ConstCreate(node, 0, 0, &runtime_names[3])
    return root, end
}


func ParseTupel(root *Node, term []rune, constructors [][]string)(*Node, int){
    // parse tupel
    name := "("
    var elemList []*Node
    end := 1
    for i := 1; i < len(term); i++{
        // break on ')'
        if(term[i] == ')'){
            end = i
            break
        }
        
        // ignore ','
        if(term[i] == ','){
            continue
        }
        
        // ignore whitespace
        if(isWhitespace(term[i])){
            continue
        }
        
        elem, offset := ParseTerm(root.NewNode(), term[i:], constructors)
        
        if(elem == nil){
            end = i
            break
        }
        
        elemList = append(elemList, elem)
        name += ","
        i += offset
    }
    name += ")"
    
    // return empty tupel
    if(len(elemList) == 0){
        return ConstCreate(root, 0, 0, &name), end
    }
    
    // return one element tupel as the element itself
    if(len(elemList) == 1){
        *root = *elemList[0]
        return root, end
    }
    
    // return tupel
    return ConstCreate(root, 0, 0, &name, elemList...), end
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

////// Methods on nodes

func (node *Node) NewNode() *Node{
    new_node := new(Node)
    new_node.ot = node.ot
    return new_node
}

func (node *Node) GetNumChildren() int{
    return len(node.Children)
}

func (node *Node) GetChild(index int) *Node{

    // eliminate redirection nodes
    for{
        lock := &node.Children[index].lock
	    lock.Lock()
        if(node.Children[index].IsRedirect()){
            node.Children[index] = node.Children[index].Children[0]
            lock.Unlock()
        } else{
            lock.Unlock()
            break
        }
    }

    // return child
    return node.Children[index]
}

func (node *Node) SetChild(index int, child *Node){
    node.Children[index] = child
}

func (node *Node) EliminateRedirect() *Node{
    // eliminate redirection nodes
    for{
        lock := &node.lock
	    lock.Lock()
        if(node.IsRedirect()){
            node = node.Children[0]
            lock.Unlock()
        } else{
            lock.Unlock()
            break
        }
    }
    return node
}

func (node *Node) GetType() NodeType{
    return node.node_type
}

func (node *Node) GetInt() int{
    return node.int_value
}

func (node *Node) GetFloat() float64{
    return node.float_literal
}

func (node *Node) GetChar() rune{
    return node.char_literal
}

func (node *Node) GetConstructor() int{
    return node.int_value
}

func (node *Node) GetFunction() func(*Task){
    return node.function
}

func (node *Node) GetDemandedArgs() int{
    return node.int_value
}

func (node *Node) GetNumArgs() int{
    return node.number_args
}

func (node *Node) GetChoiceId() int{
    return node.int_value
}

func (node *Node) GetName() string{
    return *node.name
}

func (node *Node) GetOt() int{
    return node.ot
}

func (node *Node) SetOt(ot int){
    node.ot = ot
}

func (node *Node) IsFcall() bool{
    return (node.node_type == FCALL)
}

func (node *Node) IsConst() bool{
    return (node.node_type == CONSTRUCTOR)
}

func (node *Node) IsChoice() bool{
    return (node.node_type == CHOICE)
}

func (node *Node) IsIntLit() bool{
    return (node.node_type == INT_LITERAL)
}

func (node *Node) IsFloatLit() bool{
    return (node.node_type == FLOAT_LITERAL)
}

func (node *Node) IsCharLit() bool{
    return (node.node_type == CHAR_LITERAL)
}

func (node *Node) IsExempt() bool{
    return (node.node_type == EXEMPT)
}

func (node *Node) IsRedirect() bool{
    return (node.node_type == REDIRECT)
}

func (node *Node) IsFree() bool{
    return (node.node_type == CONSTRUCTOR && node.int_value == -1)
}

func (node *Node) IsPartial() bool{
    node.lock.Lock()
    if(node.node_type == FCALL || node.node_type == CONSTRUCTOR){
        result := len(node.Children) < node.number_args
        node.lock.Unlock()
        return result
    }
    node.lock.Unlock()
    return false
}

func (node *Node) LockedIsPartial() bool{
    if(node.node_type == FCALL || node.node_type == CONSTRUCTOR){
        return len(node.Children) < node.number_args
    }
    return false
}

func (node *Node) IsHnf() bool {
    return (node.IsConst() || node.IsIntLit() || node.IsFloatLit() || node.IsCharLit() || node.LockedIsPartial())
}

func (node *Node) SetTr(id int, val *Node){
    if(node.tr == nil){
        node.tr = make(map[int]*Node)   
    }

    node.tr[id] = val
}

// Searches the task result map of node for entries
// with the key id or any key from parents.
// If a matching entry is found it and true is returned.
// Otherwise node and false is returned.
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

////// Functions for tasks tasks

func CreateTask(control *Node, id int)*Task{
    task := new(Task)
    task.id = id
    task.control = control
    task.fingerprint = make(map[int]int)
    return task
}

func EvaluateTask(task *Task, queue chan Task, result_chan chan *Node){
    queue <- *task
    done_chan := make(chan bool, 0)
    go fsTaskHandler(queue, result_chan, done_chan, 0)
}

func (task *Task) GetId() int{
    return task.id
}

func (task *Task) GetControl() *Node{
    return task.control
}

func (task *Task) NewNode() *Node{
    node := new(Node)
    node.ot = task.id
    return node
}

func (task *Task) IsBound(variable *Node) bool{
    _, ok := variable.GetTr(task.id, task.parents)
    return ok
}

// Forces the evaluation of a node to HNF.
// Doesn't guarantee the node is in HNF after one
// call. The check for HNF has to be done
// after every call and the method
// might have to be called repeatedly.
// Should only be used in external functions.
func (task *Task) ToHnf(node *Node){
    task.stack = append(task.stack, task.control)
    task.stack = append(task.stack, node)
}

// Do not share a child of the control node.
func (task *Task) NoShare(index int){
    task.control.Children[index] = CopyNode(task.control.Children[index])
}

// Takes the top entry from the stack
// and sets it as the task control node.
func (task *Task) PopStack(){
    if(len(task.stack) > 0){
        task.control = task.stack[len(task.stack) - 1]
        task.stack = task.stack[:len(task.stack) - 1]
    }
}

// Puts the current control node onto the stack
// and sets the control node to the
// specified child.
func (task *Task) MoveTo(index int){
    task.stack = append(task.stack, task.control)
    task.control = task.control.Children[index]
}

/////// Benchmark

func Benchmark(node *Node, count int, onlyHnf bool, search_strat SearchStrat, max_results int, max_tasks int){

    var sum float64 = 0

    for i := 0; i < count; i++{

        root := CopyNode(node)

        start := time.Now()

        Evaluate(root, false, onlyHnf, search_strat, max_results, max_tasks)

        end := time.Now()
        dif := end.Sub(start).Seconds()
        sum += dif
    }

    avg := sum / float64(count)

    fmt.Printf("Average time: %fs\n", avg)
}

////// Print functions

// Prints a node and all its children.
func PrintResult(node *Node) {
    result := ShowResult(node)
    if(result != ""){
        fmt.Println(result)
    }
}

// Turns a node and all its children into
// a string in standard prefix notation.
func ShowResult(node *Node)(result string){
    
    // test if node is a constructor
    if(node.IsConst()){
        // test if node is IO
        if(node.GetName() == "IO"){
            // only show child if its not ()
            if(node.GetChild(0).IsConst()){
                if(node.GetChild(0).GetName() != "()"){
                    result = ShowResult(node.GetChild(0))
                }
            } else {
                result = ShowResult(node.GetChild(0))
            }
            return
        }

        // test if node is a list
        if(node.GetName() == ":"){

            // test if the list is a string
            if(node.GetChild(0).IsCharLit()){
                // show string
                result = "\"" + ShowString(node) + "\""
                return
            }

            // show list
            result = "[" + showList(node) + "]"
            return
        }

        // test if node is a tupel
        isTupel, _ := regexp.MatchString("^\\((\054*)\\)$", node.GetName())

        if(isTupel){
            // show empty tupel
            if(len(node.Children) == 0){
                result = "()"
                return
            }
            
            // show tupel elements
            result = "(" + ShowResult(node.Children[0])
            for i := 1; i < len(node.Children); i++{
                result += ", " + ShowResult(node.Children[i])
            }
            result += ")"
            return
        }
    }
    
    // show node
    result = showNode(node)

    // show children of node
    for i := 0; i < len(node.Children); i++ {
        result += " "
        if(len(node.Children[i].Children) > 0){
            result += "(" + ShowResult(node.Children[i]) + ")"
        } else{
            result += ShowResult(node.Children[i])
        }
    }
    
    return
}

// Returns a string representation of a curry string.
// node has to be a ':' constructor.
func ShowString(node *Node)(result string){
    
    // get character
    char := node.GetChild(0).GetChar()
    if(char == 34){
        result = "\\\""
    } else if(char == 39){
        result = "'"
    } else{
        result = ShowChar(node.GetChild(0).GetChar())
    }
    
    // end string on '[]' constructor
    if(node.GetChild(1).GetName() == "[]"){
        return
    }
    
    // continue with next character
    result += ShowString(node.GetChild(1))
    return
}

// Returns a string representation of a curry list.
// node has to be a ':' constructor.
func showList(node *Node)(result string){

    // get string representation of the element
    result = ShowResult(node.GetChild(0))

    // end list on '[]' constructor
    if(node.GetChild(1).GetName() == "[]"){
        return
    }
    
    // handle not fully evaluated lists
    if(node.GetChild(1).GetName() != ":"){
        result += ", " + ShowResult(node.GetChild(1))
        return
    }

    // continue with next element
    result += ", " + showList(node.GetChild(1))
    return
}


// Returns a string representation of a node.
func showNode(node *Node) string{
    switch node.node_type {
    case INT_LITERAL:
        return strconv.Itoa(node.int_value)
    case FLOAT_LITERAL:
        return strconv.FormatFloat(node.float_literal, 'f', -1, 64)
    case CHAR_LITERAL:
        return ("'" + ShowChar(node.char_literal) + "'")
    case CONSTRUCTOR, FCALL:
        return node.GetName()
    case CHOICE:
        return "?"
    case REDIRECT:
        return "Redirect"
    case EXEMPT:
        return "Exempt"
    }
    
    return "UnknownNode"
}

func ShowChar(char rune)string{
    if(char <= 6){
        return "\\0" + strconv.Itoa(int(char))
    }
    
    if(char <= 13){
        switch char{
        case 7:
            return "\\a"
        case 8:
            return "\\b"
        case 9:
            return "\\t"
        case 10:
            return "\\n"
        case 11:
            return "\\v"
        case 12:
            return "\\f"
        case 13:
            return "\\r"
        }
    }
    
    if(char <= 31){
        return "\\" + strconv.Itoa(int(char))
    }
    
    if(char == 39){
        return "\\'"
    }
    
    if(char == 92){
        return "\\\\"
    }
    
    if(char >= 127 && char <= 255){
        return "\\" + strconv.Itoa(int(char))
    }
    
    return string(char)
}

func printDebug(node *Node, task *Task) {

    node, _ = node.GetTr(task.id, task.parents)
    
    
    if(node.IsChoice()){
        branch, ok := task.fingerprint[node.GetChoiceId()] 
        if(ok){
            printDebug(node.Children[branch], task)
            return
        }
    }
    
    // test if node is a constructor
    if(node.IsConst()){

        // test if node is a list
        if(node.GetName() == ":"){

            // test if the list is a string
            if(node.GetChild(0).IsCharLit()){
                // print string
                val := ReadString(node)
                fmt.Printf("\"%s\"",val)
                return
            }

            // print list
            fmt.Printf("[")
            printDebugList(node, task)
            fmt.Printf("]")
            return
        }

        // test if node is a tupel
        isTupel, _ := regexp.MatchString("^\\((\054*)\\)$", node.GetName())

        // do not print tupel prefix
        if(!isTupel){
            // print node if it is not a tupel
            fmt.Printf(showNode(node))
        } else{
            // print empty tupel
            if(len(node.Children) == 0){
                fmt.Printf("()")
                return
            }
        }
    }else{
        // print node
        fmt.Printf(showNode(node))
    }

    // end if node does not have Children
    if(len(node.Children) == 0){
        return
    }

    // print all Children
    fmt.Printf("(")
    printDebug(node.Children[0], task)
    for i := 1; i < len(node.Children); i++ {
        fmt.Printf(", ")
        printDebug(node.Children[i], task)
    }
    fmt.Printf(")")
}

// Prints every item of a list
// separated by commas.
// node has to be a : constructor.
func printDebugList(node *Node, task *Task){

    // print the item
    printDebug(node.GetChild(0), task)

    // end list on [] constructor
    if(node.GetChild(1).GetName() == "[]"){
        return
    }
    
    if(node.GetChild(1).GetName() != ":"){
        fmt.Printf(", ")
        printDebug(node.GetChild(1), task)
        return
    }

    // continue with next item
    fmt.Printf(", ")
    printDebugList(node.GetChild(1), task)
}
