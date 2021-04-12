package gocurry

import "fmt"
import "time"
import "regexp"
import "runtime"
import "strings"
import "strconv"
import "text/scanner"
import "go/parser"
import "go/token"
import "go/ast"

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
    for i := 0; i < len(str); i++{
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
// root is the first character of the String.
func ReadString(root *Node)(result string){
    
    // next character to read
    cur_node := root    

    // go through the string
    for{
        // finish on []
        if(cur_node.GetConstructor() == 0){
            break
        }

        // append next character to result
        result += string(cur_node.GetChild(0).GetChar())

        // move to the next character
        cur_node = cur_node.GetChild(1)
    }
    return
}

// Reads a curry list and turns it
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

// Structure to save token data
// for parsing functions.
type mToken struct{
    token rune
    name string
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
    
    // create scanner for term
    var s scanner.Scanner
    s.Init(strings.NewReader(term))
    s.Mode |= scanner.ScanFloats | scanner.ScanChars | scanner.ScanStrings
    
    // scan term
    var tokenList []mToken
    for tok := s.Scan(); tok != scanner.EOF; tok = s.Scan(){
        tokenList = append(tokenList, mToken{tok, s.TokenText()})
    }
    
    // parse term
    node, offset := ParseTokens(root, tokenList, constructors)
    retString := ""
    
    if(offset + 1 < len(tokenList)){
        for i := offset + 1; i < len(tokenList); i++{
            retString += " " + tokenList[i].name
        }
        
        if(node == nil){
            panic("Could not parse term: " + retString)
        }
    }
    
    // return result
    return node, StringCreate(root.NewNode(), retString)
}


// Parses a list of tokens.
func ParseTokens(root *Node, tokenList []mToken, constructors [][]string)(*Node, int){
    tok := tokenList[0]   
    switch tok.token{
    case scanner.Int:
        value, err := strconv.Atoi(tok.name)
        if(err != nil){
            panic(err.Error())
        }
        
        return IntLitCreate(root, value), 0
    case scanner.Float:
        value, err := strconv.ParseFloat(tok.name, 64)
        if(err != nil){
            panic(err.Error())
        }
        
        return FloatLitCreate(root, value), 0
    case scanner.Char:
        return CharLitCreate(root, []rune(tok.name)[1]), 0
    case scanner.String:
        return StringCreate(root, strings.Trim(tok.name, "\"")), 0
    case '[':
        return ParseList(root, tokenList[1:], constructors)
    case '(':
        return ParseTupel(root, tokenList[1:], constructors)
    default:
        return ParseConstructor(root, tokenList, constructors)
    }
}

func ParseConstructor(root *Node, tokenList []mToken, constructors [][]string)(*Node, int){    
    // search for constructor
    constructor_num := FindConstructor(tokenList[0].name, constructors)
    
    // return nil on invalid constructor
    if(constructor_num < 0){
        return nil, 0
    }
    
    // parse children
    isTerminal := func (tok rune)(bool){
        return tok == ')' || tok == ']' || tok == ','
    }
    var children []*Node
    
    i := 1
    loop:
    for ; i < len(tokenList); i++{
        // break on terminal
        if(isTerminal(tokenList[i].token)){
            break
        }
        
        // test for 0 arity constructors
        switch tokenList[i].token{
        case '(', '[', scanner.String, scanner.Char, scanner.Int, scanner.Float:
            // get next child
            child, offset := ParseTokens(root.NewNode(), tokenList[i:], constructors)
            
            if(child == nil){
                break loop
            }
            
            children = append(children, child)
            i += offset
        default:
            // find constructor
            number := FindConstructor(tokenList[i].name, constructors)
            
            // break on invalid constructor
            if(number < 0){
                break loop
            }
            
            // create constructor and add to children
            child := ConstCreate(root.NewNode(), number, 0, &tokenList[i].name)
            children = append(children, child)
        }
    }
    
    // create constructor node
    ConstCreate(root, constructor_num, len(children), &tokenList[0].name, children...)
    return root, (i - 1)
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

func ParseList(root *Node, tokenList []mToken, constructors [][]string)(*Node, int){
    // parse list
    var elemList []*Node
    i := 0
    for ; i < len(tokenList); i++{
        // break on terminal
        if(tokenList[i].token == ']'){
            break
        }
        
        // continue on ','
        if(tokenList[i].token == ','){
            continue
        }
        
        // get next element
        elem, offset := ParseTokens(root.NewNode(), tokenList[i:], constructors)
        
        if(elem == nil){
            break
        }
        
        elemList = append(elemList, elem)
        i += offset
    }
    
    // return empty list
    if(len(elemList) == 0){
        return ConstCreate(root, 0, 0, &runtime_names[3]), (i + 1)
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
    return root, (i + 1)
}


func ParseTupel(root *Node, tokenList []mToken, constructors [][]string)(*Node, int){
    // parse tupel
    name := "("
    var elemList []*Node
    i := 0
    for  ;i < len(tokenList); i++{
        // break on terminal
        if(tokenList[i].token == ')'){
            break        
        }
        
        // continue on ','
        if(tokenList[i].token == ','){
            continue
        }
        
        // get next element
        elem, offset := ParseTokens(root.NewNode(), tokenList[i:], constructors)
        
        if(elem == nil){
            break
        }
        
        elemList = append(elemList, elem)
        i += offset
        
        name += ","
    }
    name += ")"
    
    // return empty tupel
    if(len(elemList) == 0){
        return ConstCreate(root, 0, 0, &name), (i + 1)
    }
    
    // return one element tupel as the element itself
    if(len(elemList) == 1){
        *root = *elemList[0]
        return root, (i + 1)
    }
    
    // return tupel
    return ConstCreate(root, 0, 0, &name, elemList...), (i + 1)
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

////// Methods on tasks

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
                result = "\"" + ReadString(node) + "\""
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

// Returns a string representation of a curry list.
// node has to be a ':' constructor.
func showList(node *Node)(result string){

    // get string representation of the element
    result = ShowResult(node.GetChild(0))

    // end list on [] constructor
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
        return ("'" + string(node.char_literal) + "'")
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
