package gocurry

import "go/ast"
import "go/token"
import "go/parser"
import "strconv"
import "strings"
import "unicode"

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
        // find literal end
        end := 1
        for ; end < len(term); end++{
            if(term[end] == '\''){
                break
            }
        }
        // test if single quote literal
        if(end < len(term)){
            if(term[end + 1] == '\''){
                end += 1
            }
        }
        
        // parse char
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
                // handle  escaped numbers
                if(unicode.IsDigit(term[i+1])){
                    // find end of number
                    num_end := i + 1
                    for u := i + 1; u < len(term); u++{
                        if(unicode.IsDigit(term[u])){
                            num_end = u
                        } else{
                            break
                        }
                    }
                    
                    // parse number
                    number, _ :=  strconv.Atoi(string(term[i+1:num_end+1]))
                    result += string(rune(number))
                    i = num_end
                    continue
                }
                
                // handle escaped text
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
            
            // add char to result
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

func ParseChar(char []rune) (rune, int){
    // return single rune
    if(len(char) == 1){
        return char[0], 1
    }
    
    // condense escape sequence
    if(char[0] == '\\'){
        // handle escaped numbers
        if(unicode.IsDigit(char[1])){
            i, err := strconv.Atoi(string(char[1:]))
            if(err != nil){
                panic("Error parsing char: '" + string(char)  + "' " + err.Error())
            }
            
            return rune(i), len(char)
        }
        
        // handle escaped text
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

func ParseString(str string)(result string){
    runes := []rune(str)
    
    for i := 0; i < len(runes); i++{
        // check for escape sequences
        if(runes[i] == '\\'){
            // handle escaped numbers
            if(unicode.IsDigit(runes[i+1])){
                // find end of number
                end := i+1
                for u := i + 1; u < len(runes); u++{
                    if(unicode.IsDigit(runes[u])){
                        end = u
                    } else{
                        break
                    }
                }   
    
                // parse number
                number, _ :=  strconv.Atoi(string(runes[i+1:end+1]))
                result += string(rune(number))
                i = end
                continue
            }
            
            // handle escaped text
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
