package gocurry

import "fmt"
import "regexp"
import "strconv"
import "strings"

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
func ShowResult(node *Node) string{
    builder := new(strings.Builder)
    showResult(node, builder)
    return builder.String()
}

// Returns a string representation of a curry string.
// node is the entry to the representation of Curry's character list.
func ShowString(node *Node) string{
    builder := new(strings.Builder)
    showString(node, builder)
    return builder.String()
}

func showResult(node *Node, builder *strings.Builder){
    
    // test if node is a constructor
    if(node.IsConst()){
        // test if node is IO
        if(node.GetName() == "IO"){
            // only show child if its not ()
            if(node.GetChild(0).IsConst()){
                if(node.GetChild(0).GetName() != "()"){
                    showResult(node.GetChild(0), builder)
                }
            } else {
                showResult(node.GetChild(0), builder)
            }
            return
        }

        // test if node is a list
        if(node.GetName() == ":"){

            // test if the list is a string
            if(node.GetChild(0).IsCharLit()){
                // show string
                builder.WriteByte('"')
                showString(node, builder)
                builder.WriteByte('"')
                return
            }

            // show list
            builder.WriteByte('[')
            showList(node, builder)
            builder.WriteByte(']')
            return
        }

        // test if node is a tupel
        isTupel, _ := regexp.MatchString("^\\((\054*)\\)$", node.GetName())

        if(isTupel){
            // show empty tupel
            if(len(node.Children) == 0){
                builder.WriteString("()")
                return
            }
            
            // show tupel elements
            builder.WriteByte('(')
            showResult(node.Children[0], builder)
            for i := 1; i < len(node.Children); i++{
                builder.WriteString(", ")
                showResult(node.Children[i], builder)
            }
            builder.WriteByte(')')
            return
        }
    }
    
    // show node
    builder.WriteString(showNode(node))

    // show children of node
    for i := 0; i < len(node.Children); i++ {
        builder.WriteByte(' ')
        if(len(node.Children[i].Children) > 0){
            // do not use parenthesis with lists and tupels
            if(node.Children[i].IsConst()){
                if(node.Children[i].GetName() == ":" || node.Children[i].GetName() == "[]"){
                    showResult(node.Children[i], builder)
                    continue
                }
                
                isTupel, _ := regexp.MatchString("^\\((\054*)\\)$", node.Children[i].GetName())
                if(isTupel){
                    showResult(node.Children[i], builder)
                    continue
                }
            }
        
            builder.WriteByte('(')
            showResult(node.Children[i], builder)
            builder.WriteByte(')')
        } else{
            showResult(node.Children[i], builder)
        }
    }
    return
}

func showString(node *Node, builder *strings.Builder){
    
    // get character
    char := node.GetChild(0).GetChar()
    if(char == 34){
        builder.WriteString("\\\"")
    } else if(char == 39){
        builder.WriteByte('\'')
    } else{
        builder.WriteString(ShowChar(node.GetChild(0).GetChar()))
    }
    
    // end string on '[]' constructor
    if(node.GetChild(1).GetName() == "[]"){
        return
    }
    
    // continue with next character
    showString(node.GetChild(1), builder)
    return
}

// Returns a string representation of a curry list.
// node is the entry to the curry list.
func showList(node *Node, builder *strings.Builder){

    // get string representation of the element
    showResult(node.GetChild(0), builder)

    // end list on '[]' constructor
    if(node.GetChild(1).GetName() == "[]"){
        return
    }
    
    // handle not fully evaluated lists
    if(node.GetChild(1).GetName() != ":"){
        builder.WriteString(", ")
        showResult(node.GetChild(1), builder)
        return
    }

    // continue with next element
    builder.WriteString(", ")
    showList(node.GetChild(1), builder)
    return
}


// Returns a string representation of node.
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

// Returns a string representation of char.
func ShowChar(char rune) string{
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


////// debug printing functions
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
