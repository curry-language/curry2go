package gocurry

import "fmt"
import "io/ioutil"
import "os"
import "strconv"

func ExternalPrelude_ensureNotFree(task *Task){
    root := task.GetControl()
    //TODO implement properly
    root = RedirectCreate(root, root.GetChild(0))
}

func ExternalPrelude_DolExcl(task *Task){
    root := task.GetControl()
    x2 := root.GetChild(1)
    
    if(!x2.IsHNF()){
        task.ToHNF(x2)
        return
    }

    ExternalPrelude_apply(task)
    return
}

func ExternalPrelude_DolExclExcl(task *Task){
    root := task.GetControl()
    x2 := root.GetChild(1)

    if(!x2.IsNF()){
        task.ToNF(x2)
        return
    }

    ExternalPrelude_apply(task)
}

func ExternalPrelude_DolHashHash(task *Task){
    //TODO implement properly
    ExternalPrelude_DolExclExcl(task)
}

func ExternalPrelude_prim_error(task *Task){
    root := task.GetControl()

    msg := ReadString(root.GetChild(0))

    panic("ERROR: " + msg)
}

func ExternalPrelude_failed(task *Task){
    root := task.GetControl()
    ExemptCreate(root)
}

func ExternalPrelude_EqColEq(task *Task){
    panic("=:= is not yet implemented")
}

func ExternalPrelude_And(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)
    
    if(x1.GetConstructor() == 0){
        Prelude_FalseCreate(root)      
        return
    } 

    if(x2.GetConstructor() == 0){
        Prelude_FalseCreate(root)       
        return
    }

    Prelude_TrueCreate(root)
    return
}

func ExternalPrelude_prim_eqChar(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    if(x2.GetChar() == x1.GetChar()){
        Prelude_TrueCreate(root)
    } else{
        Prelude_FalseCreate(root)
    }
}

func ExternalPrelude_prim_eqInt(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    if(x2.GetInt() == x1.GetInt()){
        Prelude_TrueCreate(root)
    } else{
        Prelude_FalseCreate(root)
    }
}

func ExternalPrelude_prim_eqFloat(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    if(x2.GetFloat() == x1.GetFloat()){
        Prelude_TrueCreate(root)
    } else{
        Prelude_FalseCreate(root)
    }
}

func ExternalPrelude_prim_ltEqChar(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    if(x2.GetChar() <= x1.GetChar()){
        Prelude_TrueCreate(root)
    } else{
        Prelude_FalseCreate(root)
    }
}

func ExternalPrelude_prim_ltEqInt(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    if(x2.GetInt() <= x1.GetInt()){
        Prelude_TrueCreate(root)
    } else{
        Prelude_FalseCreate(root)
    }
}

func ExternalPrelude_prim_ltEqFloat(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    if(x2.GetFloat() <= x1.GetFloat()){
        Prelude_TrueCreate(root)
    } else{
        Prelude_FalseCreate(root)
    }
}

func ExternalPrelude_prim_ord(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    IntLitCreate(root, int(x1.GetChar()))
}

func ExternalPrelude_prim_chr(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)

    CharLitCreate(root, rune(x1.GetInt()))
}

func ExternalPrelude_prim_Int_plus(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    IntLitCreate(root, x2.GetInt() + x1.GetInt())
}

func ExternalPrelude_prim_Int_minus(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    IntLitCreate(root, x2.GetInt() - x1.GetInt())
}

func ExternalPrelude_prim_Int_times(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    IntLitCreate(root, x2.GetInt() * x1.GetInt())
}

func ExternalPrelude_prim_Int_div(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    IntLitCreate(root, x2.GetInt() / x1.GetInt())
}

func ExternalPrelude_prim_Int_mod(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    IntLitCreate(root, x2.GetInt() % x1.GetInt())
}

func ExternalPrelude_prim_Int_quot(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    IntLitCreate(root, x2.GetInt() / x1.GetInt())
}

func ExternalPrelude_prim_Int_rem(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    IntLitCreate(root, x2.GetInt() % x1.GetInt())
}

func ExternalPrelude_prim_negateFloat(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)

    FloatLitCreate(root, -x1.GetFloat())
}

func ExternalPrelude_GtGtEqDol(task *Task){
    task.NoShare(0)

    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    if(x1.IsFcall()){
        task.ToHNF(x1)
        return
    }

    RedirectCreate(root, CopyNode(x2, x1.GetChild(0)))
}

func ExternalPrelude_returnIO(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)

    IOCreate(root, x1)
}

func ExternalPrelude_prim_putChar(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)

    fmt.Printf("%c", x1.GetChar())

    IOCreate(root, Prelude_LbRbCreate(new(Node)))
}

func ExternalPrelude_getChar(task *Task){
    root := task.GetControl()
    var char rune

    fmt.Scanf("%c", &char)

    IOCreate(root, CharLitCreate(new(Node), char))
}

func ExternalPrelude_prim_readFile(task *Task){
    root := task.GetControl()
    // get child
    x1 := root.GetChild(0)

    // get file name
    name := ReadString(x1)

    // get data from file
    data, _ := ioutil.ReadFile(name)

    // return IO constructor with the data
    IOCreate(root, StringCreate(new(Node), string(data)))
}

func ExternalPrelude_prim_readFileContents(task *Task){

}

func ExternalPrelude_prim_writeFile(task *Task){
    root := task.GetControl()
    // get children
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    // get strings
    name := ReadString(x1)
    data := ReadString(x2)

    // write data to file
    ioutil.WriteFile(name, []byte(data), 0644)

    // return IO constructor
    IOCreate(root, Prelude_LbRbCreate(new(Node)))
}

func ExternalPrelude_prim_appendFile(task *Task){
    root := task.GetControl()

    // get children
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    // get strings
    name := ReadString(x1)
    data := ReadString(x2)

    // append data to file
    f, _ := os.OpenFile(name, os.O_APPEND | os.O_CREATE | os.O_WRONLY, 0644)
    f.Write([]byte(data))
    f.Close()

    // return IO constructor
    IOCreate(root, Prelude_LbRbCreate(new(Node)))
}

func ExternalPrelude_catch(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)    

    if(!x1.IsHNF()){
        task.ToHNF(x1)
        return
    }

    if(x1.GetName() == "IOError"){
        RedirectCreate(root, CopyNode(x2, x1))
        return
    }

    RedirectCreate(root, x1)
}

func ExternalPrelude_prim_show(task *Task){
    root := task.GetControl() 

    x1 := root.GetChild(0)
   
    if(x1.IsIntLit()){
        number := x1.GetInt()
        StringCreate(root, strconv.Itoa(number))
    } else if(x1.IsFloatLit()){
        number := x1.GetFloat()
        StringCreate(root, strconv.FormatFloat(number, 'f', -1, 64))
    } else if(x1.IsCharLit()){
        ListCreate(root, x1)
    } else{
        str := ReadString(x1)
        StringCreate(root, "\"" + str + "\"")
    }
}

func ExternalPrelude_apply(task *Task){
    root := task.GetControl()
    // get children
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    if(!x1.IsPartial()){
        task.ToHNF(x1)
        return
    }
    
    node := CopyNode(x1, x2)
    RedirectCreate(root, node)
}

func ExternalPrelude_cond(task *Task){

}

func ExternalPrelude_letrec(task *Task){

}

func ExternalPrelude_EqColLtEq(task *Task){
    panic("=:<= is not yet implemented")
}

func ExternalPrelude_EqColLtLtEq(task *Task){
    panic("=:<<= is not yet implemented")
}

func ExternalPrelude_ifVar(task *Task){

}

func ExternalPrelude_failure(task *Task){

}   

func ExternalPrelude_prim_readNatLiteral(task *Task){
    root := task.GetControl()
    // get first element of the String
    x1 := root.GetChild(0)

    // read it into a Go byte slice
    data := []rune(ReadString(x1))

    // if the String is empty, return an empty list
    if(len(data) == 0){
        Prelude_LSbRSbCreate(root)
        return
    }

    // read every number in the string
    if(data[0] >= 48 && data[0] <= 57){
        start := 0
        end := 1

        for i := 1; i < len(data); i++{
            if((data[i] >= 48 && data[i] <= 57)){
                end = i + 1
            } else {
                break
            }
        }

        // get the rest string
        rest := string(data[end:])

        // convert string to number
        lit, _ := strconv.Atoi(string(data[start : end]))

        // create list of results
        ListCreate(root, Prelude_LbCommaRbCreate(new(Node), IntLitCreate(new(Node), lit), StringCreate(new(Node), rest)))
    }else{
        // if the String doesn't start with a number, return an empty list
        Prelude_LSbRSbCreate(root)
    }
}

func ExternalPrelude_prim_readFloatLiteral(task *Task){
    root := task.GetControl()
    // get first element of the String
    x1 := root.GetChild(0)

    // read it into a Go byte slice
    data := []rune(ReadString(x1))

    // if the String is empty, return an empty list
    if(len(data) == 0){
        Prelude_LSbRSbCreate(root)
        return
    }

    // read every number in the string
    if(data[0] >= 48 && data[0] <= 57){
        dot := false
        start := 0
        end := 1

        for i := 1; i < len(data); i++{
            if(data[i] >= 48 && data[i] <= 57){
                end = i + 1
            }else if(data[i] == 46 && !dot){
                // make sure only one dot is counted
                dot = true
            }else{
                break
            }
        }

        // get rest String
        rest := string(data[end:])

        // convert String to number
        lit, _ := strconv.ParseFloat(string(data[start : end]), 64)

        // create list of results
        ListCreate(root, Prelude_LbCommaRbCreate(new(Node), FloatLitCreate(new(Node), lit), StringCreate(new(Node), rest)))
    }else{
        // return an empty list if the String doesn't start with a number
        Prelude_LSbRSbCreate(root)
    }
}

func ExternalPrelude_prim_readCharLiteral(task *Task){
    root := task.GetControl()
    // get first element of the String
    x1 := root.GetChild(0)

    // read it into a Go byte slice
    data := []rune(ReadString(x1))

    // if the String is empty, return an empty list
    // TODO FIX
    if(len(data) < 3){
        Prelude_LSbRSbCreate(root)
        return
    }

    // read a character
    if(data[0] == '\''){
        if(data[2] == '\''){
            //TODO fix
            rest := data[3:]
            lit := data[1]

            // create list of results
            ListCreate(root, Prelude_LbCommaRbCreate(new(Node), CharLitCreate(new(Node), lit), StringCreate(new(Node), string(rest))))
        }else{
            // return an empty list
            Prelude_LSbRSbCreate(root)
        }
    }else{
        // return an empty list
        Prelude_LSbRSbCreate(root)
    }
}

func ExternalPrelude_prim_readStringLiteral(task *Task){
    root := task.GetControl()
    // get first element of the String
    x1 := root.GetChild(0)

    // read it into a Go byte slice
    data := []rune(ReadString(x1))

    // if the String is empty, return an empty list
    if(len(data) == 0){
        Prelude_LSbRSbCreate(root)
        return
    }
    
    if (data[0] == '"'){
        start := 1
        end := -1
        for i := 1; i < len(data); i++{
            if (data[i] == '"'){
                end = i
                break
            }
        }

        if(end > 0){
            rest := string(data[end+1 : len(data)])
            lit := string(data[start : end])

            // create list of results
            ListCreate(root, Prelude_LbCommaRbCreate(new(Node), StringCreate(new(Node), lit), StringCreate(new(Node), rest)))
        }else{
            // return an empty list
        Prelude_LSbRSbCreate(root)
        }
    } else{
        // return an empty list
        Prelude_LSbRSbCreate(root)
    }
}

func ExternalPrelude_prim_Float_plus(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    FloatLitCreate(root, x2.GetFloat() + x1.GetFloat())
}

func ExternalPrelude_prim_Float_minus(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    FloatLitCreate(root, x2.GetFloat() - x1.GetFloat())
}

func ExternalPrelude_prim_Float_times(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    FloatLitCreate(root, x2.GetFloat() * x1.GetFloat())
}

func ExternalPrelude_prim_Float_div(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    FloatLitCreate(root, x2.GetFloat() / x1.GetFloat())
}

func ExternalPrelude_prim_i2f(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    FloatLitCreate(root ,float64(x1.GetInt()))
}
