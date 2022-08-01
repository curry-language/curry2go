package Prelude

import . "gocurry"
import "bufio"
import "fmt"
import "io"
import "os"
import "strconv"
import "math"

var names = []string{"writeFileHelper", "readFileHelper", "nonstrictEq", "markedTest"}

/////// Evaluation functions

func ExternalPreludeDot_ensureNotFree(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    if(!x1.IsHnf()){
        task.ToHnf(x1)
        return
    }
    
    if(x1.IsFree()){
        if(task.IsBound(x1)){
            task.ToHnf(x1)
            return    
        }
        
        panic("Prelude.ensureNotFree: Free variable")
    }
    
    root = RedirectCreate(root, x1)
}

// apply
func ExternalPreludeDot_apply(task *Task){
    root := task.GetControl()
    // get children
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    if(!x1.IsPartial()){
        task.ToHnf(x1)
        return
    }
    
    RedirectCreate(root, CopyNode(x1, x2))
}

// $!
func ExternalPreludeDot_Dol_Excl_(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(1)
    
    // evaluate argument to hnf
    if(!x1.IsHnf() || (x1.IsFree() && task.IsBound(x1))){
        task.ToHnf(x1)
        return
    }

    // apply function to argument in hnf
    ExternalPreludeDot_apply(task)
    return
}

// $!!
func ExternalPreludeDot_Dol_Excl_Excl_(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(1)
    
    // wrap argument in nf wrapper
    root.SetChild(1, NfCreate(root.NewNode(), x1))
    
    // evaluate wrapper to hnf
    Prelude__CREATE_Dol_Excl_(root, root.Children...)
}

// $##
func ExternalPreludeDot_Dol_Hash_Hash_(task *Task){
    //TODO implement properly
    ExternalPreludeDot_Dol_Excl_Excl_(task)
}

////// Control flow functions

func ExternalPreludeDot_primUs_error(task *Task){
    root := task.GetControl()

    msg := ReadString(root.GetChild(0))

    panic("Prelude.error: " + msg)
}

func ExternalPreludeDot_failed(task *Task){
    root := task.GetControl()
    ExemptCreate(root)
}

func ExternalPreludeDot_cond(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    if(!x1.IsHnf() || (x1.IsFree() && task.IsBound(x1))){
        task.ToHnf(x1)
        return
    }
    
    if(x1.GetConstructor() == 1){
        x2 := root.GetChild(1)
        RedirectCreate(root, x2)
    }else{
        ExemptCreate(root)
    }
}

////// Logic functions

// &
func ExternalPreludeDot_And_(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)
    
    // evaluate first argument
    if(!x1.IsHnf() || (x1.IsFree() && task.IsBound(x1))){
        task.ToHnf(x1)
        return
    }
    
    // return false if the first argument is false
    if(x1.GetConstructor() == 0){
        Prelude__CREATE_False(root)      
        return
    }
    
    // evaluate second argument
    if(!x2.IsHnf() || (x2.IsFree() && task.IsBound(x2))){
        task.ToHnf(x2)
        return
    } 

    // return false if the second argument is false
    if(x2.GetConstructor() == 0){
        Prelude__CREATE_False(root)       
        return
    }

    // return true
    Prelude__CREATE_True(root)
    return
}

// Tests if variable x is an argument of constructor c.
func occur_check(x, c *Node) bool{
    // test if c is a cosntructor
    if(!c.IsConst()){
        return false
    }
    
    // test arguments of c for occurrences of x
    for i := range(c.Children){
        child := c.GetChild(i)
        
        if(child == x){
            return true
        }
        
        if(occur_check(x, child)){
            return true
        }
    }
    
    return false
}

// Strict unification
func ExternalPreludeDot_constrEq(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)
    
    // evaluate children to hnf
    if(!x1.IsHnf() || (x1.IsFree() && task.IsBound(x1))){
        task.ToHnf(x1)
        return
    }
    
    if(!x2.IsHnf() || (x2.IsFree() && task.IsBound(x2))){
        task.ToHnf(x2)
        return
    }
    
    if(x1.IsFree() && x2.IsFree()){    
        // bind x1 to x2
        x1.SetTrLock(task.GetId(), RedirectCreate(task.NewNode(), x2))
        
        // return true
        Prelude__CREATE_True(root)
    } else if(x1.IsFree()){
    
        // run occur check
        if(occur_check(x1, x2)){
            ExemptCreate(root)
            return
        }
        
        // create copy of x2 to bind x1 to
        new_node := CopyNode(x2)
        new_node.SetOt(task.GetId())
        for i := range new_node.Children{
            new_node.Children[i] = FreeCreate(task.NewNode())
        }
        
        // bind x1 to copy
        x1.SetTrLock(task.GetId(), new_node)
    } else if(x2.IsFree()){
        // run occur check
        if(occur_check(x2, x1)){
            ExemptCreate(root)
            return
        }
    
        // create copy of x1 to bind x2 to
        new_node := CopyNode(x1)
        new_node.SetOt(task.GetId())
        for i := range new_node.Children{
            new_node.Children[i] = FreeCreate(task.NewNode())
        }

        // bind x1 to copy
        x2.SetTrLock(task.GetId(), new_node)
    } else{
        if(x1.IsIntLit()){
            // unify int
            if(x1.GetInt() == x2.GetInt()){
                Prelude__CREATE_True(root)
            } else{
                ExemptCreate(root)
            }
        } else if(x1.IsCharLit()){
            // unify char
            if(x1.GetChar() == x2.GetChar()){
                Prelude__CREATE_True(root)
            } else{
                ExemptCreate(root)
            }
        } else if(x1.IsFloatLit()){
            // unify float
            if(x1.GetFloat() == x2.GetFloat()){
                Prelude__CREATE_True(root)
            } else{
                ExemptCreate(root)
            }
        } else{
            // unify constructor
            if(x1.GetConstructor() == x2.GetConstructor()){
                unifChain(task, root, x1, x2)
            } else{
                ExemptCreate(root)
            }
        }
    }
}

// Helper function for the strict unification.
// Sets root to the unification of
// the children of x1 and x2.
func unifChain(task *Task, root, x1, x2 *Node){

    // no children: return true
    if(len(x1.Children) == 0){
        Prelude__CREATE_True(root)
        return
    }
    
    // unify single children
    if(len(x1.Children) == 1){
        Prelude__CREATE_constrEq(root, x1.GetChild(0), x2.GetChild(0))
        return
    }

    // combine unification of children with and
    node := Prelude__CREATE_And_And_(root, Prelude__CREATE_constrEq(root.NewNode(), x1.GetChild(0), x2.GetChild(0)), root.NewNode())
    for i := 1; i < len(x1.Children) - 1; i++{
        Prelude__CREATE_And_And_(node.Children[1], Prelude__CREATE_constrEq(root.NewNode(), x1.GetChild(i), x2.GetChild(i)) , root.NewNode())
        node = node.Children[1]                    
    }
    Prelude__CREATE_constrEq(node.Children[1], x1.GetChild(x1.GetArity() - 1), x2.GetChild(x1.GetArity() - 1))
}

// Nonstrict unification (=:<=)
func ExternalPreludeDot_nonstrictEq(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    // test for multiple variable occurences, linearize x1
    if(x1.IsFree() && x1.GetArity() < 0 && task.IsBound(x1)){
        Prelude__CREATE_constrEq(root, x1, root.GetChild(1))
        return
    }
    
    // evaluate first child to HNF
    if(!x1.IsHnf() || (x1.IsFree() && task.IsBound(x1))){
        task.ToHnf(x1)
        return
    }
    
    // get second child
    x2 := root.GetChild(1)
 
    // if x1 is a free variable bind it to the second child
    if(x1.IsFree()){
        // create redirection node to x2
        new_node := RedirectCreate(task.NewNode(), x2)

        // bind x1 to x2 and return true
        x1.SetTrLock(task.GetId(), new_node)
        Prelude__CREATE_True(root)
        
        // mark variable
        x1.SetArity(-1)
        return
    }
    
    // evaluate second child
    if(!x2.IsHnf() || (x2.IsFree() && task.IsBound(x2))){
        task.ToHnf(x2)
        return
    }
    
    if(x2.IsFree()){
        // create copy of x1 to bind x2 to
        new_node := CopyNode(x1)
        new_node.SetOt(task.GetId())
        for i := range new_node.Children{
            new_node.Children[i] = FreeCreate(task.NewNode())
        }

        // bind x1 to copy
        x2.SetTrLock(task.GetId(), new_node)
    } else{
    
        if(x1.IsIntLit()){
            // unify int
            if(x1.GetInt() == x2.GetInt()){
                Prelude__CREATE_True(root)
            } else{
                ExemptCreate(root)
            }
        } else if(x1.IsCharLit()){
            // unify char
            if(x1.GetChar() == x2.GetChar()){
                Prelude__CREATE_True(root)
            } else{
                ExemptCreate(root)
            }
        } else if(x1.IsFloatLit()){
            // unify float
            if(x1.GetFloat() == x2.GetFloat()){
                Prelude__CREATE_True(root)
            } else{
                ExemptCreate(root)
            }
        } else{
            // unify constructor
            if(x1.GetConstructor() == x2.GetConstructor()){
                nonstrictUnifChain(task, root, x1, x2)
            } else{
                ExemptCreate(root)
            }
        }
    }  
}

// Helper function for nonstrict unification.
// Sets root to the nonstrict unification of
// the children of x1 and x2.
func nonstrictUnifChain(task *Task, root, x1, x2 *Node){

    // no children: return true
    if(len(x1.Children) == 0){
        Prelude__CREATE_True(root)
        return
    }
    
    // unify single children
    if(len(x1.Children) == 1){
        Prelude__CREATE_nonstrictEq(root, x1.GetChild(0), x2.GetChild(0))
        return
    }

    // combine unification of children with and
    node := Prelude__CREATE_And_And_(root, Prelude__CREATE_nonstrictEq(root.NewNode(), x1.GetChild(0), x2.GetChild(0)), root.NewNode())
    for i := 1; i < len(x1.Children) - 1; i++{
        Prelude__CREATE_And_And_(node.Children[1], Prelude__CREATE_nonstrictEq(root.NewNode(), x1.GetChild(i), x2.GetChild(i)), root.NewNode())
        node = node.Children[1]     
    }
    Prelude__CREATE_nonstrictEq(node.Children[1], x1.GetChild(x1.GetArity() - 1), x2.GetChild(x1.GetArity() - 1))
}

////// Arithmetic on characters

func ExternalPreludeDot_primUs_eqChar(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    if(x2.GetChar() == x1.GetChar()){
        Prelude__CREATE_True(root)
    } else{
        Prelude__CREATE_False(root)
    }
}

func ExternalPreludeDot_primUs_ltEqChar(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    if(x2.GetChar() <= x1.GetChar()){
        Prelude__CREATE_True(root)
    } else{
        Prelude__CREATE_False(root)
    }
}

func ExternalPreludeDot_primUs_ord(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    IntLitCreate(root, int(x1.GetChar()))
}

func ExternalPreludeDot_primUs_chr(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)

    CharLitCreate(root, rune(x1.GetInt()))
}

////// Arithmetic on integers
func ExternalPreludeDot_primUs_eqInt(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    if(x2.GetInt() == x1.GetInt()){
        Prelude__CREATE_True(root)
    } else{
        Prelude__CREATE_False(root)
    }
}

func ExternalPreludeDot_primUs_ltEqInt(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    if(x2.GetInt() <= x1.GetInt()){
        Prelude__CREATE_True(root)
    } else{
        Prelude__CREATE_False(root)
    }
}

func ExternalPreludeDot_primUs_plusInt(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    IntLitCreate(root, x2.GetInt() + x1.GetInt())
}

func ExternalPreludeDot_primUs_minusInt(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    IntLitCreate(root, x2.GetInt() - x1.GetInt())
}

func ExternalPreludeDot_primUs_timesInt(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    IntLitCreate(root, x2.GetInt() * x1.GetInt())
}

func ExternalPreludeDot_primUs_divInt(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    IntLitCreate(root, x2.GetInt() / x1.GetInt())
}

func ExternalPreludeDot_primUs_modInt(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    IntLitCreate(root, x2.GetInt() % x1.GetInt())
}

func ExternalPreludeDot_primUs_quotInt(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    IntLitCreate(root, x2.GetInt() / x1.GetInt())
}

func ExternalPreludeDot_primUs_remInt(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    IntLitCreate(root, x2.GetInt() % x1.GetInt())
}

////// Arithmetic on floats
func ExternalPreludeDot_primUs_eqFloat(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    if(x2.GetFloat() == x1.GetFloat()){
        Prelude__CREATE_True(root)
    } else{
        Prelude__CREATE_False(root)
    }
}

func ExternalPreludeDot_primUs_ltEqFloat(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    if(x2.GetFloat() <= x1.GetFloat()){
        Prelude__CREATE_True(root)
    } else{
        Prelude__CREATE_False(root)
    }
}

func ExternalPreludeDot_primUs_negateFloat(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)

    FloatLitCreate(root, -x1.GetFloat())
}

func ExternalPreludeDot_primUs_plusFloat(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    FloatLitCreate(root, x2.GetFloat() + x1.GetFloat())
}

func ExternalPreludeDot_primUs_minusFloat(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    FloatLitCreate(root, x2.GetFloat() - x1.GetFloat())
}

func ExternalPreludeDot_primUs_timesFloat(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    FloatLitCreate(root, x2.GetFloat() * x1.GetFloat())
}

func ExternalPreludeDot_primUs_divFloat(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    FloatLitCreate(root, x2.GetFloat() / x1.GetFloat())
}

func ExternalPreludeDot_primUs_intToFloat(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    FloatLitCreate(root ,float64(x1.GetInt()))
}

func ExternalPreludeDot_primUs_truncateFloat(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    IntLitCreate(root, int(math.Trunc(x1.GetFloat())))
}

func ExternalPreludeDot_primUs_roundFloat(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    IntLitCreate(root, int(math.RoundToEven(x1.GetFloat())))    
}

func ExternalPreludeDot_primUs_logFloat(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    FloatLitCreate(root, math.Log(x1.GetFloat()))
}

func ExternalPreludeDot_primUs_expFloat(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
   FloatLitCreate(root, math.Exp(x1.GetFloat()))
}

func ExternalPreludeDot_primUs_sqrtFloat(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    FloatLitCreate(root, math.Sqrt(x1.GetFloat()))
}

func ExternalPreludeDot_primUs_sinFloat(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    FloatLitCreate(root, math.Sin(x1.GetFloat()))
}

func ExternalPreludeDot_primUs_cosFloat(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    FloatLitCreate(root, math.Cos(x1.GetFloat()))
}

func ExternalPreludeDot_primUs_tanFloat(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    FloatLitCreate(root, math.Tan(x1.GetFloat()))
}

func ExternalPreludeDot_primUs_asinFloat(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    FloatLitCreate(root, math.Asin(x1.GetFloat()))
}

func ExternalPreludeDot_primUs_acosFloat(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    FloatLitCreate(root, math.Acos(x1.GetFloat()))
}

func ExternalPreludeDot_primUs_atanFloat(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    FloatLitCreate(root, math.Atan(x1.GetFloat()))
}

func ExternalPreludeDot_primUs_sinhFloat(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    FloatLitCreate(root, math.Sinh(x1.GetFloat()))
}

func ExternalPreludeDot_primUs_coshFloat(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    FloatLitCreate(root, math.Cosh(x1.GetFloat()))
}

func ExternalPreludeDot_primUs_tanhFloat(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    FloatLitCreate(root, math.Tanh(x1.GetFloat()))
}

func ExternalPreludeDot_primUs_asinhFloat(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    FloatLitCreate(root, math.Asinh(x1.GetFloat()))
}

func ExternalPreludeDot_primUs_acoshFloat(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    FloatLitCreate(root, math.Acosh(x1.GetFloat()))
}

func ExternalPreludeDot_primUs_atanhFloat(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    FloatLitCreate(root, math.Atanh(x1.GetFloat()))
}

////// Show functions

func ExternalPreludeDot_primUs_showCharLiteral(task *Task){
    root := task.GetControl() 

    x1 := root.GetChild(0)
    
    StringCreate(root, "'" + ShowChar(x1.GetChar()) + "'")
}

func ExternalPreludeDot_primUs_showStringLiteral(task *Task){
    root := task.GetControl() 

    x1 := root.GetChild(0)
    
    str := ShowString(x1)
    StringCreate(root, "\"" + str + "\"")
}

func ExternalPreludeDot_primUs_showIntLiteral(task *Task){
    root := task.GetControl() 

    x1 := root.GetChild(0)
    
    number := x1.GetInt()
    StringCreate(root, strconv.Itoa(number))
}

func ExternalPreludeDot_primUs_showFloatLiteral(task *Task){
    root := task.GetControl() 

    x1 := root.GetChild(0)
    
    number := x1.GetFloat()
    StringCreate(root, strconv.FormatFloat(number, 'f', -1, 64))    
}

////// Read functions

func ExternalPreludeDot_primUs_readNatLiteral(task *Task){
    root := task.GetControl()
    // get first element of the String
    x1 := root.GetChild(0)

    // read it into a Go byte slice
    data := []rune(ReadString(x1))

    // if the String is empty, return an empty list
    if(len(data) == 0){
        Prelude__CREATE_LSb_RSb_(root)
        return
    }

    // read every number in the string
    if(data[0] >= 48 && data[0] <= 57){
        end := 1

        for ; end < len(data); end++{
            if(!(data[end] >= 48 && data[end] <= 57)){
                break
            }
        }

        // get the rest string
        rest := string(data[end:])

        // convert string to number
        lit, _ := strconv.Atoi(string(data[0 : end]))

        // create list of results
        ListCreate(root, Prelude__CREATE_Lb_Comma_Rb_(root.NewNode(), IntLitCreate(root.NewNode(), lit), StringCreate(root.NewNode(), rest)))
    }else{
        // if the String doesn't start with a number, return an empty list
        Prelude__CREATE_LSb_RSb_(root)
    }
}

func ExternalPreludeDot_primUs_readFloatLiteral(task *Task){
    root := task.GetControl()
    // get first element of the String
    x1 := root.GetChild(0)

    // read it into a Go byte slice
    data := []rune(ReadString(x1))

    // if the String is empty, return an empty list
    if(len(data) == 0){
        Prelude__CREATE_LSb_RSb_(root)
        return
    }

    // read every number in the string
    if(data[0] >= 48 && data[0] <= 57){
        dot := false
        end := 1

        for ; end < len(data); end++{
            // only read numbers
            if(data[end] >= 48 && data[end] <= 57){
                continue
            }
            
            // make sure only one dot is read
            if(data[end] == 46 && !dot){
                dot = true
                continue
            }
            
            break
        }

        // get rest String
        rest := string(data[end:])

        // convert String to number
        lit, _ := strconv.ParseFloat(string(data[0 : end]), 64)

        // create list of results
        ListCreate(root, Prelude__CREATE_Lb_Comma_Rb_(root.NewNode(), FloatLitCreate(root.NewNode(), lit), StringCreate(root.NewNode(), rest)))
    }else{
        // return an empty list if the String doesn't start with a number
        Prelude__CREATE_LSb_RSb_(root)
    }
}

func ExternalPreludeDot_primUs_readCharLiteral(task *Task){
    root := task.GetControl()
    // get first element of the String
    x1 := root.GetChild(0)

    // read it into a Go byte slice
    data := []rune(ReadString(x1))

    // if no char literal possible, return an empty list
    if(len(data) < 3){
        Prelude__CREATE_LSb_RSb_(root)
        return
    }

    // test if the string starts with a char literal
    if(data[0] == '\''){
        // find end of char literal
        for i := 1; i < len(data); i++{
            if(data[i] == '\''){
                // return on empty literal
                if(i == 1){
                    Prelude__CREATE_LSb_RSb_(root)
                    return
                }
            
                // parse char
                rest := data[i+1:]
                lit,_ := ParseChar(data[1:i])
                
                // return result
                ListCreate(root, Prelude__CREATE_Lb_Comma_Rb_(root.NewNode(), CharLitCreate(root.NewNode(), lit), StringCreate(root.NewNode(), string(rest))))
                return
            }
        }
        
        // return an empty list
        Prelude__CREATE_LSb_RSb_(root)
    } else{
        // return an empty list
        Prelude__CREATE_LSb_RSb_(root)
    }
}

func ExternalPreludeDot_primUs_readStringLiteral(task *Task){
    root := task.GetControl()
    // get first element of the String
    x1 := root.GetChild(0)

    // read it into a Go byte slice
    data := []rune(ReadString(x1))

    // if the String is empty, return an empty list
    if(len(data) == 0){
        Prelude__CREATE_LSb_RSb_(root)
        return
    }
    
    if (data[0] == '"'){
        end := -1
        for i := 1; i < len(data); i++{
            
            if (data[i] == '\\'){
                i += 1
                continue
            }
            
            if (data[i] == '"'){
                end = i
                break
            }
        }

        if(end > 0){
            rest := string(data[end+1 : len(data)])
            lit := string(data[1 : end])

            // create list of results
            ListCreate(root, Prelude__CREATE_Lb_Comma_Rb_(root.NewNode(), StringCreate(root.NewNode(), ParseString(lit)), StringCreate(root.NewNode(), rest)))
        }else{
            // return an empty list
        Prelude__CREATE_LSb_RSb_(root)
        }
    } else{
        // return an empty list
        Prelude__CREATE_LSb_RSb_(root)
    }
}

////// IO functions

func ExternalPreludeDot_returnIO(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)

    IOCreate(root, x1)
}

func ExternalPreludeDot_bindIO(task *Task){
    task.NoShare(0)
    
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    if(x1.IsFcall()){
        task.ToHnf(x1)
        return
    }

    Prelude__CREATE_apply(root, x2, x1.GetChild(0))
}

func ExternalPreludeDot_seqIO(task *Task){
    task.NoShare(0)

    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    if(x1.IsFcall()){
        task.ToHnf(x1)
        return
    }

    RedirectCreate(root, CopyNode(x2))
}

func ExternalPreludeDot_primUs_putChar(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)

    fmt.Printf("%c", x1.GetChar())

    IOCreate(root, Prelude__CREATE_Lb_Rb_(root.NewNode()))
}

func ExternalPreludeDot_getChar(task *Task){
    root := task.GetControl()
    var char rune

    fmt.Scanf("%c", &char)

    IOCreate(root, CharLitCreate(root.NewNode(), char))
}

func ExternalPreludeDot_primUs_readFile(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)

    // openFile
    name := ReadString(x1)
    f, err := os.Open(name)
    
    if(err != nil){
        panic("Prelude.readFile: " + err.Error())
    }
    
    // read a character
    r := bufio.NewReader(f)
    char, size, err := r.ReadRune()
    f.Close()
    
    if(err != nil){
        // return [] on EOF
        if(err == io.EOF){
            IOCreate(root, Prelude__CREATE_LSb_RSb_(root.NewNode()))
            return
        } else{
            panic("Prelude.readFile: " + err.Error())
        }
    }

    // return IO constructor with string
    position := IntLitCreate(root.NewNode(), size)
    reader := FuncCreate(root.NewNode(), readFileHelper(name), &names[1], 1, -1, position)
    IOCreate(root, Prelude__CREATE_Col_(root.NewNode(), CharLitCreate(root.NewNode(), char), reader))
}

func readFileHelper(name string)(func (*Task)){
    return func (task *Task){
        root := task.GetControl()
        position := root.GetChild(0)
    
        // open file
        f, err := os.Open(name)
    
        if(err != nil){
            panic("Prelude.readFile: " + err.Error())
        }
        
        // seek position
        f.Seek(int64(position.GetInt()), 0)
        
        // read a character
        r := bufio.NewReader(f)
        char, size, err := r.ReadRune()
        f.Close()
        
        if(err != nil){
            // return [] on EOF
            if(err == io.EOF){
                Prelude__CREATE_LSb_RSb_(root)
                return
            } else{
                panic("Prelude.readFile: " + err.Error())
            }
        }
        
        // return string
        IntLitCreate(position, position.GetInt() + size)
        reader := FuncCreate(root.NewNode(), readFileHelper(name), &names[1], 1, -1, position)
        Prelude__CREATE_Col_(root, CharLitCreate(root.NewNode(), char), reader)
    }
}

func ExternalPreludeDot_primUs_writeFile(task *Task){
    root := task.GetControl()
    // get children
    x1 := root.GetChild(0)
    str := root.GetChild(1)
    
    // evaluate string to hnf
    if(!str.IsHnf()){
        task.ToHnf(str)
        return
    }
    
    // open file
    name := ReadString(x1)
    f, err := os.OpenFile(name, os.O_CREATE | os.O_WRONLY | os.O_TRUNC, 0644)
    
    if(err != nil){
        panic("Prelude.writeFile: " + err.Error())
    }
    
    // return on empty string
    if(str.GetConstructor() == 0){
        f.Close()
        IOCreate(root, Prelude__CREATE_Lb_Rb_(root.NewNode()))
        return
    }
    
    // evaluate character
    char := str.GetChild(0)
    if(!char.IsHnf()){
        f.Close()
        task.ToHnf(char)
        return
    }

    // write character to file
    _, err = f.WriteString(string(char.GetChar()))
    
    if(err != nil){
        panic("Prelude.writeFile: " + err.Error())
    }

    // return IO constructor
    FuncCreate(root, writeFileHelper(f), &names[0], 1, 0, str.GetChild(1))
}

func writeFileHelper(f *os.File)(func (*Task)){
    return func(task *Task){
        root := task.GetControl()
        str := root.GetChild(0)
        
        // return on empty string
        if(str.GetConstructor() == 0){
            f.Close()
            IOCreate(root, Prelude__CREATE_Lb_Rb_(root.NewNode()))
            return
        }
        
        // evaluate char
        char := str.GetChild(0)
        if(!char.IsHnf()){
            task.ToHnf(char)
            return
        }
        
        // write character to file
        _, err := f.WriteString(string(char.GetChar()))
    
        if(err != nil){
            panic("Prelude.write/appendFile: " + err.Error())
        }
        
        root.SetChild(0, str.GetChild(1))
    }
}

func ExternalPreludeDot_primUs_appendFile(task *Task){
    root := task.GetControl()
    // get children
    x1 := root.GetChild(0)
    str := root.GetChild(1)
    
    // evaluate string to hnf
    if(!str.IsHnf()){
        task.ToHnf(str)
        return
    }
    
    // open file
    name := ReadString(x1)
    f, err := os.OpenFile(name, os.O_CREATE | os.O_WRONLY | os.O_APPEND, 0644)
    
    if(err != nil){
        panic("Prelude.appendFile: " + err.Error())
    }
    
    // return on empty string
    if(str.GetConstructor() == 0){
        f.Close()
        IOCreate(root, Prelude__CREATE_Lb_Rb_(root.NewNode()))
        return
    }
    
    // evaluate character
    char := str.GetChild(0)
    if(!char.IsHnf()){
        f.Close()
        task.ToHnf(char)
        return
    }

    // write character to file
    _, err = f.WriteString(string(char.GetChar()))
    
    if(err != nil){
        panic("Prelude.appendFile: " + err.Error())
    }

    // return IO constructor
    FuncCreate(root, writeFileHelper(f), &names[0], 1, 0, str.GetChild(1))
}

func ExternalPreludeDot_catch(task *Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)

    if(!x1.IsHnf()){
        task.ToHnf(x1)
        return
    }

    if(x1.IsError()){
        Prelude__CREATE_apply(root, x2, x1)
        return
    }

    RedirectCreate(root, x1)
}

////// PAKCS functions

func ExternalPreludeDot_unifEqLinear(task * Task){
    panic("Prelude.UnifEqLineary: not yet implemented")
}

func ExternalPrelude_letrec(task *Task){

}

func ExternalPrelude_ifVar(task *Task){

}

func ExternalPrelude_failure(task *Task){

}

func ExternalPrelude_prim_readFileContents(task *Task){

}


////// Utility functions

// Creates a list containing elements starting at root.
// Returns a pointer to root.
func ListCreate(root *Node, elements ...*Node)(*Node){

    // next node to write to
    cur_node := root

    for i:= 0; i < len(elements); i++{

        // create a : with the current element and the rest
        Prelude__CREATE_Col_(cur_node, elements[i], root.NewNode())
        
        // move to next node
        cur_node = cur_node.GetChild(1)
    }

    // set last element to []
    Prelude__CREATE_LSb_RSb_(cur_node)
    return root
}

