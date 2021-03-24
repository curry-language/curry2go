package SystemIO

import "gocurry"
import "os"
import "time"
import "fmt"
import "errors"
import "io"
import "bufio"
import "../../Prelude"

// Array storing file pointers of handles.
var handles []*os.File = []*os.File{}

var IO_Handle_names []string = []string{"Handle"}

var stdin_node *gocurry.Node = HandleCreate(new(gocurry.Node), os.Stdin, 0)
var stdout_node *gocurry.Node = HandleCreate(new(gocurry.Node), os.Stdout, 1)
var stderr_node *gocurry.Node = HandleCreate(new(gocurry.Node), os.Stderr, 1)

// Implements the Handle data type.
// A Handle is a constructor with two children of type INT_LITERAL.
// The first child stores the index, where the file pointer
// handle is stored in the handles array.
// The second child saves the opening mode of the Handle.
// A third child of type CHAR_LITERAL acts as a read-buffer
// in order to test for EOF.
func HandleCreate(root *gocurry.Node, handle *os.File, mode int)(*gocurry.Node){
    id := len(handles)
    handles = append(handles, handle)
    
    gocurry.ConstCreate(root, 0, 1, &IO_Handle_names[0],
        gocurry.IntLitCreate(root.NewNode(), id),
        gocurry.IntLitCreate(root.NewNode(), mode),
        gocurry.CharLitCreate(root.NewNode(), '\000'))
    return root
}

func ExternalSystem_IO_handle_eq(task *gocurry.Task){
    root := task.GetControl()
    handle1 := root.GetChild(0)
    handle2 := root.GetChild(1)
    hIndex1 := handle1.GetChild(0).GetInt()
    hIndex2 := handle2.GetChild(0).GetInt()
    
    if(handles[hIndex1].Fd() == handles[hIndex2].Fd()){
        Prelude.Prelude_TrueCreate(root)
    } else{
        Prelude.Prelude_FalseCreate(root)
    }
}

func ExternalSystem_IO_stdin(task *gocurry.Task){
    root := task.GetControl()
    
    gocurry.RedirectCreate(root, stdin_node)
}

func ExternalSystem_IO_stdout(task *gocurry.Task){
    root := task.GetControl()
    
    gocurry.RedirectCreate(root, stdout_node)
}

func ExternalSystem_IO_stderr(task *gocurry.Task){
    root := task.GetControl()
    
    gocurry.RedirectCreate(root, stderr_node)
}

func ExternalSystem_IO_prim_openFile(task *gocurry.Task){
    root := task.GetControl()
    hPath := root.GetChild(0)
    hMode := root.GetChild(1).GetInt()
    
    // turn curry string into go string
    path := gocurry.ReadString(hPath)
    
    // turn curry opening mode into go opening mode
    var mode int
    if(hMode == 0){
        mode = os.O_RDONLY
    } else if(hMode == 1){
        mode = os.O_WRONLY
    } else if(hMode == 2){
        mode = os.O_WRONLY | os.O_APPEND
    }
    
    // open file
    file, err := os.OpenFile(path, mode, 0)
    
    if(err != nil){
        panic("System.IO.openFile: " + err.Error())
    }
    
    // create and return Handle
    handle := HandleCreate(root.NewNode(), file, hMode)
    gocurry.IOCreate(root, handle)
}

func ExternalSystem_IO_prim_hClose(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hIndex := handle.GetChild(0).GetInt()
    
    err := handles[hIndex].Close()
    
    if(err != nil){
        panic("System.IO.hClose: " + err.Error())
    }
    
    gocurry.IOCreate(root, Prelude.Prelude_LbRbCreate(root.NewNode()))
}

func ExternalSystem_IO_prim_hFlush(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hIndex := handle.GetChild(0).GetInt()
    
    err := handles[hIndex].Sync()
    
    if(err != nil){
        panic("System.IO.hFlush: " + err.Error())
    }
    
    gocurry.IOCreate(root, Prelude.Prelude_LbRbCreate(root.NewNode()))
}

func ExternalSystem_IO_prim_hIsEOF(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hIndex := handle.GetChild(0).GetInt()
    hBuffer := handle.GetChild(2)
    
    // if buffer not empty, return false
    if(hBuffer.GetChar() != '\000'){
        gocurry.IOCreate(root, Prelude.Prelude_FalseCreate(root.NewNode()))
        return
    }
    
    // read a rune from the handle
    var char rune
    _, err := fmt.Fscanf(handles[hIndex], "%c", &char)
    
    // test if read was successful
    if(err != nil){
        // return True on EOF
        if(errors.Is(err, io.EOF)){
            gocurry.IOCreate(root, Prelude.Prelude_TrueCreate(root.NewNode()))
        } else{
            panic("System.IO.hIsEOF: " + err.Error())
        }
    } else{
        // store read rune in buffer and return false
        gocurry.CharLitCreate(hBuffer, char)
        gocurry.IOCreate(root, Prelude.Prelude_FalseCreate(root.NewNode()))
    }
}

func ExternalSystem_IO_prim_hSeek(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hMode := root.GetChild(1).GetInt()
    pos := root.GetChild(2).GetInt()
    hIndex := handle.GetChild(0).GetInt()
    hBuffer := handle.GetChild(2)
    
    _, err := handles[hIndex].Seek(int64(pos), hMode)
    
    if(err != nil){
        panic("System.IO.hSeek: " + err.Error())
    }
    
    gocurry.CharLitCreate(hBuffer, '\000')
    
    gocurry.IOCreate(root, Prelude.Prelude_LbRbCreate(root.NewNode()))
}

func ExternalSystem_IO_prim_hWaitForInput(task *gocurry.Task){
    // TODO: make it work with stdin
    root := task.GetControl()
    handle := root.GetChild(0)
    hIndex := handle.GetChild(0).GetInt()
    hBuffer := handle.GetChild(2)
    
    // if buffer is not empty, return true
    if(hBuffer.GetChar() != '\000'){
        gocurry.IOCreate(root, Prelude.Prelude_TrueCreate(root.NewNode()))
        return
    }
    
    // get timeout
    timeout := time.Duration(root.GetChild(1).GetInt())
    ch := make(chan bool)
    
    // start input test in another routine
    go func() {
        reader := bufio.NewReader(handles[hIndex])
        
        if(reader.Size() > 0){
            ch <- true
        }
    }()
    
    // wait for available input or timeout
    select{
    case  <-ch:
        gocurry.IOCreate(root, Prelude.Prelude_TrueCreate(root.NewNode()))
    case  <-time.After(timeout * time.Millisecond):
         gocurry.IOCreate(root, gocurry.IntLitCreate(root.NewNode(), -1))
    }
}

func ExternalSystem_IO_prim_hWaitForInputs(task *gocurry.Task){
    root := task.GetControl()
    hList := gocurry.ReadList(root.GetChild(0))
    
    // if any buffer is not empty, return its index
    for i := range(hList){
        hBuffer := hList[i].GetChild(2)
        if(hBuffer.GetChar() != '\000'){
            gocurry.IOCreate(root, gocurry.IntLitCreate(root.NewNode(), i))
        }
    }
    
    // get timeout
    timeout := time.Duration(root.GetChild(1).GetInt())
    ch := make(chan int)
    
    // start input test in another routine for every handle
    for i := range(hList){
        hIndex := hList[i].GetChild(0).GetInt()
        
        go func(index int){
            reader := bufio.NewReader(handles[hIndex])
        
            if(reader.Size() > 0){
                ch <- i
            }
        }(i)
    }
    
    // wait for available input or timeout
    select{
    case  <-ch:
        gocurry.IOCreate(root, Prelude.Prelude_TrueCreate(root.NewNode()))
    case  <-time.After(timeout * time.Millisecond):
         gocurry.IOCreate(root, gocurry.IntLitCreate(root.NewNode(), -1))
    }
}

func ExternalSystem_IO_prim_hGetChar(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hIndex := handle.GetChild(0).GetInt()
    hBuffer := handle.GetChild(2)
    
    // get buffer content
    var char rune = hBuffer.GetChar()
    
    // test if buffer is not empty
    if(char != '\000'){
        // empty the buffer
        gocurry.CharLitCreate(hBuffer, '\000')
    } else{
        // read a new rune
        _, err := fmt.Fscanf(handles[hIndex], "%c", &char)
    
        if(err != nil){
            panic("System.IO.hGetChar: " + err.Error())
        }
    }
    
    // return char
    gocurry.IOCreate(root, gocurry.CharLitCreate(root.NewNode(), char))
}

func ExternalSystem_IO_prim_hPutChar(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    char := root.GetChild(1)
    hIndex := handle.GetChild(0)
    
    _, err := handles[hIndex.GetInt()].WriteString(string(char.GetChar()))
    
    if(err != nil){
        panic("System.IO.hPutChar: " + err.Error())
    }
    
    gocurry.IOCreate(root, Prelude.Prelude_LbRbCreate(root.NewNode()))
}

func ExternalSystem_IO_prim_hIsReadable(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hMode := handle.GetChild(1).GetInt()

    if(hMode == 0){
        gocurry.IOCreate(root, Prelude.Prelude_TrueCreate(root.NewNode()))
    }else{
        gocurry.IOCreate(root, Prelude.Prelude_FalseCreate(root.NewNode()))
    }
}


func ExternalSystem_IO_prim_hIsWritable(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hMode := handle.GetChild(1).GetInt()

    if(hMode > 0){
        gocurry.IOCreate(root, Prelude.Prelude_TrueCreate(root.NewNode()))
    }else{
        gocurry.IOCreate(root, Prelude.Prelude_FalseCreate(root.NewNode()))
    }
}

func ExternalSystem_IO_prim_hIsTerminalDevice(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hIndex := handle.GetChild(0).GetInt()
    
    info, err := handles[hIndex].Stat()
    
    if(err != nil){
        panic("System.IO.hIsTerminalDevice: " + err.Error())
    }
    
    if((info.Mode() & os.ModeCharDevice) != 0){
        gocurry.IOCreate(root, Prelude.Prelude_TrueCreate(root.NewNode()))
    }else{
        gocurry.IOCreate(root, Prelude.Prelude_FalseCreate(root.NewNode()))
    }
}





