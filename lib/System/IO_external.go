package SystemIO

import "gocurry"
import "os"
import "time"
import "fmt"
import "errors"
import "io"
import "bufio"
import "../../Prelude"

type Handle struct{
    file *os.File
    buffer rune
    mode int
}

// Array storing file pointers of handles.
var handles []Handle

var IO_Handle_names []string = []string{"Handle"}

var stdin_node *gocurry.Node = HandleFromFile(new(gocurry.Node), os.Stdin, 0)
var stdout_node *gocurry.Node = HandleFromFile(new(gocurry.Node), os.Stdout, 1)
var stderr_node *gocurry.Node = HandleFromFile(new(gocurry.Node), os.Stderr, 1)

// Implements the Handle data type.
// A Handle is a constructor with two children of type INT_LITERAL.
// The first child stores the index, where the file pointer
// handle is stored in the handles array.
func HandleFromFile(root *gocurry.Node, file *os.File, mode int)(*gocurry.Node){
    id := len(handles)
    handle := Handle{file, '\000', mode}
    handles = append(handles, handle)
    
    gocurry.ConstCreate(root, 0, 1, &IO_Handle_names[0],
        gocurry.IntLitCreate(root.NewNode(), id))
    return root
}

func ExternalSystem_IO_handle_eq(task *gocurry.Task){
    root := task.GetControl()
    handle1 := root.GetChild(0)
    handle2 := root.GetChild(1)
    hIndex1 := handle1.GetChild(0).GetInt()
    hIndex2 := handle2.GetChild(0).GetInt()
    
    // compare files
    if(handles[hIndex1].file.Fd() == handles[hIndex2].file.Fd()){
        Prelude.Prelude__CREATE_True(root)
    } else{
        Prelude.Prelude__CREATE_False(root)
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
    handle := HandleFromFile(root.NewNode(), file, hMode)
    gocurry.IOCreate(root, handle)
}

func ExternalSystem_IO_prim_hClose(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hIndex := handle.GetChild(0).GetInt()
    
    // handle files
    err := handles[hIndex].file.Close()
    
    if(err != nil){
        panic("System.IO.hClose: " + err.Error())
    }
    
    gocurry.IOCreate(root, Prelude.Prelude__CREATE_LbRb(root.NewNode()))
}

func ExternalSystem_IO_prim_hFlush(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hIndex := handle.GetChild(0).GetInt()
    
    // handle files
    err := handles[hIndex].file.Sync()
    
    if(err != nil){
        panic("System.IO.hFlush: " + err.Error())
    }
    
    gocurry.IOCreate(root, Prelude.Prelude__CREATE_LbRb(root.NewNode()))
}

func ExternalSystem_IO_prim_hIsEOF(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hIndex := handle.GetChild(0).GetInt()
    
    // handle files
    // if buffer not empty, return false
    if(handles[hIndex].buffer != '\000'){
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_False(root.NewNode()))
        return
    }
    
    // read a rune from the handle
    var char rune
    _, err := fmt.Fscanf(handles[hIndex].file, "%c", &char)
    
    // test if read was successful
    if(err != nil){
        // return True on EOF
        if(errors.Is(err, io.EOF)){
            gocurry.IOCreate(root, Prelude.Prelude__CREATE_True(root.NewNode()))
        } else{
            panic("System.IO.hIsEOF: " + err.Error())
        }
    } else{
        // store read rune in buffer and return false
        handles[hIndex].buffer = char
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_False(root.NewNode()))
    }
}

func ExternalSystem_IO_prim_hSeek(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hMode := root.GetChild(1).GetInt()
    pos := root.GetChild(2).GetInt()
    hIndex := handle.GetChild(0).GetInt()
   
    // handle files
    _, err := handles[hIndex].file.Seek(int64(pos), hMode)
    
    if(err != nil){
        panic("System.IO.hSeek: " + err.Error())
    }
    
    handles[hIndex].buffer = '\000'
    
    gocurry.IOCreate(root, Prelude.Prelude__CREATE_LbRb(root.NewNode()))
}

func ExternalSystem_IO_prim_hWaitForInput(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hIndex := handle.GetChild(0).GetInt()
    
    // if buffer is not empty, return true
    if(handles[hIndex].buffer != '\000'){
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_True(root.NewNode()))
        return
    }
    
    // get timeout
    timeout := time.Duration(root.GetChild(1).GetInt())
    ch := make(chan bool)
    
    // start input test in another routine
    go func() {
        reader := bufio.NewReader(handles[hIndex].file)
        
        if(reader.Size() > 0){
            ch <- true
        }
    }()
    
    // wait for available input or timeout
    select{
    case  <-ch:
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_True(root.NewNode()))
    case  <-time.After(timeout * time.Millisecond):
         gocurry.IOCreate(root, Prelude.Prelude__CREATE_False(root.NewNode()))
    }
}

func ExternalSystem_IO_prim_hWaitForInputs(task *gocurry.Task){
    root := task.GetControl()
    hList := gocurry.ReadList(root.GetChild(0))
    
    // if any buffer is not empty, return its index
    for i := range(hList){
        hIndex := hList[i].GetChild(0).GetInt()
        if(handles[hIndex].buffer != '\000'){
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
            reader := bufio.NewReader(handles[hIndex].file)
        
            if(reader.Size() > 0){
                ch <- i
            }
        }(i)
    }
    
    // wait for available input or timeout
    select{
    case  index := <-ch:
        gocurry.IOCreate(root, gocurry.IntLitCreate(root.NewNode(), index))
    case  <-time.After(timeout * time.Millisecond):
         gocurry.IOCreate(root, gocurry.IntLitCreate(root.NewNode(), -1))
    }
}

func ExternalSystem_IO_prim_hGetChar(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hIndex := handle.GetChild(0).GetInt()
    
    // get buffer content
    var char rune = handles[hIndex].buffer
    
    // test if buffer is not empty
    if(char != '\000'){
        // empty the buffer
        handles[hIndex].buffer = '\000'
    } else{
        // read a new rune
        _, err := fmt.Fscanf(handles[hIndex].file, "%c", &char)
    
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
    hIndex := handle.GetChild(0).GetInt()
    
    // handle files
    _, err := handles[hIndex].file.WriteString(string(char.GetChar()))
    
    if(err != nil){
        panic("System.IO.hPutChar: " + err.Error())
    }
    
    gocurry.IOCreate(root, Prelude.Prelude__CREATE_LbRb(root.NewNode()))
}

func ExternalSystem_IO_prim_hIsReadable(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hMode := handles[handle.GetChild(0).GetInt()].mode

    if(hMode == 0){
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_True(root.NewNode()))
    }else{
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_False(root.NewNode()))
    }
}


func ExternalSystem_IO_prim_hIsWritable(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hMode := handles[handle.GetChild(0).GetInt()].mode

    if(hMode > 0){
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_True(root.NewNode()))
    }else{
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_False(root.NewNode()))
    }
}

func ExternalSystem_IO_prim_hIsTerminalDevice(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hIndex := handle.GetChild(0).GetInt()
    
    // handle files
    info, err := handles[hIndex].file.Stat()
    
    if(err != nil){
        panic("System.IO.hIsTerminalDevice: " + err.Error())
    }
    
    if((info.Mode() & os.ModeCharDevice) != 0){
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_True(root.NewNode()))
    }else{
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_False(root.NewNode()))
    }
}





