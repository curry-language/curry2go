package SystemIO

import "gocurry"
import "os"
import "time"
import "fmt"
import "errors"
import "io"
import "bufio"
import "../../Prelude"

// Struct representing a handle.
type Handle struct{
    reader *io.ReadCloser
    writer *io.WriteCloser
    file *os.File
    buffer rune
    mode int
    terminal bool
}

// Array storing handles.
var handles []Handle

// Array storing constructor names.
var IO_Handle_names []string = []string{"Handle"}

// Nodes with std handles.
var stdin_node *gocurry.Node = HandleFromFile(new(gocurry.Node), os.Stdin, 0)
var stdout_node *gocurry.Node = HandleFromFile(new(gocurry.Node), os.Stdout, 1)
var stderr_node *gocurry.Node = HandleFromFile(new(gocurry.Node), os.Stderr, 1)

func HandleCreate(root *gocurry.Node, reader *io.ReadCloser, writer *io.WriteCloser, mode int, terminal bool)(*gocurry.Node){
    // create handle and add it to handles
    handle := Handle{reader, writer, nil, '\000', mode, terminal}
    handles = append(handles, handle)
    
    // return handle constructor
    id := len(handles) - 1
    gocurry.ConstCreate(root, 0, 1, &IO_Handle_names[0],
        gocurry.IntLitCreate(root.NewNode(), id))
    return root
}

// Creates a Handle from file.
// mode is the mode the file was opened with,
// has to be 0 (ReadMode), 1 (WriteMode) or 2 (AppendMode).
func HandleFromFile(root *gocurry.Node, file *os.File, mode int)(*gocurry.Node){
    // test if file is connected to terminal
    var terminal bool
    info, err := file.Stat()
    
    if(err != nil){
        panic("System.IO.HandleFromFile: " + err.Error())
    }
    
    if((info.Mode() & os.ModeCharDevice) != 0){
        terminal = true
    }else{
        terminal = false
    }

    // create handle and add it to handles
    var handle Handle
    if(mode > 0){
        handle = Handle{nil, nil, file, '\000', mode, terminal}
    } else{
        handle = Handle{nil, nil, file, '\000', mode, terminal}
    }
    handles = append(handles, handle)
    
    // return handle constructor
    id := len(handles) - 1
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
    
    // test if indices are the same
    if(hIndex1 == hIndex2){
        //return true
        Prelude.Prelude__CREATE_True(root)
        return
    }
    
    // test for files
    if(handles[hIndex1].file != nil && handles[hIndex2].file != nil){
        // compare files
        if(handles[hIndex1].file.Fd() == handles[hIndex2].file.Fd()){
            Prelude.Prelude__CREATE_True(root)
            return
        } else{
            Prelude.Prelude__CREATE_False(root)
            return
        }
    }
    
    // return false if readers unequal
    if(handles[hIndex1].reader != handles[hIndex2].reader){
        Prelude.Prelude__CREATE_False(root)
        return
    }
    
    // return false if writers unequal
    if(handles[hIndex1].writer != handles[hIndex2].writer){
        Prelude.Prelude__CREATE_False(root)
        return
    }
    
    // return true
    Prelude.Prelude__CREATE_True(root)
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
        mode = os.O_WRONLY | os.O_CREATE
    } else if(hMode == 2){
        mode = os.O_WRONLY | os.O_APPEND | os.O_CREATE
    }
    
    // open file
    file, err := os.OpenFile(path, mode, 0777)
    
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
    
    // close writer
    if(handles[hIndex].writer != nil){
        err := (*handles[hIndex].writer).Close()
        
        if(err != nil){
            panic("System.IO.hClose: " + err.Error())
        }
    }
    
    // close reader
    if(handles[hIndex].reader != nil){
        err := (*handles[hIndex].reader).Close()
        
        if(err != nil){
            panic("System.IO.hClose: " + err.Error())
        }
    }
    
    // close file
    if(handles[hIndex].file != nil){
        err := handles[hIndex].file.Close()
        
        if(err != nil){
            panic("System.IO.hClose: " + err.Error())
        }
    }

    gocurry.IOCreate(root, Prelude.Prelude__CREATE_LbRb(root.NewNode()))
}

func ExternalSystem_IO_prim_hFlush(task *gocurry.Task){
    root := task.GetControl()
    
    gocurry.IOCreate(root, Prelude.Prelude__CREATE_LbRb(root.NewNode()))
}

func ExternalSystem_IO_prim_hIsEOF(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hIndex := handle.GetChild(0).GetInt()
    
    // if buffer not empty, return false
    if(handles[hIndex].buffer != '\000'){
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_False(root.NewNode()))
        return
    }
    
    // test stdio
    if(handles[hIndex].terminal && handles[hIndex].file != nil){
        bufReader := bufio.NewReader(handles[hIndex].file)
        
        if(bufReader.Size() > 0){
            gocurry.IOCreate(root, Prelude.Prelude__CREATE_False(root.NewNode()))
        } else{
            gocurry.IOCreate(root, Prelude.Prelude__CREATE_True(root.NewNode()))
        }
        return
    }
    
    // get reader
    var reader io.Reader
    if(handles[hIndex].reader != nil){
        reader = *handles[hIndex].reader
    } else if(handles[hIndex].file != nil){
        reader = handles[hIndex].file
    } else{
        panic("System.IO.hIsEOF: Handle cannot be read.")
    }
    
    // read a rune from the handle
    var char rune
    _, err := fmt.Fscanf(reader, "%c", &char)
    
    // test if read failed
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
   
    // panic if handle is not a file
    if(handles[hIndex].file == nil){
        panic("System.IO.hSeek: Cannot seek on non file handle.")
    }
    
    // seek file
    _, err := handles[hIndex].file.Seek(int64(pos), hMode)
    
    if(err != nil){
        panic("System.IO.hSeek: " + err.Error())
    }
    
    // discard buffer
    handles[hIndex].buffer = '\000'
    
    // return
    gocurry.IOCreate(root, Prelude.Prelude__CREATE_LbRb(root.NewNode()))
}

func ExternalSystem_IO_prim_hWaitForInput(task *gocurry.Task){
    // TODO: make work with stdin
    root := task.GetControl()
    handle := root.GetChild(0)
    hIndex := handle.GetChild(0).GetInt()
    
    // if buffer is not empty, return true
    if(handles[hIndex].buffer != '\000'){
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_True(root.NewNode()))
        return
    }
    
    // get reader
    var reader io.Reader
    if(handles[hIndex].reader != nil){
        reader = *handles[hIndex].reader
    } else if(handles[hIndex].file != nil){
        reader = handles[hIndex].file
    } else{
        panic("System.IO.hWaitForInput: Handle cannot be read.")
    }
    
    // get timeout
    timeout := time.Duration(root.GetChild(1).GetInt())
    ch := make(chan bool)
    
    // start input test in another routine
    go func() {
        bufReader := bufio.NewReader(reader)
        
        if(bufReader.Size() > 0){
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
        
        // get reader
        var reader io.Reader
        if(handles[hIndex].reader != nil){
            reader = *handles[hIndex].reader
        } else if(handles[hIndex].file != nil){
            reader = handles[hIndex].file
        } else{
            panic("System.IO.hWaitForInputs: Handle cannot be read.")
        }
        
        go func(index int){
            bufReader := bufio.NewReader(reader)
        
            if(bufReader.Size() > 0){
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
    
        // get reader
        var reader io.Reader
        if(handles[hIndex].reader != nil){
            reader = *handles[hIndex].reader
        } else if(handles[hIndex].file != nil){
            reader = handles[hIndex].file
        } else{
            panic("System.IO.hGetChar: Handle cannot be read.")
        }
        
        // read a new rune
        _, err := fmt.Fscanf(reader, "%c", &char)
    
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
    
    // get writer
    var writer io.Writer
    if(handles[hIndex].writer != nil){
        writer = *handles[hIndex].writer
    } else if(handles[hIndex].file != nil){
        writer = handles[hIndex].file
    } else{
        panic("System.IO.hPutChar: Handle cannot be written.")
    }
    
    // handle files
    _, err := fmt.Fprintf(writer, "%c", char.GetChar())
    
    if(err != nil){
        panic("System.IO.hPutChar: " + err.Error())
    }
    
    gocurry.IOCreate(root, Prelude.Prelude__CREATE_LbRb(root.NewNode()))
}

func ExternalSystem_IO_prim_hIsReadable(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hIndex := handle.GetChild(0).GetInt()
    
    // test if reader available
    if(handles[hIndex].reader != nil){
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_True(root.NewNode()))
        return
    }

    // test if hMode is set for reading
    if(handles[hIndex].mode == 0){
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_True(root.NewNode()))
    }else{
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_False(root.NewNode()))
    }
}


func ExternalSystem_IO_prim_hIsWritable(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hIndex := handle.GetChild(0).GetInt()
    
    // test if writer available
    if(handles[hIndex].writer != nil){
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_True(root.NewNode()))
        return
    }

    // test if hMode is set for writting
    if(handles[hIndex].mode > 0){
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_True(root.NewNode()))
    }else{
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_False(root.NewNode()))
    }
}

func ExternalSystem_IO_prim_hIsTerminalDevice(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hIndex := handle.GetChild(0).GetInt()
    
    if(handles[hIndex].terminal){
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_True(root.NewNode()))
    }else{
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_False(root.NewNode()))
    }
}





