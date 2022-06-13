package System_DOT_IO

import "gocurry"
import "os"
import "time"
import "fmt"
import "errors"
import "io"
import "bufio"
import "curry2go/Prelude"

// Struct representing a handle.
type Handle struct{
    Reader *io.ReadCloser
    Writer *io.WriteCloser
    File *os.File
    Buffer rune
    Mode int
    Terminal bool
}

// Array storing Handles.
var Handles []Handle

// Array storing constructor names.
var IO_Handle_names []string = []string{"Handle"}

// Nodes with std Handles.
var stdin_node *gocurry.Node = HandleFromFile(new(gocurry.Node), os.Stdin, 0)
var stdout_node *gocurry.Node = HandleFromFile(new(gocurry.Node), os.Stdout, 1)
var stderr_node *gocurry.Node = HandleFromFile(new(gocurry.Node), os.Stderr, 1)

func HandleCreate(root *gocurry.Node, reader *io.ReadCloser, writer *io.WriteCloser, mode int, terminal bool)(*gocurry.Node){
    // create handle and add it to Handles
    handle := Handle{reader, writer, nil, '\000', mode, terminal}
    Handles = append(Handles, handle)
    
    // return handle constructor
    id := len(Handles) - 1
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

    // create handle and add it to Handles
    var handle Handle
    if(mode > 0){
        handle = Handle{nil, nil, file, '\000', mode, terminal}
    } else{
        handle = Handle{nil, nil, file, '\000', mode, terminal}
    }
    Handles = append(Handles, handle)
    
    // return handle constructor
    id := len(Handles) - 1
    gocurry.ConstCreate(root, 0, 1, &IO_Handle_names[0],
        gocurry.IntLitCreate(root.NewNode(), id))
    return root
}

func ExternalSystemDot_IODot_handleUs_eq(task *gocurry.Task){
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
    if(Handles[hIndex1].File != nil && Handles[hIndex2].File != nil){
        // compare files
        if(Handles[hIndex1].File.Fd() == Handles[hIndex2].File.Fd()){
            Prelude.Prelude__CREATE_True(root)
            return
        } else{
            Prelude.Prelude__CREATE_False(root)
            return
        }
    }
    
    // return false if readers unequal
    if(Handles[hIndex1].Reader != Handles[hIndex2].Reader){
        Prelude.Prelude__CREATE_False(root)
        return
    }
    
    // return false if writers unequal
    if(Handles[hIndex1].Writer != Handles[hIndex2].Writer){
        Prelude.Prelude__CREATE_False(root)
        return
    }
    
    // return true
    Prelude.Prelude__CREATE_True(root)
}

func ExternalSystemDot_IODot_stdin(task *gocurry.Task){
    root := task.GetControl()
    
    gocurry.RedirectCreate(root, stdin_node)
}

func ExternalSystemDot_IODot_stdout(task *gocurry.Task){
    root := task.GetControl()
    
    gocurry.RedirectCreate(root, stdout_node)
}

func ExternalSystemDot_IODot_stderr(task *gocurry.Task){
    root := task.GetControl()
    
    gocurry.RedirectCreate(root, stderr_node)
}

func ExternalSystemDot_IODot_primUs_openFile(task *gocurry.Task){
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
    file, err := os.OpenFile(path, mode, 0644)
    
    if(err != nil){
        panic("System.IO.openFile: " + err.Error())
    }
    
    // create and return Handle
    handle := HandleFromFile(root.NewNode(), file, hMode)
    gocurry.IOCreate(root, handle)
}

func ExternalSystemDot_IODot_primUs_hClose(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hIndex := handle.GetChild(0).GetInt()
    
    // close writer
    if(Handles[hIndex].Writer != nil){
        err := (*Handles[hIndex].Writer).Close()
        
        if(err != nil){
            panic("System.IO.hClose: " + err.Error())
        }
        
        // return if reader and writer are the same
        if(Handles[hIndex].Mode == 3){
            gocurry.IOCreate(root, Prelude.Prelude__CREATE_Lb_Rb_(root.NewNode()))
            return
        }
    }
    
    // close reader
    if(Handles[hIndex].Reader != nil){
        err := (*Handles[hIndex].Reader).Close()
        
        if(err != nil){
            panic("System.IO.hClose: " + err.Error())
        }
    }
    
    // close file
    if(Handles[hIndex].File != nil){
        err := Handles[hIndex].File.Close()
        
        if(err != nil){
            panic("System.IO.hClose: " + err.Error())
        }
    }

    gocurry.IOCreate(root, Prelude.Prelude__CREATE_Lb_Rb_(root.NewNode()))
}

func ExternalSystemDot_IODot_primUs_hFlush(task *gocurry.Task){
    root := task.GetControl()
    
    gocurry.IOCreate(root, Prelude.Prelude__CREATE_Lb_Rb_(root.NewNode()))
}

func ExternalSystemDot_IODot_primUs_hIsEOF(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hIndex := handle.GetChild(0).GetInt()
    
    // if buffer not empty, return false
    if(Handles[hIndex].Buffer != '\000'){
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_False(root.NewNode()))
        return
    }
    
    // test stdio
    if(Handles[hIndex].Terminal && Handles[hIndex].File != nil){
        bufReader := bufio.NewReader(Handles[hIndex].File)
        
        if(bufReader.Size() > 0){
            gocurry.IOCreate(root, Prelude.Prelude__CREATE_False(root.NewNode()))
        } else{
            gocurry.IOCreate(root, Prelude.Prelude__CREATE_True(root.NewNode()))
        }
        return
    }
    
    if(Handles[hIndex].Reader != nil){
        // read rune from handle
        var char rune
        _, err := fmt.Fscanf(*Handles[hIndex].Reader, "%c", &char)
        
        // test for eof
        if(err != nil){
            if(errors.Is(err, io.EOF)){
                gocurry.IOCreate(root, Prelude.Prelude__CREATE_True(root.NewNode()))
                return
            } else{
                panic("System.IO.hIsEOF: " + err.Error())
            }
        }
        
        // save rune in buffer
        Handles[hIndex].Buffer = char
        
        // return false
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_False(root.NewNode()))
    } else if(Handles[hIndex].File != nil){
        // read a byte from the file
        bytes := make([]byte, 1, 1)
        _, err := Handles[hIndex].File.Read(bytes)
        
        // check for eof
        if(err != nil){
            if(errors.Is(err, io.EOF)){
                gocurry.IOCreate(root, Prelude.Prelude__CREATE_True(root.NewNode()))
                return
            } else{
                panic("System.IO.hIsEOF: " + err.Error())
            }
        }
        
        // return file to old position
        _, err = Handles[hIndex].File.Seek(-1, 1)
        
        if (err != nil){
            panic("System.IO.hIsEOF: " + err.Error())
        }
        
        // return false
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_False(root.NewNode()))
    } else{
        panic("System.IO.hIsEOF: Handle cannot be read.")
    }
}

func ExternalSystemDot_IODot_primUs_hSeek(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hMode := root.GetChild(1).GetInt()
    pos := root.GetChild(2).GetInt()
    hIndex := handle.GetChild(0).GetInt()
   
    // panic if handle is not a file
    if(Handles[hIndex].File == nil){
        panic("System.IO.hSeek: Cannot seek on non file handle.")
    }
    
    // seek file
    _, err := Handles[hIndex].File.Seek(int64(pos), hMode)
    
    if(err != nil){
        panic("System.IO.hSeek: " + err.Error())
    }
    
    // discard buffer
    Handles[hIndex].Buffer = '\000'
    
    // return
    gocurry.IOCreate(root, Prelude.Prelude__CREATE_Lb_Rb_(root.NewNode()))
}

func ExternalSystemDot_IODot_primUs_hWaitForInput(task *gocurry.Task){
    // TODO: make work with stdin
    root := task.GetControl()
    handle := root.GetChild(0)
    hIndex := handle.GetChild(0).GetInt()
    
    // if buffer is not empty, return true
    if(Handles[hIndex].Buffer != '\000'){
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_True(root.NewNode()))
        return
    }
    
    // get reader
    var reader io.Reader
    if(Handles[hIndex].Reader != nil){
        reader = *Handles[hIndex].Reader
    } else if(Handles[hIndex].File != nil){
        reader = Handles[hIndex].File
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

func ExternalSystemDot_IODot_primUs_hWaitForInputs(task *gocurry.Task){
    root := task.GetControl()
    hList := gocurry.ReadList(root.GetChild(0))
    
    // if any buffer is not empty, return its index
    for i := range(hList){
        hIndex := hList[i].GetChild(0).GetInt()
        if(Handles[hIndex].Buffer != '\000'){
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
        if(Handles[hIndex].Reader != nil){
            reader = *Handles[hIndex].Reader
        } else if(Handles[hIndex].File != nil){
            reader = Handles[hIndex].File
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

func ExternalSystemDot_IODot_primUs_hGetChar(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hIndex := handle.GetChild(0).GetInt()
    
    // get buffer content
    var char rune = Handles[hIndex].Buffer
    
    // test if buffer is not empty
    if(char != '\000'){
        // empty the buffer
        Handles[hIndex].Buffer = '\000'
    } else{
    
        // get reader
        var reader io.Reader
        if(Handles[hIndex].Reader != nil){
            reader = *Handles[hIndex].Reader
        } else if(Handles[hIndex].File != nil){
            reader = Handles[hIndex].File
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

func ExternalSystemDot_IODot_primUs_hPutChar(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    char := root.GetChild(1)
    hIndex := handle.GetChild(0).GetInt()
    
    // get writer
    var writer io.Writer
    if(Handles[hIndex].Writer != nil){
        writer = *Handles[hIndex].Writer
    } else if(Handles[hIndex].File != nil){
        writer = Handles[hIndex].File
    } else{
        panic("System.IO.hPutChar: Handle cannot be written.")
    }
    
    // handle files
    _, err := fmt.Fprintf(writer, "%c", char.GetChar())
    
    if(err != nil){
        panic("System.IO.hPutChar: " + err.Error())
    }
    
    gocurry.IOCreate(root, Prelude.Prelude__CREATE_Lb_Rb_(root.NewNode()))
}

func ExternalSystemDot_IODot_primUs_hIsReadable(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hIndex := handle.GetChild(0).GetInt()
    
    // test if reader available
    if(Handles[hIndex].Reader != nil){
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_True(root.NewNode()))
        return
    }

    // test if hMode is set for reading
    if(Handles[hIndex].Mode == 0){
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_True(root.NewNode()))
    }else{
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_False(root.NewNode()))
    }
}


func ExternalSystemDot_IODot_primUs_hIsWritable(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hIndex := handle.GetChild(0).GetInt()
    
    // test if writer available
    if(Handles[hIndex].Writer != nil){
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_True(root.NewNode()))
        return
    }

    // test if hMode is set for writting
    if(Handles[hIndex].Mode > 0){
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_True(root.NewNode()))
    }else{
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_False(root.NewNode()))
    }
}

func ExternalSystemDot_IODot_primUs_hIsTerminalDevice(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hIndex := handle.GetChild(0).GetInt()
    
    if(Handles[hIndex].Terminal){
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_True(root.NewNode()))
    }else{
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_False(root.NewNode()))
    }
}





