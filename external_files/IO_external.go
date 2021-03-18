package SystemIO

import "gocurry"
import "os"
import "bytes"
import "../../Prelude"


var handles []*os.File = []*os.File{os.Stdin, os.Stdout, os.Stderr}

var IO_Handle_names []string = []string{"Handle"}

func HandleCreate(root *gocurry.Node, handle *os.File, mode int)(*gocurry.Node){
    id := len(handles)
    handles = append(handles, handle)
    
    gocurry.ConstCreate(root, 0, 1, &IO_Handle_names[0], gocurry.IntLitCreate(root.NewNode(), id), gocurry.IntLitCreate(root.NewNode(), mode))
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
    
    gocurry.ConstCreate(root, 0, 1, &IO_Handle_names[0], gocurry.IntLitCreate(root.NewNode(), 0), gocurry.IntLitCreate(root.NewNode(), 0))
}

func ExternalSystem_IO_stdout(task *gocurry.Task){
    root := task.GetControl()
    
    gocurry.ConstCreate(root, 0, 1, &IO_Handle_names[0], gocurry.IntLitCreate(root.NewNode(), 1), gocurry.IntLitCreate(root.NewNode(), 1))
}

func ExternalSystem_IO_stderr(task *gocurry.Task){
    root := task.GetControl()
    
    gocurry.ConstCreate(root, 0, 1, &IO_Handle_names[0], gocurry.IntLitCreate(root.NewNode(), 2), gocurry.IntLitCreate(root.NewNode(), 1))
}

func ExternalSystem_IO_prim_openFile(task *gocurry.Task){
    root := task.GetControl()
    hPath := root.GetChild(0)
    hMode := root.GetChild(1).GetInt()
    
    path := gocurry.ReadString(hPath)
    var mode int
    
    if(hMode == 0){
        mode = os.O_RDONLY
    } else if(hMode == 1){
        mode = os.O_WRONLY
    } else if(hMode == 2){
        mode = os.O_WRONLY | os.O_APPEND
    }
    
    file, err := os.OpenFile(path, mode, 0)
    
    if(err != nil){
        panic(err.Error())
    }
    
    handle := HandleCreate(root.NewNode(), file, hMode)
    
    gocurry.IOCreate(root, handle)
}

func ExternalSystem_IO_prim_hClose(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hIndex := handle.GetChild(0).GetInt()
    
    handles[hIndex].Close()
    
    gocurry.IOCreate(root, Prelude.Prelude_LbRbCreate(root.NewNode()))
}

func ExternalSystem_IO_prim_hFlush(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hIndex := handle.GetChild(0).GetInt()
    
    handles[hIndex].Sync()
    
    gocurry.IOCreate(root, Prelude.Prelude_LbRbCreate(root.NewNode()))
}

func ExternalSystem_IO_prim_hIsEOF(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hIndex := handle.GetChild(0).GetInt()
    
    fInfo,_ := handles[hIndex].Stat()
    size := fInfo.Size()
    pos,_ :=  handles[hIndex].Seek(0,1)
    
    if(size == pos){
        gocurry.IOCreate(root, Prelude.Prelude_TrueCreate(root.NewNode()))
    } else{
        gocurry.IOCreate(root, Prelude.Prelude_FalseCreate(root.NewNode()))
    }
}

func ExternalSystem_IO_prim_hSeek(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hMode := root.GetChild(1).GetInt()
    pos := root.GetChild(2).GetInt()
    
    hIndex := handle.GetChild(0).GetInt()
    handles[hIndex].Seek(int64(pos), hMode)
    
    gocurry.IOCreate(root, Prelude.Prelude_LbRbCreate(root.NewNode()))
}

func ExternalSystem_IO_prim_hWaitForInput(task *gocurry.Task){
    panic("System.IO.hWaitForInput: not yet implemented")
}

func ExternalSystem_IO_prim_hWaitForInputs(task *gocurry.Task){
    panic("System.IO.hWaitForInputs: not yet implemented")
}

func ExternalSystem_IO_prim_hGetChar(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hIndex := handle.GetChild(0).GetInt()
    
    byteBuf := make([]byte, 4)
    n, err := handles[hIndex].Read(byteBuf)
    
    if(n == 0){
        panic(err.Error())
    }
    
    reader := bytes.NewReader(byteBuf)
    char, size, _ := reader.ReadRune()
    
    handles[hIndex].Seek(int64(size - n),1)
    
    gocurry.IOCreate(root, gocurry.CharLitCreate(root.NewNode(), char))
}

func ExternalSystem_IO_prim_hPutChar(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    char := root.GetChild(1)
    
    hIndex := handle.GetChild(0)
    
    handles[hIndex.GetInt()].WriteString(string(char.GetChar()))
    
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
    panic("System.IO.hisTerminalDevice: not yet implemented")
}





