package System_DOT_IO_DOT_BinaryFile

import "gocurry"
import "curry2go/Prelude"
import "curry2go/System/IO"

func ExternalSystemDot_IODot_BinaryFileDot_primUs_openBinaryFile(task *gocurry.Task){
    System_DOT_IO.ExternalSystemDot_IODot_primUs_openFile(task)
}

func ExternalSystemDot_IODot_BinaryFileDot_primUs_hGetByte(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hIndex := handle.GetChild(0).GetInt()

    // test if handle is a file
    if(System_DOT_IO.Handles[hIndex].File == nil){
        panic("System.IO.BinaryFile.hPutByte: Handle is not a file.")
    }
    
    // read a byte of the file
    bytes := make([]byte, 1, 1)
    _, err := System_DOT_IO.Handles[hIndex].File.Read(bytes)

    if(err != nil){
        panic("System.IO.BinaryFile.hGetByte: " + err.Error())
    }

	gocurry.IOCreate(root, gocurry.IntLitCreate(root.NewNode(), int(bytes[0])))
}

func ExternalSystemDot_IODot_BinaryFileDot_primUs_hPutByte(task *gocurry.Task){
    root := task.GetControl()
    handle := root.GetChild(0)
    hIndex := handle.GetChild(0).GetInt()
    content := root.GetChild(1).GetInt()

    // test if handle is a file
    if(System_DOT_IO.Handles[hIndex].File == nil){
        panic("System.IO.BinaryFile.hPutByte: Handle is not a file.")
    }

    // write content to file
    bytes := []byte{byte(content)}
    _, err := System_DOT_IO.Handles[hIndex].File.Write(bytes)

    if(err != nil){
        panic("System.IO.BinaryFile.hPutByte: " + err.Error())
    }

    gocurry.IOCreate(root, Prelude.Prelude__CREATE_Lb_Rb_(root.NewNode()))
}