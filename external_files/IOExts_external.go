package System_DOT_IOExts

import "gocurry"
import "curry2go/System/IO"
import "curry2go/Prelude"
import "os/exec"


func ExternalSystemDot_IOExtsDot_primUs_execCmd(task *gocurry.Task){
    root := task.GetControl()
    commandNode := root.GetChild(0)
    command := gocurry.ReadString(commandNode)
    
    // create command
    cmd := exec.Command("bash", "-c", command)
    
    // get pipes from command
    stdinPipe, err := cmd.StdinPipe()
    if(err != nil){
        panic("System.IOExts.execCmd: " + err.Error())
    }
    
    stdoutPipe, err := cmd.StdoutPipe()
    if(err != nil){
        panic("System.IOExts.execCmd: " + err.Error())
    }
    
    stderrPipe, err := cmd.StderrPipe()
    if(err != nil){
        panic("System.IOExts.execCmd: " + err.Error())
    }
    
    // create handles for pipes
    stdinHandle := System_DOT_IO.HandleCreate(root.NewNode(), nil, &stdinPipe, 1, true)
    stdoutHandle := System_DOT_IO.HandleCreate(root.NewNode(), &stdoutPipe, nil, 0, true)
    stderrHandle := System_DOT_IO.HandleCreate(root.NewNode(), &stderrPipe, nil, 0, true)
    
    // start command
    cmd.Start()
    
    // return
    tupel := Prelude.Prelude__CREATE_Lb_Comma_Comma_Comma_Rb_(root.NewNode(), stdinHandle, stdoutHandle, stderrHandle)
    gocurry.IOCreate(root, tupel)
}

func ExternalSystemDot_IOExtsDot_primUs_connectToCmd(task *gocurry.Task){
    root := task.GetControl()
    commandNode := root.GetChild(0)
    command := gocurry.ReadString(commandNode)
    
    
    // create command
    cmd := exec.Command("bash", "-c", command)
    
    // get pipes from command
    stdinPipe, err := cmd.StdinPipe()
    if(err != nil){
        panic("System.IOExts.execCmd: " + err.Error())
    }
    
    stdoutPipe, err := cmd.StdoutPipe()
    if(err != nil){
        panic("System.IOExts.execCmd: " + err.Error())
    }
    
    
    // create handles for pipes
    handle := System_DOT_IO.HandleCreate(root.NewNode(), &stdoutPipe, &stdinPipe, 1, true)
    
    // start command
    cmd.Start()
    
    // return
    gocurry.IOCreate(root, handle)
}
