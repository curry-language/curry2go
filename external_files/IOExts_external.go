package SystemIOExts

import "gocurry"
import "curry2go/System/IO"
import "curry2go/Prelude"
import "os/exec"


func ExternalSystem_IOExts_prim_execCmd(task *gocurry.Task){
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
    stdinHandle := SystemIO.HandleCreate(root.NewNode(), nil, &stdinPipe, 1, true)
    stdoutHandle := SystemIO.HandleCreate(root.NewNode(), &stdoutPipe, nil, 0, true)
    stderrHandle := SystemIO.HandleCreate(root.NewNode(), &stderrPipe, nil, 0, true)
    
    // start command
    cmd.Start()
    
    // return
    tupel := Prelude.Prelude__CREATE_LbCommaCommaCommaRb(root.NewNode(), stdinHandle, stdoutHandle, stderrHandle)
    gocurry.IOCreate(root, tupel)
}

func ExternalSystem_IOExts_prim_connectToCmd(task *gocurry.Task){
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
    handle := SystemIO.HandleCreate(root.NewNode(), &stdoutPipe, &stdinPipe, 1, true)
    
    // start command
    cmd.Start()
    
    // return
    gocurry.IOCreate(root, handle)
}
