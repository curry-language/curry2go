package SystemProcess

import "gocurry"
import "os"
import "os/exec"
import "time"
import "fmt"
import "curry2go/Prelude"

func ExternalSystem_Process_getPID(task *gocurry.Task){
    root := task.GetControl()
    
    pid := os.Getpid()
    
    gocurry.IOCreate(root, gocurry.IntLitCreate(root.NewNode(), pid))
}

func ExternalSystem_Process_prim_system(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    cmd := exec.Command("/bin/sh", "-c", gocurry.ReadString(x1))
    cmd.Stdin = os.Stdin
    cmd.Stdout = os.Stdout
    cmd.Stderr = os.Stderr
    err := cmd.Run()
    
    if(err != nil) {
        fmt.Println("System.Process.system: " + err.Error())
    }
    
    gocurry.IOCreate(root, gocurry.IntLitCreate(root.NewNode(), cmd.ProcessState.ExitCode()))
}

func ExternalSystem_Process_prim_exitWith(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    code := x1.GetInt()
    
    os.Exit(code)
}

func ExternalSystem_Process_prim_sleep(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    dur := time.Duration(x1.GetInt())
    time.Sleep(dur * 1000000000)
    
    gocurry.IOCreate(root, Prelude.Prelude__CREATE_LbRb(root.NewNode()))
}
