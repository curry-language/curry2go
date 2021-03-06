package System_DOT_Environment

import "gocurry"
import "os"
import "runtime"
import "curry2go/Prelude"

func ExternalSystemDot_EnvironmentDot_getArgs(task *gocurry.Task){
    root := task.GetControl()
    
    // get command line args
    args := os.Args[1:]
    
    // convert args into a list of curry strings
    var curArgs = make([]*gocurry.Node, len(args))
    for i:=0; i < len(args); i++{
        curArgs[i] = gocurry.StringCreate(root.NewNode(), args[i])
    }
    
    // return args
    gocurry.IOCreate(root, Prelude.ListCreate(root.NewNode(), curArgs...))
}

func ExternalSystemDot_EnvironmentDot_primUs_getEnviron(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    key := gocurry.ReadString(x1)
    
    val := os.Getenv(key)
    
    gocurry.IOCreate(root, gocurry.StringCreate(root.NewNode(), val))
}

func ExternalSystemDot_EnvironmentDot_primUs_setEnviron(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)
    
    key := gocurry.ReadString(x1)
    val := gocurry.ReadString(x2)
    
    os.Setenv(key, val)
    
    gocurry.IOCreate(root, Prelude.Prelude__CREATE_Lb_Rb_(root.NewNode()))
}

func ExternalSystemDot_EnvironmentDot_primUs_unsetEnviron(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    key := gocurry.ReadString(x1)
    os.Unsetenv(key)
    
    gocurry.IOCreate(root, Prelude.Prelude__CREATE_Lb_Rb_(root.NewNode()))
}

func ExternalSystemDot_EnvironmentDot_getHostname(task *gocurry.Task){
    root := task.GetControl()
    
    hostName, _ := os.Hostname()
    
    gocurry.IOCreate(root, gocurry.StringCreate(root.NewNode(), hostName))
}

func ExternalSystemDot_EnvironmentDot_getProgName(task *gocurry.Task){
    root := task.GetControl()
    
    progName := os.Args[0]
    
    gocurry.IOCreate(root, gocurry.StringCreate(root.NewNode(), progName))
}

func ExternalSystemDot_EnvironmentDot_isWindows(task *gocurry.Task){
    root := task.GetControl()
    
    if(runtime.GOOS == "windows"){
        Prelude.Prelude__CREATE_True(root)
        return
    }
    
    Prelude.Prelude__CREATE_False(root)
}
