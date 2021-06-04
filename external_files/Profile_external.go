package DebugProfile

import "gocurry"
import "../../Prelude"

// TODO
func ExternalDebug_Profile_getProcessInfos(task *gocurry.Task){
    root := task.GetControl()
    
    gocurry.IOCreate(root, Prelude.Prelude__CREATE_LSbRSb(root.NewNode()))
}

func ExternalDebug_Profile_garbageCollectorOff(task *gocurry.Task){
    root := task.GetControl()
    
    gocurry.IOCreate(root, Prelude.Prelude__CREATE_LbRb(root.NewNode()))
}

// not supported
func ExternalDebug_Profile_garbageCollectorOn(task *gocurry.Task){
    root := task.GetControl()
    
    gocurry.IOCreate(root, Prelude.Prelude__CREATE_LbRb(root.NewNode()))
}

// not supported
func ExternalDebug_Profile_garbageCollect(task *gocurry.Task){
    root := task.GetControl()
    
    gocurry.IOCreate(root, Prelude.Prelude__CREATE_LbRb(root.NewNode()))
}
