package Debug_DOT_Profile

import "gocurry"
import "runtime"
import "runtime/debug"
import "curry2go/Prelude"


func ExternalDebugDot_ProfileDot_getProcessInfos(task *gocurry.Task){
    root := task.GetControl()
    
    // get elapsed time
    elapsed := gocurry.IntLitCreate(root.NewNode(), int(gocurry.Uptime().Milliseconds()))
    elapsed_node := Prelude.Prelude__CREATE_Lb_Comma_Rb_(root.NewNode(), Debug_DOT_Profile__CREATE_ElapsedTime(root.NewNode()), elapsed)
    
    // get MemStats
    mem_stats := new(runtime.MemStats)
    runtime.ReadMemStats(mem_stats)
    
    // get total memory
    sys := gocurry.IntLitCreate(root.NewNode(), int(mem_stats.Sys))
    sys_node := Prelude.Prelude__CREATE_Lb_Comma_Rb_(root.NewNode(), Debug_DOT_Profile__CREATE_Memory(root.NewNode()), sys)
    
    // get stack memory
    stack := gocurry.IntLitCreate(root.NewNode(), int(mem_stats.StackSys))
    stack_node := Prelude.Prelude__CREATE_Lb_Comma_Rb_(root.NewNode(), Debug_DOT_Profile__CREATE_Stack(root.NewNode()), stack)
    
    // get heap memory
    heap := gocurry.IntLitCreate(root.NewNode(), int(mem_stats.HeapAlloc))
    heap_node := Prelude.Prelude__CREATE_Lb_Comma_Rb_(root.NewNode(), Debug_DOT_Profile__CREATE_Heap(root.NewNode()), heap)
    
    // get garbage collection count
    gc := gocurry.IntLitCreate(root.NewNode(), int(mem_stats.NumGC))
    gc_node := Prelude.Prelude__CREATE_Lb_Comma_Rb_(root.NewNode(), Debug_DOT_Profile__CREATE_GarbageCollections(root.NewNode()), gc)
    
    // return
    list := Prelude.ListCreate(root.NewNode(), elapsed_node, sys_node, stack_node, heap_node, gc_node)
    gocurry.IOCreate(root, list)
}

func ExternalDebugDot_ProfileDot_garbageCollectorOff(task *gocurry.Task){
    root := task.GetControl()
    
    debug.SetGCPercent(-1)
    
    gocurry.IOCreate(root, Prelude.Prelude__CREATE_Lb_Rb_(root.NewNode()))
}

func ExternalDebugDot_ProfileDot_garbageCollectorOn(task *gocurry.Task){
    root := task.GetControl()
    
    debug.SetGCPercent(100)
    
    gocurry.IOCreate(root, Prelude.Prelude__CREATE_Lb_Rb_(root.NewNode()))
}

func ExternalDebugDot_ProfileDot_garbageCollect(task *gocurry.Task){
    root := task.GetControl()
    
    runtime.GC()
    
    gocurry.IOCreate(root, Prelude.Prelude__CREATE_Lb_Rb_(root.NewNode()))
}
