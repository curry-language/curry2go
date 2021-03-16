package SystemCPUTime

import "gocurry"
import "syscall"


func ExternalSystem_CPUTime_getCPUTime(task *gocurry.Task){
    root := task.GetControl()
    
    usage := new(syscall.Rusage)
    syscall.Getrusage(syscall.RUSAGE_SELF, usage)
    
    user_time := usage.Utime.Sec*1e3 + usage.Utime.Usec/1e3
    system_time := usage.Stime.Sec*1e3 + usage.Stime.Usec/1e3
    total_time := user_time + system_time
    
    gocurry.IOCreate(root, gocurry.IntLitCreate(root.NewNode(), int(total_time)))
}

func ExternalSystem_CPUTime_getElapsedTime(task *gocurry.Task){
    panic("System.CPUTime.getElapsedTime: not yet implemented")
}
