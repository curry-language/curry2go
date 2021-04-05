package SystemDirectory

import "gocurry"
import "os"
import "../../Prelude"
import "../../Data/Time"

func ExternalSystem_Directory_prim_doesFileExist(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    // get file info
    path := gocurry.ReadString(x1)
    info, err := os.Stat(path)
    
    if(os.IsNotExist(err) || info.IsDir()){
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_False(root.NewNode()))
    } else if(err != nil){
        panic("System.Directory.doesFileExist: " + err.Error())
    } else{
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_True(root.NewNode()))
    }
}

func ExternalSystem_Directory_prim_doesDirectoryExist(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    // get file info
    path := gocurry.ReadString(x1)
    info, err := os.Stat(path)
    
    if(os.IsNotExist(err) || !info.IsDir()){
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_False(root.NewNode()))
    } else if(err != nil){
        panic("System.Directory.doesDirectoryExist: " + err.Error())
    } else{
        gocurry.IOCreate(root, Prelude.Prelude__CREATE_True(root.NewNode()))
    }
}

func ExternalSystem_Directory_prim_fileSize(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    // get file info
    path := gocurry.ReadString(x1)
    info, err := os.Stat(path)
    
    if(err != nil){
        if(os.IsNotExist(err)){
            panic("System.Directory.fileSize: file " + path + " does not exist")
        } else{
            panic("System.Directory.fileSize: cannot load file " + path + ". " + err.Error())
        }
    }
    
    // return file size
    sizeNode := gocurry.IntLitCreate(root.NewNode(), int(info.Size()))
    gocurry.IOCreate(root, sizeNode)
}

func ExternalSystem_Directory_prim_getModificationTime(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    // get file info
    path := gocurry.ReadString(x1)
    info, err := os.Stat(path)
    
    if(err != nil){
        if(os.IsNotExist(err)){
            panic("System.Directory.getModificationTime: file " + path + " does not exist")
        } else{
            panic("System.Directory.getModificationTime: cannot load file " + path + ". " + err.Error())
        }
    }
    
    // return modification time
    modNode := gocurry.IntLitCreate(root.NewNode(), int(info.ModTime().Unix()))
    gocurry.IOCreate(root, DataTime.DataTime__CREATE_CTime(root.NewNode(), modNode))
}

func ExternalSystem_Directory_getCurrentDirectory(task *gocurry.Task){
    root := task.GetControl()
    
    dir, err := os.Getwd()
    
    if(err != nil){
        panic("System.Directory.getCurrentDirectory: cannot load current directory")
    }
    
    gocurry.IOCreate(root, gocurry.StringCreate(root.NewNode(), dir))
}

func ExternalSystem_Directory_prim_setCurrentDirectory(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    // get directory info
    path := gocurry.ReadString(x1)
    info, err := os.Stat(path)
    
    if(os.IsNotExist(err) || !info.IsDir()){
        panic("System.Directory.setCurrentDirectory: directory " + path + " does not exist")
    } else if(err != nil){
        panic("System.Directory.setCurrentDirectory: cannot load directory " + path + ". " + err.Error())
    }
    
    // change working directory
    err = os.Chdir(path)
    
    if(err != nil){
        panic("System.Directory.setCurrentDirectory: " + err.Error())
    }
    
    // return
    gocurry.IOCreate(root, Prelude.Prelude__CREATE_LbRb(root.NewNode()))
}

func ExternalSystem_Directory_prim_getDirectoryContents(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    // get directory info
    path := gocurry.ReadString(x1)
    info, err := os.Stat(path)
    
    if(os.IsNotExist(err) || !info.IsDir()){
        panic("System.Directory.getDirectoryContents: directory " + path + " does not exist")
    } else if(err != nil){
        panic("System.Directory.getDirectoryContents: cannot load directory " + path + ". " + err.Error())
    }
    
    // open directory
    file, fErr := os.Open(path)
    
    if(fErr != nil){
        panic("System.Directory.getDirectoryContents: " + fErr.Error())
    }
    
    
    // load directory contents
    names, nErr := file.Readdirnames(0)
    
    if(nErr != nil){
        panic("System.Directory.getDirectoryContents: cannot load directory contents for " + path)
    }
    
    // create result list
    result := make([]*gocurry.Node, len(names))
    for i := 0; i<len(names); i++{
        result[i] = gocurry.StringCreate(root.NewNode(), names[i])
    }
    
    // return result
    gocurry.IOCreate(root, Prelude.ListCreate(root.NewNode(), result...))
}

func ExternalSystem_Directory_prim_createDirectory(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    path := gocurry.ReadString(x1)
    err := os.Mkdir(path, os.ModePerm)
    
    if(err != nil){
        panic("System.Directory.createDirectory: " + err.Error())
    }
    
    gocurry.IOCreate(root, Prelude.Prelude__CREATE_LbRb(root.NewNode()))
}

func ExternalSystem_Directory_prim_removeDirectory(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    path := gocurry.ReadString(x1)
    err := os.RemoveAll(path)
    
    if(err != nil){
        panic("System.Directory.removeDirectory: " + err.Error())
    }
    
    gocurry.IOCreate(root, Prelude.Prelude__CREATE_LbRb(root.NewNode()))
}

func ExternalSystem_Directory_prim_renameDirectory(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)
    
    // get directory info
    path := gocurry.ReadString(x1)
    info, err := os.Stat(path)
    
    if(os.IsNotExist(err) || !info.IsDir()){
        panic("System.Directory.renameDirectory: directory " + path + " does not exist")
    } else if(err != nil){
        panic("System.Directory.renameDirectory: cannot load directory " + path + ". " + err.Error())
    }
    
    // rename directory
    name := gocurry.ReadString(x2)
    err = os.Rename(path, name)
    
    if(err != nil){
        panic("System.Directory.renameDirectory: " + err.Error())
    }
    
    // return
    gocurry.IOCreate(root, Prelude.Prelude__CREATE_LbRb(root.NewNode()))
}

func ExternalSystem_Directory_prim_removeFile(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    path := gocurry.ReadString(x1)
    
    err := os.Remove(path)
    
    if(err != nil){
        panic("System.Directory.removeFile: " + err.Error())
    }
    
    gocurry.IOCreate(root, Prelude.Prelude__CREATE_LbRb(root.NewNode()))
}

func ExternalSystem_Directory_prim_renameFile(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    x2 := root.GetChild(1)
    
    // get file info
    path := gocurry.ReadString(x1)
    info, err := os.Stat(path)
    
    if(os.IsNotExist(err) || info.IsDir()){
        panic("System.Directory.renameDirectory: file " + path + " does not exist")
    } else if(err != nil){
        panic("System.Directory.renameDirectory: cannot load file " + path + ". " + err.Error())
    }
    
    // rename file
    name := gocurry.ReadString(x2)
    err = os.Rename(path, name)
    
    if(err != nil){
        panic("System.Directory.renameDirectory: " + err.Error())
    }
    
    // return
    gocurry.IOCreate(root, Prelude.Prelude__CREATE_LbRb(root.NewNode()))
}
