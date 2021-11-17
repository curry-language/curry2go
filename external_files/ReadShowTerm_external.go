package ReadShowTerm

import "strings"
import "runtime"
import "gocurry"
import "path/filepath"
import "curry2go/Prelude"

func ExternalReadShowTermDot_primUs_showTerm(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    stringRep := gocurry.ShowResult(x1)
    
    gocurry.StringCreate(root, stringRep)
}

func ExternalReadShowTermDot_primUs_readsUnqualifiedTerm(task *gocurry.Task){
    root := task.GetControl()
    cModList := gocurry.ReadList(root.GetChild(0))
    term := gocurry.ReadString(root.GetChild(1))
    
    // get directory where compiled modules are saved
    _, modDir, _, _ := runtime.Caller(0)
    modDir = filepath.Dir(filepath.Dir(modDir))
    
    // get the path to the go file for every module in cModList
    modList := make([]string, len(cModList))
    for i := range(cModList){
        modList[i] = strings.ReplaceAll(gocurry.ReadString(cModList[i]), ".", string(filepath.Separator))
        modList[i] = filepath.Join(modDir, modList[i])
        modList[i] = filepath.Join(modList[i], filepath.Base(modList[i]) + ".go")
        
    }
    
    // parse the term and return result
    parseResult, restString := gocurry.ReadUQTerm(root.NewNode(), term, modList)
    tupel := Prelude.Prelude__CREATE_Lb_Comma_Rb_(root.NewNode(), parseResult, restString)
    Prelude.ListCreate(root, tupel)
}
