package Data_DOT_Time

import "gocurry"
import "time"

func ExternalDataDot_TimeDot_getClockTime(task *gocurry.Task){
    root := task.GetControl()
    
    // save unix time as IntLit
    cTime := gocurry.IntLitCreate(root.NewNode(), int(time.Now().Unix()))
    
    gocurry.IOCreate(root, Data_DOT_Time__CREATE_CTime(root.NewNode(), cTime))
}

func ExternalDataDot_TimeDot_primUs_toCalendarTime(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    // get unix time
    cTimeUnix := x1.GetChild(0).GetInt()
    
    // convert to go Time
    cTime := time.Unix(int64(cTimeUnix), 0)
    _, timeZone := cTime.Zone()
    
    // convert go Time to CalendarTime components
    year := gocurry.IntLitCreate(root.NewNode(), cTime.Year())
    month := gocurry.IntLitCreate(root.NewNode(), int(cTime.Month()))
    day := gocurry.IntLitCreate(root.NewNode(), cTime.Day())
    hour := gocurry.IntLitCreate(root.NewNode(), cTime.Hour())
    minute := gocurry.IntLitCreate(root.NewNode(), cTime.Minute())
    second := gocurry.IntLitCreate(root.NewNode(), cTime.Second())
    zone := gocurry.IntLitCreate(root.NewNode(), timeZone)
    
    // return CalendarTime
    gocurry.IOCreate(root, Data_DOT_Time__CREATE_CalendarTime(root.NewNode(), year, month, day, hour, minute, second, zone))
}

func ExternalDataDot_TimeDot_primUs_toUTCTime(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    // get unix time
    cTimeUnix := x1.GetChild(0).GetInt()
    
    // convert to go Time in UTC
    cTime := time.Unix(int64(cTimeUnix), 0).UTC()
    _, timeZone := cTime.Zone()
    
    // convert go Time to CalendarTime components
    year := gocurry.IntLitCreate(root.NewNode(), cTime.Year())
    month := gocurry.IntLitCreate(root.NewNode(), int(cTime.Month()))
    day := gocurry.IntLitCreate(root.NewNode(), cTime.Day())
    hour := gocurry.IntLitCreate(root.NewNode(), cTime.Hour())
    minute := gocurry.IntLitCreate(root.NewNode(), cTime.Minute())
    second := gocurry.IntLitCreate(root.NewNode(), cTime.Second())
    zone := gocurry.IntLitCreate(root.NewNode(), timeZone)
    
    // return CalendarTime
    Data_DOT_Time__CREATE_CalendarTime(root, year, month, day, hour, minute, second, zone)
}

func ExternalDataDot_TimeDot_primUs_toClockTime(task *gocurry.Task){
    root := task.GetControl()
    x1 := root.GetChild(0)
    
    // get time components from CalendarTime
    year := x1.GetChild(0).GetInt()
    month := time.Month(x1.GetChild(1).GetInt())
    day := x1.GetChild(2).GetInt()
    hour := x1.GetChild(3).GetInt()
    minute := x1.GetChild(4).GetInt()
    second := x1.GetChild(5).GetInt()
    zone := x1.GetChild(6).GetInt()
    
    // create go Time from components
    calTime := time.Date(year, month, day, hour, minute, second, 0, time.FixedZone("", zone))
    
    // convert to unix time
    cTime := gocurry.IntLitCreate(root.NewNode(), int(calTime.Unix()))
    
    //return result
    Data_DOT_Time__CREATE_CTime(root, cTime)
}
