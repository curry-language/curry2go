package Network_DOT_Socket

import "gocurry"
import "curry2go/Prelude"
import "curry2go/System/IO"
import "strconv"
import "net"
import "io"
import "time"

var Sockets []*net.TCPListener

var Socket_names []string = []string{"Socket"}

func SocketCreate(root *gocurry.Node, l *net.TCPListener)(*gocurry.Node){
    Sockets = append(Sockets, l)

    id := len(Sockets) - 1
    gocurry.ConstCreate(root, 0, 1, &Socket_names[0],
        gocurry.IntLitCreate(root.NewNode(), id))
    return root
}

func ExternalNetworkDot_SocketDot_primUs_listenOn(task *gocurry.Task){
    root := task.GetControl()
    port := root.GetChild(0).GetInt()
    addr := &net.TCPAddr{Port: port}
    
    listener, err := net.ListenTCP("tcp", addr)
    
    if(err != nil){
        panic("Network.Socket.listonOn: " + err.Error())
    }
    
    gocurry.IOCreate(root, SocketCreate(root.NewNode(), listener))
}

func ExternalNetworkDot_SocketDot_listenOnFresh(task *gocurry.Task){
    root := task.GetControl()
    addr := &net.TCPAddr{Port: 0}
    
    listener, err := net.ListenTCP("tcp", addr)
    
    if(err != nil){
        panic("Network.Socket.listonOnFresh: " + err.Error())
    }
    
    tcpAddr, _ := net.ResolveTCPAddr("tcp", listener.Addr().String())
    port := gocurry.IntLitCreate(root.NewNode(), tcpAddr.Port)
    sock := SocketCreate(root.NewNode(), listener)
    gocurry.IOCreate(root, Prelude.Prelude__CREATE_Lb_Comma_Rb_(root.NewNode(), port, sock))
}

func ExternalNetworkDot_SocketDot_primUs_socketAccept(task *gocurry.Task){
    root := task.GetControl()
    sockNode := root.GetChild(0)
    sockId := sockNode.GetChild(0).GetInt()
    
    // accept connection
    conn, err := Sockets[sockId].Accept()
    
    if(err != nil){
        panic("Network.Socket.socketAccept: " + err.Error())
    }
    
    // create handle
    reader := io.ReadCloser(conn)
    writer := io.WriteCloser(conn)
    handle := System_DOT_IO.HandleCreate(root.NewNode(), &reader, &writer, 3, false)
    
    // return
    id := gocurry.StringCreate(root.NewNode(), conn.RemoteAddr().String())
    gocurry.IOCreate(root, Prelude.Prelude__CREATE_Lb_Comma_Rb_(root.NewNode(), id, handle))
}

func ExternalNetworkDot_SocketDot_primUs_waitForSocketAccept(task *gocurry.Task){
    root := task.GetControl()
    sockNode := root.GetChild(0)
    sockId := sockNode.GetChild(0).GetInt()
    timeout := root.GetChild(1).GetInt()
    
    // set deadline
    deadline := time.Now().Add(time.Duration(timeout) * time.Millisecond)
    Sockets[sockId].SetDeadline(deadline)
    
    // accept connection
    conn, err := Sockets[sockId].Accept()
    
    // reset deadline
    Sockets[sockId].SetDeadline(time.Time{})
    
    if(err != nil){
        // test for timeout
        if nErr, ok := err.(*net.OpError); ok && nErr.Timeout(){
            gocurry.IOCreate(root, Prelude.Prelude__CREATE_Nothing(root.NewNode()))
            return
        }
    
        panic("Network.Socket.socketAccept: " + err.Error())
    }
    
    // create handle
    reader := io.ReadCloser(conn)
    writer := io.WriteCloser(conn)
    handle := System_DOT_IO.HandleCreate(root.NewNode(), &reader, &writer, 3, false)
    
    // return
    id := gocurry.StringCreate(root.NewNode(), conn.RemoteAddr().String())
    gocurry.IOCreate(root, Prelude.Prelude__CREATE_Just(root.NewNode(), Prelude.Prelude__CREATE_Lb_Comma_Rb_(root.NewNode(), id, handle)))
}

func ExternalNetworkDot_SocketDot_primUs_sClose(task *gocurry.Task){
    root := task.GetControl()
    sockNode := root.GetChild(0)
    sockId := sockNode.GetChild(0).GetInt()
    
    err := Sockets[sockId].Close()
    
    if(err != nil){
        panic("Network.Socket.Close: " + err.Error())
    }
    
    gocurry.IOCreate(root, Prelude.Prelude__CREATE_Lb_Rb_(root.NewNode()))
}

func ExternalNetworkDot_SocketDot_primUs_connectToSocket(task *gocurry.Task){
    root := task.GetControl()
    host := gocurry.ReadString(root.GetChild(0))
    port := root.GetChild(1).GetInt()
    addrStr := host + ":" + strconv.Itoa(port)
    
    // resolve address
    addr, err := net.ResolveTCPAddr("tcp", addrStr)
    
    if(err != nil){
        panic("Network.Socket.connectToSocket: " + err.Error())
    }
    
    // dial connection
    conn, err := net.DialTCP("tcp", nil, addr)
    
    if(err != nil){
        panic("Network.Socket.connectToSocket: " + err.Error())
    }
    
    // create handle
    reader := io.ReadCloser(conn)
    writer := io.WriteCloser(conn)
    
    gocurry.IOCreate(root, System_DOT_IO.HandleCreate(root.NewNode(), &reader,  &writer, 3, false))
}
