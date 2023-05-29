module Server

open System
open System.Net.Sockets
open System.Net
open System.Text


type ServerOptions = { Port: int }

let private evalProgram program =
    let lexer = Lexer.init program
    let tokens = [ for token in Lexer.lex lexer -> token.ToString() ]
    String.Join(" ", tokens)

let private handleClient (client: TcpClient) =
    let stream = client.GetStream()
    let buffer = Array.zeroCreate 1024

    let rec loop () =
        let bytesRead = stream.Read(buffer, 0, buffer.Length)

        if bytesRead = 0 then
            ()
        else
            let program = Encoding.UTF8.GetString buffer
            let result = evalProgram program
            let resultBytes = Encoding.UTF8.GetBytes result
            stream.Write(resultBytes, 0, resultBytes.Length)
            stream.Flush()
            loop ()

    loop ()


let serve serverOptions =
    let listener = new TcpListener(IPAddress.Any, serverOptions.Port)
    listener.Start()

    while true do
        let client = listener.AcceptTcpClient()
        handleClient client
        client.Close()

    listener.Stop()
