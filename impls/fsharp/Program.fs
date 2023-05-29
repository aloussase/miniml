open System

module Option =
    let fromBoolean b = if b then Some() else None

    let tryParseInt (s: string) =
        let (ok, value) = Int32.TryParse s
        if ok then Some value else None


[<EntryPoint>]
let main argv =
    let port =
        Option.fromBoolean (argv.Length > 0)
        |> Option.bind (fun _ -> Option.tryParseInt argv.[0])
        |> Option.defaultValue 3669

    Server.serve { Port = port }
    0
