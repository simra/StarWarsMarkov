// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open ParseScripts
open System
open System.Text.RegularExpressions

let train str =
    Regex.Split(str,@"\s+")
    |> Seq.windowed 2
    |> Seq.map (fun s -> String.concat " " s)
    |> Seq.windowed 2
    |> Seq.map (fun a -> (a.[0],a.[1]))
    |> Seq.countBy id
    |> Seq.groupBy (fun ((a,b),c) -> a)
    |> Map.ofSeq

let rnd=new Random()
    
let query (m:Map<string,((string*string)*int) seq>) (qq:string) =
    let choose (s:((string*string)*int) seq) : (string*string)*int =
        let l=rnd.Next(Seq.length s)
        Seq.skip l s
        |> Seq.take 1
        |> Seq.exactlyOne

    
    Seq.unfold (fun (q,i) -> 
        if i=0 then None
        else if Map.containsKey q m then 
            let ((a,b),c)=choose m.[q]
            Some(b,(b,i-1))
        else None // What to do?
        ) (qq,100)

[<EntryPoint>]
let main argv = 
    let m = 
        ParseScripts.scriptUrls
        |> List.map (fun (t,u) -> (ParseScripts.getScript u))    
        |> Seq.map (fun x -> string x)
        |> String.concat "\n"
        |> train

    
    //printfn "%A" m
    //let lookup = "your father"
    let lookup = "LEIA </b>"
    
    while (true) do
        printfn "Enter a two-token seed. eg \"Help me\""
        let l=Console.ReadLine()
        if l.Split(' ').Length<>2 then 
            eprintfn "Query must be two tokens in length"
        else             
            //query m lookup
            query m l
           // |> Seq.map (fun ((a,b),c) -> a)
            |> Seq.map (fun s -> if s.Contains(" ") then s.Split(' ').[1] else s)
            |> String.concat " "
            |> fun s -> l + " "+s
            |> printfn "%s"
    0 // return an integer exit code
