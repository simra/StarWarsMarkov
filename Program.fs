open ParseScripts
open System
open System.IO
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

let randomSeed m =
    Map.toSeq m
    |> Seq.map (fun (s,_)->s)
    |> Array.ofSeq
    |> fun a -> a.[rnd.Next(a.Length)]

let query (m:Map<string,((string*string)*int) seq>) (qq:string) =
    let choose (s:((string*string)*int) seq) : (string*string)*int =
        let l=rnd.Next(Seq.length s)
        Seq.skip l s
        |> Seq.take 1
        |> Seq.exactlyOne

    let qq'= 
        if qq="rand" then
            randomSeed m
        else qq

    Seq.unfold (fun (q,i) -> 
        if i=0 then None
        else if Map.containsKey q m then 
            let ((a,b),c)=choose m.[q]
            Some(b,(b,i-1))
        else None // What to do?
        ) (qq',100)
        |> Seq.map (fun s -> if s.Contains(" ") then s.Split(' ').[1] else s)
        |> String.concat " "
        |> fun s -> qq'+" "+s
            
// seed the training with additional corpi: pass them as filenames on the command line (text only)  
// Pass them multiple times to give them more weight (TODO: make this configurable)
let ParseArgs (argv:string[]) =
    [0..argv.Length-1]
    |> Seq.map (fun x -> 
            eprintfn "Reading %s" argv.[x]
            File.ReadAllLines(argv.[x]))
    |> Seq.map (fun lines -> lines|>Seq.takeWhile (fun s -> not (s.StartsWith("*** END OF THIS PROJECT GUTENBERG EBOOK"))))
    |> Seq.concat
    |> String.concat "\n"

[<EntryPoint>]
let main argv = 
    let m = 
        [ ParseScripts.scriptUrls
            |> List.map (fun (t,u) -> (ParseScripts.getScript u))    
            |> Seq.map (fun x -> string x)
            |> Seq.map (fun x -> x.Replace("<b>","").Replace("</b>",""))
            |> String.concat "\n";
          ParseArgs argv]
        |> String.concat "\n"
        |> train

    
    //printfn "%A" m
    //let lookup = "your father"
    let lookup = "LEIA </b>"
    
    while (true) do
        printfn "Enter a two-token seed: eg \"the Force\", or \"rand\" to pick a random seed."
        let l=Console.ReadLine()
        if l<>"rand" && l.Split(' ').Length<>2 then 
            eprintfn "Query must be two tokens in length"
        else             
            //query m lookup
            query m l
           // |> Seq.map (fun ((a,b),c) -> a)
            |> printfn "%s"
    0 // return an integer exit code
