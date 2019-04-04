open System

let input = 
    let r = "37442,16945 37209,16402 36950,15870 36664,15352 36353,14849 36018,14362 35658,13892 35276,13440 34872,13008 34447,12597 34002,12207 33539,11839 33057,11496 32560,11176 32047,10881 31520,10613 30980,10371 30429,10155 29868,9968 29298,9808 28721,9677 28139,9575 27552,9502 26962,9458 26370,9443 25779,9458 25189,9502 24602,9575 24019,9677 23443,9808 22873,9968 22312,10155 21761,10371 21221,10613 20694,10881 20181,11176 19683,11496 19202,11839 18738,12207 18293,12597 17868,13008 17464,13440 17082,13892 16723,14362 16388,14849 16077,15352 15791,15870 15532,16402 15299,16945"
    //r.Split([|" "|], StringSplitOptions.None)
    r.Split [|' '|] |> List.ofArray
    |> List.map (fun x -> match x.Split [|','|] |> List.ofArray with | x::y::[] -> x, y | x -> failwith "" )
    |> List.map (function x, y -> (Int32.Parse x), (Int32.Parse y))

let line (x1, y1) (x2, y2) =
    if y2 - y1 = 0 then (fun _ -> y1)
    elif x2 - x1 = 0 then (fun _ -> x1)
    else (fun x -> (x*y1-x*y2+x1*y2-x2*y1)/(-x2+x1))

let coord p1 p2 = 
    //let p1 = input.Head
    //let p2 = input.Tail.Head
    let f = line p1 p2
    let d = fst p1 - fst p2 |> abs

    let xs = 
        let (x1, _), (x2, _) = p1, p2
        [for i in List.min [x1; x2] .. 20 .. List.max [x1; x2] -> i]

    let ys = [for i in xs -> f i]
    List.zip xs ys
    //|> printfn "%A"

let split = [for i in 0..input.Length - 2 -> input.[i], input.[i+1]]

let pointDraw (x, y) = 
    "<circle
       style=\"fill:#ff0000;fill-opacity:1;stroke:#ff0000;stroke-opacity:1\"
       id=\"path4136\"
       cy=\"{0}\"
       cx=\"{1}\"
       r=\"17.675119\" />".Replace("{1}", x.ToString()).Replace("{0}", y.ToString())

let res (p1, p2) = 
    coord p1 p2
    |> List.map pointDraw |> List.reduce (fun first sec -> first + "\n" + sec)

let x = split |> List.map res |> List.reduce (fun first sec -> first + "\n" + sec)

printfn "DONE!" 
Console.ReadKey() |> ignore