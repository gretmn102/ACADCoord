module ImportDFX

open System
open System.IO
open netDxf

let circle (cx, cy) r = 
    (fun t -> 
        let t = t * (Math.PI / 180.0)
        r * cos(t) + cx, r * sin(t) + cy)

let circleP (arc:Entities.Arc) = 
    let c = circle (arc.Center.X, arc.Center.Y) arc.Radius
    c arc.StartAngle, c arc.EndAngle

module DxfToSvgFormat =
    let private commaRep (s:string) = s.Replace(",", ".")
    let private p (x, y) = 
        let inverse y = 162.0 * 2.0 - y // Вращение вокруг точки. 162.97350616 - центр (по y)
        commaRep (x.ToString()) + "," + commaRep (y.ToString()) // ((inverse y).ToString())

    let arc (arc:Entities.Arc) = 
        let format s r e =
            String.Format(
                "<path
                       style=\"stroke:#000000;fill:none\"
                       d=\"M {0} A {1},{1} 0 0 0 {2}\"
                       id=\"path4160\" />", p s, commaRep (r.ToString()), p e)
        let (s, e) = 
            let c = circle (arc.Center.X, arc.Center.Y) arc.Radius
            c arc.StartAngle, c arc.EndAngle
        format s arc.Radius e

    let line (l:Entities.Line) = 
            String.Format(     
                "<path
                   style=\"stroke:#000000;fill:none\"
                   d=\"M {0} {1}\"
                   id=\"path4560\" />", p (l.StartPoint.X, l.StartPoint.Y), p (l.EndPoint.X, l.EndPoint.Y))
    let point (x, y) =
            "<circle
               style=\"fill:#ff0000;fill-opacity:1;stroke:#ff0000;stroke-opacity:1\"
               id=\"path4136\"
               cy=\"{0}\"
               cx=\"{1}\"
               r=\"0.1\" />".Replace("{1}", x.ToString()).Replace("{0}", y.ToString())

let d = DxfDocument.Load @"Dxf\dnische_gaza2013.dxf"

///<summary> Собрать все отрезки и дуги в строку из узлов svg. </summary>
let str () =
    let fold = Seq.fold (fun state x -> state + "\n" + x) ""
    let arcs = d.Arcs |> Seq.map DxfToSvgFormat.arc |> fold
    let lines =  d.Lines |> Seq.map DxfToSvgFormat.line |> fold
    arcs + "\n" + lines

let s = 
    let s1 = d.Arcs |> Seq.map box
    let s2 = d.Lines |> Seq.map box
    Seq.append s1 s2 |> List.ofSeq

type Z (points : (float*float) list) = 
    let d (Ax, Ay) (Bx, By) = sqrt( (Ax - Bx)**2.0 + (Ay - By)**2.0 )
    member this.P = points
    override this.Equals(o : obj) =
        match o with
        | :? Z as other -> 
            let f func L1 L2 = 
                let rec f' l =
                    if List.isEmpty l then seq[]
                    else seq[ yield! List.map (func (Seq.head l)) L2; yield! f' (List.tail l) ]
                in f' L1
            let s = f d points (other.P)
            Seq.exists (fun x -> x < 1.0) s
        | _ -> false
    override this.GetHashCode() = failwith "hashcode not implemented"
    override this.ToString() = points.ToString()

let points = 
    s 
    |> Seq.mapi (fun i x ->
        match x with
        | :? Entities.Arc as a -> 
                            let (A, B) = circleP a
                            [A; B], i
        | :? Entities.Line as l -> 
                            [(l.StartPoint.X, l.StartPoint.Y); (l.EndPoint.X, l.EndPoint.Y)], i
        | _ -> failwithf "нечто неопознанное")
    |> List.ofSeq
    |> List.map (function lst, i -> new Z(lst), i)

let group = 
    points
    |> Seq.groupBy (fun x -> fst x)
    |> List.ofSeq
    |> List.map (fun x -> 
            let f = snd x
            f |> Seq.map snd |> List.ofSeq)
    |> List.map (fun x -> x |> List.map (fun x -> s.[x]))
module Draw = 
    let private lineEq ((x1:float), (y1:float)) ((x2:float), (y2:float)) =
        if y2 - y1 = 0.0 then (fun _ -> y1)
        elif x2 - x1 = 0.0 then (fun _ -> x1)
        else (fun (x:float) -> (x*y1-x*y2+x1*y2-x2*y1)/(-x2+x1))
    
    let polyline start input func = 
        let coord p1 p2 func  = 
            let f = func p1 p2
            let d = fst p1 - fst p2 |> abs

            let xs = 
                let (x1, _), (x2, _) = p1, p2
                [for i in List.min [x1; x2] .. 20.0 .. List.max [x1; x2] -> i]

            let ys = [for i in xs -> f i]
            List.zip xs ys

        let res func (p1, p2) = 
            coord p1 p2 func
            |> List.map (DxfToSvgFormat.point) |> List.reduce (fun first sec -> first + "\n" + sec)

        let start input func = 
            let split = [for i in 0..List.length input - 2 -> input.[i], input.[i+1]]
            split |> List.map (res func) |> List.reduce (fun first sec -> first + "\n" + sec)
        start input func

    let circle (arc:Entities.Arc) =
        let getcoords t1 t2 f (s:float) = 
            let xs = [List.min [t1; t2] .. s .. List.max [t1; t2]]
            xs |> List.map f
        
        let circleF (arc:Entities.Arc) =
            let f = circle (arc.Center.X, arc.Center.Y) arc.Radius
            let (s, e) = arc.StartAngle, arc.EndAngle
            getcoords s e f 1.0
        let c = circleF arc
        c

    let line (l:Entities.Line) =
        let getcoords t1 t2 f (s:float) = 
            let xs = [List.min [t1; t2] .. s .. List.max [t1; t2]]
            let ys = xs |> List.map f
            List.zip xs ys
        let f = lineEq (l.StartPoint.X, l.StartPoint.Y) (l.EndPoint.X, l.StartPoint.Y)
        getcoords l.StartPoint.X l.EndPoint.X f 1.0

let color = 
    let decToRgb n = 
        let rec f' (acc:int list) n i =
            match i with
            | 0 -> acc
            | i ->
                let m = n % 256
                f' (m::acc) (n/256) (i-1)
        f' [] n 3
    decToRgb 100

let coords = 
    let unpack (o:obj) = 
        match o with
        | :? Entities.Arc as a ->  Draw.circle a
        | :? Entities.Line as l -> Draw.line l
        | _ -> failwith "must be Arc or Line"

    let group = 
        let x = 
            group 
            |> List.map (List.map unpack >> List.concat >> List.sortBy fst)
        x |> List.sortBy ( List.maxBy snd >> snd) |> List.rev

    let transpose L = 
        let f1 x = List.map (function | hd::tl -> Some hd, tl | [] -> None, []) x
    
        let f2 x = 
            List.foldBack (fun x (ls, Ls) ->
                                match x with
                                | Some a, l -> Some a :: ls, l :: Ls
                                | None, l -> None :: ls, l :: Ls) x ([], [])
        let res = ref []
        let f3 (l, ls) = res := l :: !res; f1 ls
    
        let l1 = f1 L
        let rec f l = 
            if l |> List.exists (function _, x -> List.length x <> 0) then f (f3 (f2 l)) else l
        in f l1 |> ignore
        !res |> List.rev
            (*
        let l2 = f2 l1
        let l3 = f3 l2
        let l4 = f2 l3
        let l5 = f3 l4
        let l6 = f2 l5
        let l7 = f3 l6
        let l8 = f2 l7
        let l9 = f3 l8
        let l10 = f2 l9
        //!res |> List.rev
        *)

    let cat strSplit l = List.reduce (fun x1 x2 -> x1 + strSplit + x2) l
    transpose group
    |> List.map (List.map (function | Some(x, y) -> x.ToString() + "\t" + y.ToString() | None -> "\t"))
    |> List.map (cat "\t")
    |> cat "\n"

    

(*
    let l = Draw.circle d.Arcs.[0]
    
    l |> List.map (fun x -> (fst x).ToString() + "\t" + (snd x).ToString())
    //l |> List.map DxfToSvgFormat.point
    //|> List.reduce (fun state x -> state + "\n" + x)
    File.WriteAllLines("coords.txt", l)
    *)
    (*
    let f = List.map (DxfToSvgFormat.point) >> List.reduce (fun state x -> state + "\n" + x)
    Seq.take 10 group2
    |> List.ofSeq
    |> List.map f
    |> List.reduce (fun state x -> state + "\n" + x)
    *)
//let s = Draw.circle

File.WriteAllText("out.txt", coords)

printfn "DONE!"
Console.ReadKey() |> ignore
