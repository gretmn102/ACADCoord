module ImportDFX

#if INTERACTIVE
#r @"..\..\packages\netDXF\lib\net45\netDxf.dll"
#endif
open System
open System.IO
open netDxf

let circle (cx, cy) r =
    (fun t ->
        let t = t * (Math.PI / 180.0)
        r * cos(t) + cx, r * sin(t) + cy)

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

#if INTERACTIVE
let path = @"AutoCADCoords\AutoCADCoords\bin\Debug\net461\Dxf\dnische_gaza2013.dxf"
#else
let path = @"Dxf\dnische_gaza2013.dxf"
#endif
let d = DxfDocument.Load(path)

/// Собрать все отрезки и дуги в строку из узлов svg.
let str () =
    let fold = Seq.fold (fun state x -> state + "\n" + x) ""
    let arcs = d.Arcs |> Seq.map DxfToSvgFormat.arc |> fold
    let lines =  d.Lines |> Seq.map DxfToSvgFormat.line |> fold
    arcs + "\n" + lines
module Draw =
    let polyline start input func =
        let coord p1 p2 func  =
            let f = func p1 p2
            let d = fst p1 - fst p2 |> abs
            let step = 20.0
            let xs =
                let (x1, _), (x2, _) = p1, p2
                [ for i in min x1 x2 .. step .. max x1 x2 -> i ]
            let ys = List.map f xs
            List.zip xs ys

        let start input func =
            let res func (p1, p2) =
                coord p1 p2 func
                |> List.map DxfToSvgFormat.point
                |> List.reduce (sprintf "%s\n%s")
            [ for i in 0..List.length input - 2 -> input.[i], input.[i + 1]]
            |> List.map (res func)
            |> List.reduce (sprintf "%s\n%s")
        start input func

    let circle (arc:Entities.Arc) =
        let getcoords t1 t2 f (step:float) =
            [min t1 t2 .. step .. max t1 t2]
            |> List.map f

        let circleF (arc:Entities.Arc) =
            let f = circle (arc.Center.X, arc.Center.Y) arc.Radius
            let (s, e) = arc.StartAngle, arc.EndAngle
            getcoords s e f 1.0
        circleF arc

    let line (l:Entities.Line) =
        /// Уравнение прямой через две точки.
        let lineEq ((x1:float), (y1:float)) ((x2:float), (y2:float)) =
            if y2 - y1 = 0.0 then (fun _ -> y1)
            elif x2 - x1 = 0.0 then (fun _ -> x1)
            else
                let t1 = x1 * y2 - x2 * y1
                let t2 = x1 - x2
                let t3 = y1 - y2
                (fun (x:float) -> (x*t3 + t1) / t2)
        let step = 1.
        let getcoords t1 t2 f =
            let xs = [ min t1 t2 .. step .. max t1 t2]
            let ys = xs |> List.map f
            List.zip xs ys
        let f = lineEq (l.StartPoint.X, l.StartPoint.Y) (l.EndPoint.X, l.StartPoint.Y)
        getcoords l.StartPoint.X l.EndPoint.X f

// TODO: дан ряд двухмерных точек, из него нужно взять самую ближайшую к указанной точке.
type Z (points : (float*float) list) =
    member this.P = points
    override this.Equals(o : obj) =
        let distant (x, y) (x', y') =
            sqrt( pown (x - x') 2 + pown (y - y') 2 )
        // printfn "using equals"
        match o with
        | :? Z as other ->
            // Seq.collect (fun x -> Seq.map (distant x) other.P) points
            // |> Seq.exists (fun x -> x < 1.0)
            points
            |> List.exists (fun x ->
                List.exists (distant x >> fun x -> x < 1.0) other.P)
        | _ -> false
    override this.GetHashCode() =
        // this.P.GetHashCode()
        0
    override this.ToString() = points.ToString()

type T =
    | Arc of Entities.Arc
    | Line of Entities.Line

let group =
    let xs =
        let s1 =
            d.Arcs |> Seq.map Arc
        let s2 =
            d.Lines |> Seq.map Line
        Seq.append s1 s2
    let points =
        xs
        |> Seq.map (fun x ->
            match x with
            | Arc x ->
                let circleP (arc:Entities.Arc) =
                    let c = circle (arc.Center.X, arc.Center.Y) arc.Radius
                    c arc.StartAngle, c arc.EndAngle
                let (a, b) = circleP x
                [a; b]
            | Line l ->
                [ l.StartPoint.X, l.StartPoint.Y; l.EndPoint.X, l.EndPoint.Y ]
            |> fun points -> Z(points), x
        )
    points
    |> Seq.groupBy fst
    |> Seq.map (snd >> Seq.map snd >> List.ofSeq)
    |> List.ofSeq

let coords =
    let group =
        group
        |> List.map (
            List.collect (function
                | Arc a ->  Draw.circle a
                | Line l -> Draw.line l )
            >> List.sortBy fst )
        |> List.sortBy (List.maxBy snd >> snd)
        |> List.rev

    FsharpMyExtension.List.transposeOpt group
    |> List.map (
        List.collect (function
            | Some(x, y) ->
                // sprintf "%s\t%s" (x.ToString()) (y.ToString())
                [x.ToString(); y.ToString()]
            | None ->
                // "\t"
                ["\t"]
            )
        >> String.concat "\t")
    |> String.concat "\n"

File.WriteAllText("out.txt", coords)

// printfn "DONE!"
// Console.ReadKey() |> ignore
