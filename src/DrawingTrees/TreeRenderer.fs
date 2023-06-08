module TreeRenderer

open TreeDesigner
open Plotly.NET
open Plotly.NET.LayoutObjects // this namespace contains all object abstractions for layout styling

type Color = 
  | Black
  | Blue | BlueDark | BlueLight
  | Brown
  | Cyan | CyanDark | CyanLight
  | Grey | GreyDark | GreyLight
  | Green | GreenDark | GreenLight
  | Magenta
  | Orange | OrangeDark
  | Pink
  | Purple
  | Red | RedDark
  | Violet
  | White
  | Yellow
  | RGB of int * int * int
  | ARGB of int * int * int * int

let mapColor (c:Color) :Plotly.NET.Color =
    match c with 
    | Black -> Color.fromKeyword ColorKeyword.Black
    | Blue -> Color.fromKeyword ColorKeyword.Blue
    | BlueDark -> Color.fromKeyword ColorKeyword.DarkBlue
    | BlueLight -> Color.fromKeyword ColorKeyword.LightBlue
    | Brown -> Color.fromKeyword ColorKeyword.Brown
    | Cyan -> Color.fromKeyword ColorKeyword.Cyan
    | CyanDark -> Color.fromKeyword ColorKeyword.DarkCyan
    | CyanLight -> Color.fromKeyword ColorKeyword.LightCyan
    | Grey -> Color.fromKeyword ColorKeyword.Grey
    | GreyDark -> Color.fromKeyword ColorKeyword.DarkGrey
    | GreyLight -> Color.fromKeyword ColorKeyword.LightGrey
    | Green -> Color.fromKeyword ColorKeyword.Green
    | GreenDark -> Color.fromKeyword ColorKeyword.DarkGreen
    | GreenLight -> Color.fromKeyword ColorKeyword.LightGreen
    | Magenta -> Color.fromKeyword ColorKeyword.Magenta
    | Orange -> Color.fromKeyword ColorKeyword.Orange
    | OrangeDark -> Color.fromKeyword ColorKeyword.DarkOrange
    | Pink -> Color.fromKeyword ColorKeyword.Pink
    | Purple -> Color.fromKeyword ColorKeyword.Purple
    | Red -> Color.fromKeyword ColorKeyword.Red
    | RedDark -> Color.fromKeyword ColorKeyword.DarkRed
    | Violet -> Color.fromKeyword ColorKeyword.Violet
    | White -> Color.fromKeyword ColorKeyword.White
    | Yellow -> Color.fromKeyword ColorKeyword.Yellow
    | RGB(r,g,b) -> Color.fromRGB r g b
    | ARGB(a,r,g,b) -> Color.fromARGB a r g b


let getPositions trees =
    List.map (fun (Node((_,p),_)) -> p) trees

let parseLabel lbl = 
    let split (delimiter:string) (source:string) : string list = 
        source.Split(delimiter) |> Array.toList

    let replace (pattern:string) (substitution:string) (source: string) :string =
        source.Replace (pattern, substitution)

    let characters (source:string) :string list =
        source.ToCharArray() |> Array.map string |> Array.toList

    let join (delimiter:string) (sources: string list) : string = 
        System.String.Join(delimiter, (List.toArray sources))

    let takeMax limit list =
        List.take (min limit (List.length list)) list

    let notWhitespace (source: string) :bool = 
        System.String.IsNullOrWhiteSpace source |> not

    // todo: choose amount of lines based on config
    let maxLines = 2
    let maxLineLength = 10
    let lines = string lbl |> replace "\n" "<br>" |> split "<br>" |> List.filter notWhitespace

    let formatted = 
        takeMax maxLines lines
            |> List.map (fun line -> characters line |> takeMax maxLineLength |> join "")
            |> join "<br>"

    formatted

    
let point xy label =
    let color = Color.Black // todo: get from config
    Chart.Point([xy], 
        Text=parseLabel label, 
        TextPosition=StyleParam.TextPosition.TopCenter, 
        ShowLegend = false, 
        MarkerColor = mapColor color
    );

let line (x1,y1) (x2,y2) =
    let color = Color.Black // todo: get from config
    Chart.Line([x1;x2], [y1;y2], 
        LineColor = mapColor color,
        ShowLegend = false
    );

let getChart tree =
    let rec helper level (xOffset:Position) (Node((label, position), subtrees)) =
        let nodeX = position + xOffset
        let nodePoint = point (nodeX, level) label
        let subTreePositions = List.map (fun (Node((_,p),_)) -> p + nodeX) subtrees
        let subTreeConnections = List.map (fun p -> line (nodeX, level) (p, level-1)) subTreePositions
        let nodeChart = nodePoint :: subTreeConnections |> Chart.combine
        let subCharts = List.map (helper (level-1) nodeX) subtrees
        nodeChart :: subCharts |> Chart.combine
    in helper 0 0.0 tree

let getBounds (tree:Tree<'a*Position>) = 
    let verticalSpacing = 1.0 // todo: get from config
    let rec maxBounds ypos (Node((_, p),c)) = 
        let childBounds = List.map (maxBounds (ypos-verticalSpacing)) c
        let (xBounds, yBounds) = List.unzip childBounds
        // adding x pos to child positions to translate relative positions
        let xmin' = List.map fst xBounds |> List.map ((+)p) |> List.fold min p
        let xmax' = List.map snd xBounds |> List.map ((+)p) |> List.fold max p
        let ymin' = List.map fst yBounds |> List.fold min ypos
        let ymax' = List.map snd yBounds |> List.fold max ypos
        (xmin', xmax'), (ymin', ymax')
    in maxBounds 0 tree

let render (tree:Tree<'a * Position>) =
    let margin = 20.0 // percent, todo: get from config

    let plainAxis minmax = LinearAxis.init(
        ShowLine = false,
        ShowGrid = false,
        ZeroLine = false,
        ShowBackground = false,
        ShowTickLabels = false,
        Range = StyleParam.Range.MinMax(minmax)
    )

    let marginScale = margin / 100.0
    let ((xmin,xmax),(ymin,ymax)) = getBounds tree
    let xRange = xmax - xmin
    let yRange = ymax - ymin
    let yMinMax = (ymin - yRange * marginScale, ymax + yRange * marginScale)
    let xMinMax = (xmin - xRange * marginScale, xmax + xRange * marginScale)

    printfn $"bounds: {(xmin,xmax)} {(ymin,ymax)}. yminmax: {yMinMax}. xminmax: {xMinMax}"
    getChart tree
    |> Chart.withXAxis (plainAxis xMinMax)
    |> Chart.withYAxis (plainAxis yMinMax)
    |> Chart.withLayoutStyle(PlotBGColor=Color.fromKeyword ColorKeyword.GhostWhite)
    |> Chart.show
