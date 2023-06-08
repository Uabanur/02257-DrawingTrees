module Renderer

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

type Rendering = R of GenericChart.GenericChart

let point xy label color =
    Chart.Point([xy], 
        Text= StringF.join "<br>" label, 
        TextPosition=StyleParam.TextPosition.TopCenter, 
        ShowLegend = false, 
        MarkerColor = mapColor color
    ) |> R;

let line (x1,y1) (x2,y2) color =
    Chart.Line([x1;x2], [y1;y2], 
        LineColor = mapColor color,
        ShowLegend = false
    ) |> R;

let combineRenderings renderings = 
  renderings |> Seq.map (fun (R(c)) -> c) |> Chart.combine |> R


let plot (xMinMax, yMinMax) (R(chart)) =
    let plainAxis minmax = LinearAxis.init(
        ShowLine = false,
        ShowGrid = false,
        ZeroLine = false,
        ShowBackground = false,
        ShowTickLabels = false,
        Range = StyleParam.Range.MinMax(minmax)
    )

    chart
      |> Chart.withXAxis (plainAxis xMinMax)
      |> Chart.withYAxis (plainAxis yMinMax)
      |> Chart.withLayoutStyle(PlotBGColor=Color.fromKeyword ColorKeyword.GhostWhite)
      |> Chart.show
