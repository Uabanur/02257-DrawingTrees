module Renderer

open Plotly.NET
open Plotly.NET.LayoutObjects // this namespace contains all object abstractions for layout styling
open Config
open Color

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


let plot config (xMinMax, yMinMax) (R(chart)) =
    let background = config.BackgroundColor
    let dimensions = (config.Width, config.Height)

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
      |> Chart.withLayoutStyle(PlotBGColor=mapColor background, Width = fst dimensions, Height = snd dimensions)
      |> Chart.show
