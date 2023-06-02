module TreeRenderer

open TreeDesigner
open Plotly.NET
open Plotly.NET.LayoutObjects // this namespace contains all object abstractions for layout styling
open System.Drawing

// TODO Add white background to the labels

let getPositions trees =
    List.map (fun (Node((_,p),_)) -> p) trees

let point xy label =
    Chart.Point(xy=[xy], MultiText=[label], MultiTextPosition=[StyleParam.TextPosition.TopCenter], ShowLegend = false);

let line (x1,y1) (x2,y2) =
    Chart.Line([x1;x2], [y1;y2], LineColor = Color.fromString "black", ShowLegend = false);

let getChart tree =
    let rec helper level (xOffset:Position) (Node((label, position), subtrees)) =
        let nodeX = position + xOffset
        let nodePoint = point (nodeX, level) label
        let subTreePositions = List.map (fun (Node((_,p),_)) -> p + nodeX) subtrees
        let subTreeHorizontalConnections = List.map (fun p -> line (nodeX, level - 0.1) (p, level - 0.1)) subTreePositions
        let subTreeVerticalConnections = List.map (fun p -> line (p, level - 0.1) (p, level - 1.0)) subTreePositions
        let nodeSubTreeConnection = if List.isEmpty subtrees then [] else [line (nodeX, level) (nodeX, level - 1.0)]
        let nodeChart = nodePoint :: nodeSubTreeConnection @ subTreeHorizontalConnections @ subTreeVerticalConnections |> Chart.combine
        let subCharts = List.map (helper (level - 1.0) nodeX) subtrees
        nodeChart :: subCharts |> Chart.combine
    in helper 0 0.0 tree

let render (tree:Tree<'a * Position>) =
    let plainAxis = LinearAxis.init(ShowLine = false, Mirror = StyleParam.Mirror.False, ShowGrid = false)

    getChart tree
    |> Chart.withXAxis plainAxis
    |> Chart.withYAxis plainAxis
    |> Chart.show
