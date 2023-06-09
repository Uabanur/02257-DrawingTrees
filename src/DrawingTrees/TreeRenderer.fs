module TreeRenderer

open TreeDesigner
open Renderer
open Config

let getPositions trees =
    List.map (fun (Node((_,p),_)) -> p) trees

let parseLabel lbl =
    // todo: choose amount of lines based on config
    let maxLines = 2
    let maxLineLength = 10

    let takeMax limit list =
        List.take (min limit (List.length list)) list

    string lbl
        |> StringF.replace "\n" "<br>"
        |> StringF.split "<br>"
        |> List.filter StringF.notWhitespace
        |> takeMax maxLines
        |> List.map (StringF.truncate maxLineLength)

let getRendering config tree =
    let rec helper level (xOffset:Position) (Node((label, position), subtrees)) =
        let pointColor = Color.Black // todo get from config
        let lineColor = Color.Black // todo get from config

        let nodeX = position + xOffset
        let nodePoint = point (nodeX, level) (parseLabel label) pointColor
        let subTreePositions = List.map (fun (Node((_,p),_)) -> p + nodeX) subtrees
        if config.Mode = Alternative then
            let subTreeConnections = List.map (fun p -> line (nodeX, level) (p, level- 1.0) lineColor) subTreePositions
            let nodeChart = nodePoint :: subTreeConnections |> combineRenderings
            let subCharts = List.map (helper (level - 1.0) nodeX) subtrees
            nodeChart :: subCharts |> combineRenderings
        else
            let subTreeHorizontalConnections = if List.isEmpty subtrees then [] else [line (List.min subTreePositions, level - 0.2) (List.max subTreePositions, level - 0.2) lineColor]
            let subTreeVerticalConnections = List.map (fun p -> line (p, level - 0.2) (p, level - 1.0) lineColor) subTreePositions
            let nodeSubTreeConnection = if List.isEmpty subtrees then [] else [line (nodeX, level) (nodeX, level - 0.2) lineColor]
            let nodeChart = subTreeHorizontalConnections @ nodeSubTreeConnection @ subTreeVerticalConnections @ [nodePoint] |> combineRenderings
            let subCharts = List.map (helper (level - 1.0) nodeX) subtrees
            nodeChart :: subCharts |> combineRenderings
    helper 0 0.0 tree

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

let render (config: RenderConfig) (tree:Tree<'a * Position>) =
    let margin = 20.0 // percent, todo: get from config

    let marginScale = margin / 100.0
    let ((xmin,xmax),(ymin,ymax)) = getBounds tree
    let xRange = xmax - xmin
    let yRange = ymax - ymin
    let yMinMax = (ymin - yRange * marginScale, ymax + yRange * marginScale)
    let xMinMax = (xmin - xRange * marginScale, xmax + xRange * marginScale)

    printfn $"bounds: {(xmin,xmax)} {(ymin,ymax)}. yminmax: {yMinMax}. xminmax: {xMinMax}"
    getRendering config tree |> plot (xMinMax, yMinMax)
