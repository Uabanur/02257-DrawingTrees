module TreeExamples
open System

let rnd = new Random()
open TreeDesigner
let singleNode =
    Node("root", [])

let rec binaryTree height = 
    let height = max height 1
    match height with 
    | 1 -> Node(1, [])
    | n -> Node(n, [binaryTree (n-1); binaryTree (n-1)]) 

let parallelPaths paths height =
    let height = max height 1
    let rec pathOfLength = function | 0 -> Node(0, []) | n -> Node(n, [pathOfLength (n-1)])
    Node(height, seq {1..paths} |> Seq.map (fun _ -> pathOfLength (height-1)) |> Seq.toList)
