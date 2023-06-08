open TreeDesigner
open TreeRenderer
open System.Diagnostics

let runTests() =
  TreeDesignerChecks.runAll()
  TreeRendererChecks.runAll()

// let tree =
//     Node("r", [
//         Node("a", [
//             Node("aa", []);
//             Node("ab", []);
//             Node("ac", []);
//         ]);
//         Node("b", [
//             Node("ba", [
//                 Node("baa", []);
//                 Node("bab", []);
//                 Node("bac", []);
//             ]);
//         ]);
//         Node("c", []);
//     ])

// let tree =
//     Node
//       ('a',
//        [Node ('a', [Node ('a', []); Node ('a', [])]); Node ('a', [Node ('a', [])]);
//         Node ('a', [])])


let renderExamples() = 
  TreeExamples.binaryTree 3 |> design 1.0 |> render
  TreeExamples.parallelPaths 4 3 |> design 1.0 |> render

let runDesignBenchmarks() = 
  let iterations = 100

  let treeGen h = TreeExamples.parallelPaths 4 h

  seq {1..iterations} |> Seq.iter (fun _ -> treeGen 2 |> design 1.0 |> ignore)

  seq {1..200} 
    |> Seq.iter 
      (fun height -> 
        let tree = treeGen height
        let start = Stopwatch.GetTimestamp()
        seq {1..iterations} |> Seq.iter (fun _ -> design 1.0 tree |> ignore)
        let elapsedNs = (Stopwatch.GetElapsedTime start).TotalNanoseconds / (float iterations)
        // printfn $"Binary tree height: {height}. Design avg time: {elapsedNs} ns"
        printfn $"{height}|{elapsedNs}"
      )

let runRenderingBenchmarks () =
  let iterations = 100

  let treeGen h = TreeExamples.binaryTree h

  seq {1..iterations} |> Seq.iter (fun _ -> treeGen 2 |> design 1.0 |> getChart |> ignore)

  seq {1..15} 
    |> Seq.iter 
      (fun height -> 
        let tree = treeGen height |> design 1.0
        let start = Stopwatch.GetTimestamp()
        seq {1..iterations} |> Seq.iter (fun _ -> let chart = getChart tree in ())
        let elapsedNs = (Stopwatch.GetElapsedTime start).TotalNanoseconds / (float iterations)
        // printfn $"Binary tree height: {height}. Design avg time: {elapsedNs} ns"
        printfn $"{height}|{elapsedNs}"
      )


runTests()