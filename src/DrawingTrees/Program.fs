open TreeDesigner
open TreeRenderer
open System.Diagnostics

let runTests() =
  TreeDesignerChecks.runAll
  TreeRendererChecks.runAll

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

let runBenchmarks() = 
  let iterations = 100

  let treeGen h = TreeExamples.parallelPaths 4 h

  seq {1..iterations} |> Seq.iter (fun _ -> treeGen 2 |> design 1.0 |> ignore)

  seq {0..200} 
    |> Seq.iter 
      (fun height -> 
        let start = Stopwatch.GetTimestamp()
        let tree = treeGen height
        seq {1..iterations} |> Seq.iter (fun _ -> design 1.0 tree |> ignore)
        let elapsedNs = (Stopwatch.GetElapsedTime start).TotalNanoseconds / (float iterations)
        // printfn $"Binary tree height: {height}. Design avg time: {elapsedNs} ns"
        printfn $"{height}|{elapsedNs}"
      )

runTests()