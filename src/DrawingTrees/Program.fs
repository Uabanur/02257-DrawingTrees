open TreeDesigner
open TreeRenderer
open System.Diagnostics
open Config

let designConfig, renderConfig = getConfig()

let runTests() =
  TreeDesignerChecks.runAll()
  TreeRendererChecks.runAll()

let renderExamples() =
  TreeExamples.binaryTree 3 |> design designConfig |> render
  TreeExamples.parallelPaths 4 3 |> design designConfig |> render

let runDesignBenchmarks() =
  let iterations = 100

  let treeGen h = TreeExamples.parallelPaths 4 h

  seq {1..iterations} |> Seq.iter (fun _ -> treeGen 2 |> design designConfig |> ignore)

  seq {1..200}
    |> Seq.iter
      (fun height ->
        let tree = treeGen height
        let start = Stopwatch.GetTimestamp()
        seq {1..iterations} |> Seq.iter (fun _ -> design designConfig tree |> ignore)
        let elapsedNs = (Stopwatch.GetElapsedTime start).TotalNanoseconds / (float iterations)
        // printfn $"Binary tree height: {height}. Design avg time: {elapsedNs} ns"
        printfn $"{height}|{elapsedNs}"
      )

let runRenderingBenchmarks () =
  let iterations = 100

  let treeGen h = TreeExamples.binaryTree h

  seq {1..iterations} |> Seq.iter (fun _ -> treeGen 2 |> design designConfig |> getRendering |> ignore)

  seq {1..15}
    |> Seq.iter
      (fun height ->
        let tree = treeGen height |> design designConfig
        let start = Stopwatch.GetTimestamp()
        seq {1..iterations} |> Seq.iter (fun _ -> let chart = getRendering tree in ())
        let elapsedNs = (Stopwatch.GetElapsedTime start).TotalNanoseconds / (float iterations)
        // printfn $"Binary tree height: {height}. Design avg time: {elapsedNs} ns"
        printfn $"{height}|{elapsedNs}"
      )


runTests()

TreeExamples.binaryTree 4
  |> design designConfig
  |> render
