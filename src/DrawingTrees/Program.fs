open TreeDesigner
open TreeRenderer

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

let tree =
    Node
      ('a',
       [Node ('a', [Node ('a', []); Node ('a', [])]); Node ('a', [Node ('a', [])]);
        Node ('a', [])])

let designedTree = design 62.0 tree
designedTree |> printfn "%A"
designedTree |> render
let _ = System.Console.ReadKey ()
