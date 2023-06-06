open Config
open TreeDesigner
open TreeRenderer

// TreeDesignerChecks.runAll
TreeRendererChecks.runAll

let tree =
    Node("r", [
        Node("a", [
            Node("aa", []);
            Node("ab", []);
            Node("ac", []);
        ]);
        Node("b", [
            Node("ba", [
                Node("baa", []);
                Node("bab", []);
                Node("bac", []);
            ]);
        ]);
        Node("c", []);
    ])

let config = { Spacing = Settings().Designer.Spacing }
let designedTree = design config tree
// designedTree |> printfn "%A"
designedTree |> render
let _ = System.Console.ReadKey ()
