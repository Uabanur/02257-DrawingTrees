﻿open TreeDesigner
open TreeDesignerChecks
open TreeRenderer
open TreeRendererChecks

TreeDesignerChecks.runAll
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

let designedTree = design 2.0 tree
designedTree |> printfn "%A"
designedTree |> render
let _ = System.Console.ReadKey ()
