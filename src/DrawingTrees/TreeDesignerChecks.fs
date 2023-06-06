module TreeDesignerChecks
type internal Marker = interface end
let internal moduleName = string typeof<Marker>.DeclaringType

open FsCheck
open TreeDesigner

// let moveTreeMovesTree originalPosition move =
//     let (Node((_, newPosition), _)) = moveTree (Node(("test", originalPosition), []), move)
//     newPosition.Equals(originalPosition + move)
//
// let moveExtentMovesAllPairs (ex:Extent) move =
//     moveExtent (ex,move)
//         |> List.zip ex
//         |> List.forall (fun ((p,q),(p',q')) -> (p'.Equals(p+move) && q'.Equals(q+move)))
//
// let mergedExtentsHasMaxLength ex1 ex2 =
//     merge ex1 ex2 |> List.length = max (List.length ex1) (List.length ex2)
//
// let mergedExtentsCorrectPairMerge ex1 ex2 =
//     let merged = merge ex1 ex2
//     let leftCheck = Seq.zip ex1 merged |> Seq.forall (fun ((p,_), (m,_)) -> p.Equals(m))
//     let rightCheck = Seq.zip ex2 merged |> Seq.forall (fun ((_,q), (_,m)) -> q.Equals(m))
//     leftCheck && rightCheck
//
// let floatTolerance = 1e-5
//
// let floatsEquals v1 v2 =
//     abs (v2-v1) < floatTolerance
//
// let floatsHasMinDifference minDifference (v1, v2) =
//     let diff = abs (v2-v1)
//     diff + floatTolerance >= minDifference
//
// type Distance = float
//
// // Property 1
// let nodesAtSameLevelShouldBeAtleastAGivenDistanceApart (spacing:Distance) (tree:Tree<unit>) =
//     let checkSpacing =
//         floatsHasMinDifference spacing
//
//     let childrenWithAbsolutePositionToParent (Node((_,p),c)) =
//         List.map (fun (Node((l', p'), c')) -> Node((l', p+p'), c')) c
//
//     let rec nodeDistanceCheck levelNodes =
//         let validDistance =
//             List.length levelNodes <= 1  ||
//                 let positions = List.map (fun (Node((_,p),_)) -> p) levelNodes |> List.sort
//                 Seq.zip positions (Seq.skip 1 positions) |> Seq.forall checkSpacing
//
//         let nextLevel = List.collect childrenWithAbsolutePositionToParent levelNodes
//         validDistance && (List.isEmpty nextLevel || nodeDistanceCheck nextLevel)
//     in nodeDistanceCheck <| design spacing tree :: []
//
// let nodesAtSameLevelShouldBeAtleastAGivenDistanceApartNF spacingn tree =
//     // TODO change float generator to positive spacings
//     let spacing = abs <| NormalFloat.op_Explicit spacingn
//     nodesAtSameLevelShouldBeAtleastAGivenDistanceApart spacing tree
//
// // Property 2
// let parentIsCenteredOverOffsprings (spacing: Distance) (tree: Tree<unit>) =
//     let designedTree = design spacing tree
//     let rec checkPositions (Node(_, children)) =
//         let positions = List.map (fun (Node((_, p), _)) -> p) children
//         let sum = if List.isEmpty positions then 0.0 else List.min positions + List.max positions
//         floatsEquals sum 0.0 && List.forall checkPositions children
//     checkPositions designedTree
//
// let parentIsCenteredOverOffspringsNF spacing tree =
//     let spacing = abs <| NormalFloat.op_Explicit spacing
//     parentIsCenteredOverOffsprings spacing tree
//
// // Property 3
// let treeHasReflectionalSymmetry (spacing:Distance) (tree:Tree<unit>) =
//     let rec mirrorTree (Node(v,c)) =
//         Node(v, c |> List.map mirrorTree |> List.rev)
//
//     let positionedOriginalTree = design spacing tree
//     let positionedMirroredTree = design spacing (mirrorTree tree)
//
//     let areMirrored (Node((_,pOriginal),_), Node((_,pMirrored),_)) =
//         floatsEquals pOriginal -pMirrored
//
//     let rec hasReflectionalSymmetry nodeOriginal nodeMirrored =
//         let (Node(_,originalChildren)) = nodeOriginal
//         let (Node(_,mirroredChildren)) = nodeMirrored
//         let mirroredPairs = List.zip originalChildren (List.rev mirroredChildren)
//         areMirrored (nodeOriginal, nodeMirrored) && List.forall areMirrored mirroredPairs
//
//     in hasReflectionalSymmetry positionedOriginalTree positionedMirroredTree
//
// let treeHasReflectionalSymmetryNF spacingn tree =
//     // TODO change float generator to positive spacings
//     let spacing = abs <| NormalFloat.op_Explicit spacingn
//     treeHasReflectionalSymmetry spacing tree
//
//
// let runAll =
//     let config = {Config.QuickThrowOnFailure with QuietOnSuccess = true}
//     let check prop = Check.One (config, prop)
//
//     check moveTreeMovesTree
//     check moveExtentMovesAllPairs
//     check mergedExtentsHasMaxLength
//     check mergedExtentsCorrectPairMerge
//     check nodesAtSameLevelShouldBeAtleastAGivenDistanceApartNF // prop 1
//     check parentIsCenteredOverOffspringsNF                     // prop 2
//     check treeHasReflectionalSymmetryNF                        // prop 3
//     printfn $"{moduleName}: All checks are valid."
