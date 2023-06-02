module TreeDesignerChecks

open FsCheck
open TreeDesigner

let moveTreeMovesTree originalPosition move =
    let (Node((_, newPosition), _)) = moveTree (Node(("test", originalPosition), []), move)
    newPosition.Equals(originalPosition + move)

let moveExtentMovesAllPairs (ex:Extent) move =
    moveExtent (ex,move)
        |> List.zip ex
        |> List.forall (fun ((p,q),(p',q')) -> (p'.Equals(p+move) && q'.Equals(q+move)))

let mergedExtentsHasMaxLength ex1 ex2 =
    merge ex1 ex2 |> List.length = max (List.length ex1) (List.length ex2)

let mergedExtentsCorrectPairMerge ex1 ex2 =
    let merged = merge ex1 ex2
    let leftCheck = Seq.zip ex1 merged |> Seq.forall (fun ((p,_), (m,_)) -> p.Equals(m))
    let rightCheck = Seq.zip ex2 merged |> Seq.forall (fun ((_,q), (_,m)) -> q.Equals(m))
    leftCheck && rightCheck

// Property 1
let nodesAtSameLevelShouldBeAtleastAGivenDistanceApart (tree:Tree<string*float>) =
    let rec nodeDistanceCheck levelNodes minSpacing =
        if List.length levelNodes <= 1 then true
        else
            let positions = List.map (fun (Node((_,p),_)) -> p) levelNodes |> List.sort
            let validDistance = Seq.zip positions (Seq.skip 1 positions)
                                    |> Seq.fold (fun s (v1,v2) -> s && v2-v1 >= minSpacing) true
            let nextLevel = List.collect (fun (Node(_,children)) -> children) levelNodes
            validDistance && nodeDistanceCheck nextLevel minSpacing
    in nodeDistanceCheck [tree] 1.0

// Property 2
// TODO: check sum = 0

let runAll =
    let check prop name = Check.One ({Config.Quick with Name = name}, prop)

    check moveTreeMovesTree <| nameof moveTreeMovesTree
    check moveExtentMovesAllPairs <| nameof moveExtentMovesAllPairs
    check mergedExtentsHasMaxLength <| nameof mergedExtentsHasMaxLength
    check mergedExtentsCorrectPairMerge <| nameof mergedExtentsCorrectPairMerge
    check nodesAtSameLevelShouldBeAtleastAGivenDistanceApart <| nameof nodesAtSameLevelShouldBeAtleastAGivenDistanceApart
