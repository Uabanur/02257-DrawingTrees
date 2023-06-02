module TreeDesigner

type Position = float
type Tree<'a> = Node of 'a * (Tree<'a> list)
type Extent = (Position * Position) list

let moveTree (Node((label, x), subtrees), x') =
    Node((label, x+x'), subtrees)

let moveExtent ((e:Extent), x) :Extent =
    List.map (fun (p,q) -> (p+x, q+x)) e

let rec merge (ps:Extent) (qs:Extent) :Extent =
    match (ps,qs) with
        | ([], qs)               -> qs
        | (ps, [])               -> ps
        | ((p,_)::ps, (_,q)::qs) -> (p,q) :: merge ps qs

let mergeList (es: Extent list) : Extent =
    List.fold merge [] es

let rec fit (ps: Extent) (qs: Extent) (dist: float) =
    match (ps, qs) with
    | ((_,p)::ps, (q,_)::qs) -> max (fit ps qs dist) (p - q + dist)
    | _                      -> 0.0

let fitListLeft es dist =
    let rec helper acc es =
        match es with
            | [] -> []
            | (e::ess) ->
                let x = fit acc e dist
                in x :: helper (merge acc (moveExtent (e,x))) ess
    in es |> helper []

let fitListRight es dist =
    let rec helper acc es =
        match es with
            | [] -> []
            | (e::ess) ->
                let x = -(fit e acc dist)
                in x :: helper (merge (moveExtent (e,x)) acc) ess
    in List.rev es |> helper [] |> List.rev

let mean (x,y) :Position =
    (x + y)/2.0

let fitList es dist =
    List.map mean (List.zip (fitListLeft es dist) (fitListRight es dist))

let design dist tree =
    let rec design' (Node(label, subtrees)) =
        let (trees,extents) = List.unzip (List.map design' subtrees)
        let positions = fitList extents dist
        let ptrees = List.map moveTree (List.zip trees positions)
        let pextents = List.map moveExtent (List.zip extents positions)
        let resultextent = (0.0, 0.0) :: mergeList pextents
        let resulttree = Node((label, 0.0), ptrees)
        in (resulttree, resultextent)
    in fst(design' tree)

module TreeDesignerChecks =
    open FsCheck

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

    let runAll =
        let check prop name = Check.One ({Config.Quick with Name = name}, prop)

        check moveTreeMovesTree <| nameof moveTreeMovesTree
        check moveExtentMovesAllPairs <| nameof moveExtentMovesAllPairs
        check mergedExtentsHasMaxLength <| nameof mergedExtentsHasMaxLength
        check mergedExtentsCorrectPairMerge <| nameof mergedExtentsCorrectPairMerge
