module AlgGraph exposing
    ( Graph
    , compact
    , concatMap
    , edges
    , empty
    , foldEdges
    , foldVertices
    , fromEdges
    , fromVertices
    , hasEdge
    , hasVertex
    , isEmpty
    , mapEdges
    , mapVertices
    , product
    , union
    , vertex
    , vertices
    )

{-| A multi-dimensional algebraic graph library with both vertex and edge
data.

Currently, both edges and nodes need to be comparables, but it might be
nice to represent each with custom types.

-}

import Dict exposing (Dict)
import Set exposing (Set)


type Graph e a
    = Empty
    | Vertex a
    | Overlay (Graph e a) (Graph e a)
    | Connect e (Graph e a) (Graph e a)


empty : Graph e a
empty =
    Empty


vertex : a -> Graph e a
vertex a =
    Vertex a


union : Graph e a -> Graph e a -> Graph e a
union g1 g2 =
    Overlay g1 g2


product : e -> Graph e a -> Graph e a -> Graph e a
product e g1 g2 =
    Connect e g1 g2


vertices : Graph e comparable -> Set comparable
vertices graph =
    case graph of
        Empty ->
            Set.empty

        Vertex a ->
            Set.singleton a

        Overlay a b ->
            Set.union (vertices a) (vertices b)

        Connect _ a b ->
            Set.union (vertices a) (vertices b)


edges :
    Graph comparable2 comparable
    -> Dict ( comparable, comparable ) (Set comparable2)
edges graph =
    let
        lo :
            ( comparable, comparable )
            -> Set comparable2
            -> Dict ( comparable, comparable ) (Set comparable2)
            -> Dict ( comparable, comparable ) (Set comparable2)
        lo =
            Dict.insert

        ro :
            ( comparable, comparable )
            -> Set comparable2
            -> Dict ( comparable, comparable ) (Set comparable2)
            -> Dict ( comparable, comparable ) (Set comparable2)
        ro =
            Dict.insert

        lr :
            ( comparable, comparable )
            -> Set comparable2
            -> Set comparable2
            -> Dict ( comparable, comparable ) (Set comparable2)
            -> Dict ( comparable, comparable ) (Set comparable2)
        lr ab e1 e2 d =
            Dict.insert ab (Set.union e1 e2) d

        merge :
            Dict ( comparable, comparable ) (Set comparable2)
            -> Dict ( comparable, comparable ) (Set comparable2)
            -> Dict ( comparable, comparable ) (Set comparable2)
        merge d1 d2 =
            Dict.merge lo lr ro d1 d2 Dict.empty
    in
    case graph of
        Empty ->
            Dict.empty

        Vertex _ ->
            Dict.empty

        Overlay a b ->
            merge (edges a) (edges b)

        Connect e a b ->
            let
                vA : List comparable
                vA =
                    vertices a |> Set.toList

                vB : List comparable
                vB =
                    vertices b |> Set.toList

                -- #C0FFEE
                cross : List ( ( comparable, comparable ), Set comparable2 )
                cross =
                    vA
                        |> List.concatMap
                            (\x ->
                                List.map
                                    (\y -> ( ( x, y ), Set.singleton e ))
                                    vB
                            )

                --|> List.map (\x -> ( x, Set.singleton e ))
            in
            cross
                -- NOTE: Dict.fromList new entries clobber old ones!
                |> Dict.fromList
                |> merge (edges a)
                |> merge (edges b)


isEmpty : Graph e a -> Bool
isEmpty graph =
    case graph of
        Empty ->
            True

        Vertex _ ->
            False

        Overlay a b ->
            isEmpty a && isEmpty b

        Connect _ a b ->
            isEmpty a && isEmpty b


hasVertex : a -> Graph e a -> Bool
hasVertex elem graph =
    case graph of
        Empty ->
            False

        Vertex a ->
            elem == a

        Overlay a b ->
            hasVertex elem a || hasVertex elem b

        Connect _ a b ->
            hasVertex elem a || hasVertex elem b


hasEdge : comparable -> Graph comparable a -> Bool
hasEdge edg graph =
    case graph of
        Empty ->
            False

        Vertex _ ->
            False

        Overlay a b ->
            hasEdge edg a || hasEdge edg b

        Connect e a b ->
            if edg == e then
                True

            else
                hasEdge edg a || hasEdge edg b


mapVertices : (a -> b) -> Graph e a -> Graph e b
mapVertices fun graph =
    case graph of
        Empty ->
            Empty

        Vertex a ->
            Vertex (fun a)

        Overlay a b ->
            Overlay (mapVertices fun a) (mapVertices fun b)

        Connect e a b ->
            Connect e (mapVertices fun a) (mapVertices fun b)


mapEdges : (e -> f) -> Graph e a -> Graph f a
mapEdges fun graph =
    case graph of
        Empty ->
            Empty

        Vertex a ->
            Vertex a

        Overlay a b ->
            Overlay (mapEdges fun a) (mapEdges fun b)

        Connect e a b ->
            Connect (fun e) (mapEdges fun a) (mapEdges fun b)


concatMap : (a -> Graph e b) -> Graph e a -> Graph e b
concatMap fun graph =
    case graph of
        Empty ->
            Empty

        Vertex a ->
            fun a

        Overlay a b ->
            Overlay (concatMap fun a) (concatMap fun b)

        Connect e a b ->
            Connect e (concatMap fun a) (concatMap fun b)



-- concatMapEdge : (e -> Graph f a) -> Graph e a -> Graph f a
-- concatMapEdge fun graph =


foldVertices : (a -> b -> b) -> b -> Graph e a -> b
foldVertices fun acc graph =
    case graph of
        Empty ->
            acc

        Vertex a ->
            fun a acc

        Overlay a b ->
            foldVertices fun (foldVertices fun acc a) b

        Connect _ a b ->
            foldVertices fun (foldVertices fun acc a) b


foldEdges : (e -> f -> f) -> f -> Graph e a -> f
foldEdges fun acc graph =
    case graph of
        Empty ->
            acc

        Vertex _ ->
            acc

        Overlay a b ->
            foldEdges fun (foldEdges fun acc a) b

        Connect e a b ->
            foldEdges fun (foldEdges fun (fun e acc) a) b


compact : Graph comparable2 comparable -> Graph comparable2 comparable
compact graph =
    let
        -- Edges --
        e =
            graph
                |> edges
                |> fromEdges

        -- Vertices --
        v =
            graph
                |> vertices
                |> fromVertices
    in
    union v e


fromEdges :
    Dict ( comparable, comparable ) (Set comparable2)
    -> Graph comparable2 comparable
fromEdges d =
    let
        foldFun :
            ( comparable, comparable )
            -> Set comparable2
            -> Graph comparable2 comparable
            -> Graph comparable2 comparable
        foldFun ( a, b ) edgeSet acc =
            -- Set edge -> List edge
            -- Empty FIXME!
            edgeSet
                |> Set.toList
                |> (\_ -> Empty)

        -- Dict.foldl : (k -> v -> b -> b) -> b -> Dict k v -> b
    in
    if Dict.isEmpty d then
        Empty

    else
        d |> Dict.foldl foldFun Empty



{-
   removeIdentityMorphisms : Graph e comparable -> Graph e comparable
   removeIdentityMorphisms graph =
       graph
           |> edges
           |> Dict.filter (\( a, b ) _ -> a /= b)
           |> fromEdges


-}


{-| FIXME: This doesn't work right!
-}
fromVertices : Set comparable -> Graph e comparable
fromVertices vs =
    if Set.isEmpty vs then
        Empty

    else
        vs
            |> Set.foldl
                (\a b -> Overlay (Vertex a) b)
                Empty
