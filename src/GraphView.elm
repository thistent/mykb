module GraphView exposing (..)

import Browser.Dom as Dom
import Browser.Events as Events
import Color exposing (Color)
import Force
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Json.Decode as Decode
import Task
import TypedSvg as Ts
import TypedSvg.Attributes as Tsa
import TypedSvg.Attributes.InPx as TsPx
import TypedSvg.Core as Ts exposing (Svg)
import TypedSvg.Types as Ts
import Types exposing (..)
import Ui as El exposing (Element)
import Zoom exposing (OnZoom, Zoom)


type alias Pal colorType =
    { black : colorType
    , red : colorType
    , green : colorType
    , yellow : colorType
    , blue : colorType
    , magenta : colorType
    , cyan : colorType
    , white : colorType
    }


type GraphMsg
    = WinResize Int Int
    | DragAt ( Float, Float )
    | DragEnd ( Float, Float )
    | DragStart NodeId ( Float, Float )
    | ReceiveElementPosition (Result Dom.Error Dom.Element)
    | Resize
    | Tick
    | ZoomMsg OnZoom


type alias Flags =
    { width : Int, height : Int }


init : ( GraphModel, Cmd Msg )
init =
    ( Init <| Graph.mapContexts initNode ()
    , getElementPosition
    )


{-| The graph data we defined at the end of the module has the type
`Graph String ()`. We have to convert it into a `Graph Entity ()`.
`Force.Entity` is an extensible record which includes the coordinates for the
node.
-}
initNode : NodeContext String () -> NodeContext Entity ()
initNode ctx =
    { node =
        { label = Force.entity ctx.node.id ctx.node.label
        , id = ctx.node.id
        }
    , incoming = ctx.incoming
    , outgoing = ctx.outgoing
    }


{-| Initializes the simulation by setting the forces for the graph.
-}
initSimulation : Graph Entity () -> Float -> Float -> Force.State NodeId
initSimulation graph width height =
    let
        link : { c | from : a, to : b } -> ( a, b )
        link { from, to } =
            ( from, to )
    in
    Force.simulation
        [ -- Defines the force that pulls connected nodes together. You can use
          -- `Force.customLinks` if you need to adjust the distance and
          -- strength.
          Force.links <| List.map link <| Graph.edges graph

        -- Defines the force that pushes the nodes apart. The default strength
        -- is `-30`, but since we are drawing fairly large circles for each
        -- node, we need to increase the repulsion by decreasing the strength to
        -- `-150`.
        , Force.manyBodyStrength -50 <| List.map .id <| Graph.nodes graph
        , Force.collision 30 <| List.map .id <| Graph.nodes graph

        -- Defines the force that pulls nodes to a center. We set the center
        -- coordinates to the center of the svg viewport.
        , Force.center (width / 2) (height / 2)
        ]
        |> Force.iterations 400


{-| Initializes the zoom and sets a minimum and maximum zoom level.

You can also use `Zoom.translateExtent` to restrict the area in which the user
may drag, but since the graph is larger than the viewport and the exact
dimensions depend on the data and the final layout, you would either need to use
some kind of heuristic for the final dimensions here, or you would have to let
the simulation play out (or use `Force.computeSimulate` to calculate it at
once), find the min and max x and y positions of the graph nodes and use those
values to set the translate extent.

-}
initZoom : SvgElement -> Zoom
initZoom element =
    Zoom.init { width = element.width, height = element.height }
        |> Zoom.scaleExtent 0.5 3
        |> Zoom.translateExtent ( ( 0, 0 ), ( element.width, element.height ) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        mapGraphModel =
            Tuple.mapFirst (\gs -> { model | graph = gs })
    in
    case ( msg, model.graph ) of
        ( WinResize x y, _ ) ->
            ( { model
                | width = x
                , height = y
              }
            , Cmd.none
            )

        ( Tick, Ready state ) ->
            let
                newCount =
                    model.count + 1
            in
            handleTick state
                |> mapGraphModel
                |> (\( m, c ) -> ( { m | count = newCount }, c ))

        ( Tick, Init _ ) ->
            let
                newCount =
                    model.count + 1
            in
            ( { model | count = newCount }, Cmd.none )

        --else
        --    ( { model | count = 0 }, Cmd.none )
        ( DragAt xy, Ready state ) ->
            handleDragAt xy state |> mapGraphModel

        ( DragAt _, Init _ ) ->
            ( model, Cmd.none )

        ( DragEnd xy, Ready state ) ->
            case state.drag of
                Just { index } ->
                    ( { model
                        | graph =
                            Ready
                                { state
                                    | drag = Nothing
                                    , graph = updateNodePosition index xy state
                                }
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model | graph = Ready state }, Cmd.none )

        ( DragEnd _, Init _ ) ->
            ( model, Cmd.none )

        ( DragStart index xy, Ready state ) ->
            ( { model
                | graph =
                    Ready
                        { state
                            | drag =
                                Just
                                    { start = xy
                                    , current = xy
                                    , index = index
                                    }
                        }
              }
            , Cmd.none
            )

        ( DragStart _ _, Init _ ) ->
            ( model, Cmd.none )

        ( ReceiveElementPosition (Ok { element }), Init graph ) ->
            -- When we get the svg element position and dimensions, we are
            -- ready to initialize the simulation and the zoom, but we cannot
            -- show the graph yet. If we did, we would see a noticable jump.
            ( { model
                | graph =
                    Ready
                        { drag = Nothing
                        , element = element
                        , graph = graph
                        , showGraph = False
                        , simulation =
                            initSimulation
                                graph
                                element.width
                                element.height
                        , zoom = initZoom element
                        }
              }
            , Cmd.none
            )

        ( ReceiveElementPosition (Ok { element }), Ready state ) ->
            ( { model
                | graph =
                    Ready
                        { drag = state.drag
                        , element = element
                        , graph = state.graph
                        , showGraph = True
                        , simulation =
                            initSimulation
                                state.graph
                                element.width
                                element.height
                        , zoom = initZoom element
                        }
              }
            , Cmd.none
            )

        ( ReceiveElementPosition (Err _), _ ) ->
            ( model, Cmd.none )

        ( Resize, _ ) ->
            ( model, getElementPosition )

        ( ZoomMsg zoomMsg, Ready state ) ->
            ( { model
                | graph = Ready { state | zoom = Zoom.update zoomMsg state.zoom }
              }
            , Cmd.none
            )

        ( ZoomMsg _, Init _ ) ->
            ( model, Cmd.none )


handleDragAt : ( Float, Float ) -> ReadyState -> ( GraphModel, Cmd Msg )
handleDragAt xy ({ drag, simulation } as state) =
    case drag of
        Just { start, index } ->
            ( Ready
                { state
                    | drag =
                        Just
                            { start = start
                            , current = xy
                            , index = index
                            }
                    , graph = updateNodePosition index xy state
                    , simulation = Force.reheat simulation
                }
            , Cmd.none
            )

        Nothing ->
            ( Ready state, Cmd.none )


handleTick : ReadyState -> ( GraphModel, Cmd Msg )
handleTick state =
    let
        ( newSimulation, list ) =
            Force.tick state.simulation <|
                List.map .label <|
                    Graph.nodes state.graph
    in
    case state.drag of
        Nothing ->
            ( Ready
                { state
                    | graph = updateGraphWithList state.graph list
                    , showGraph = True
                    , simulation = newSimulation
                }
            , Cmd.none
            )

        Just { current, index } ->
            ( Ready
                { state
                    | graph =
                        Graph.update index
                            (Maybe.map
                                (updateNode
                                    (shiftPosition
                                        state.zoom
                                        ( state.element.x
                                        , state.element.y
                                        )
                                        current
                                    )
                                )
                            )
                            (updateGraphWithList state.graph list)
                    , showGraph = True
                    , simulation = newSimulation
                }
            , Cmd.none
            )


updateNode :
    ( Float, Float )
    -> NodeContext Entity ()
    -> NodeContext Entity ()
updateNode ( x, y ) nodeCtx =
    let
        nodeValue =
            nodeCtx.node.label
    in
    updateContextWithValue nodeCtx { nodeValue | x = x, y = y }


updateNodePosition : NodeId -> ( Float, Float ) -> ReadyState -> Graph Entity ()
updateNodePosition index xy state =
    Graph.update
        index
        (Maybe.map
            (updateNode
                (shiftPosition
                    state.zoom
                    ( state.element.x, state.element.y )
                    xy
                )
            )
        )
        state.graph


updateContextWithValue :
    NodeContext Entity ()
    -> Entity
    -> NodeContext Entity ()
updateContextWithValue nodeCtx value =
    let
        node =
            nodeCtx.node
    in
    { nodeCtx | node = { node | label = value } }


updateGraphWithList : Graph Entity () -> List Entity -> Graph Entity ()
updateGraphWithList =
    let
        graphUpdater value =
            Maybe.map (\ctx -> updateContextWithValue ctx value)
    in
    List.foldr (\node graph -> Graph.update node.id (graphUpdater node) graph)


shiftPosition : Zoom -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
shiftPosition zoom ( elementX, elementY ) ( clientX, clientY ) =
    let
        zoomRecord =
            Zoom.asRecord zoom
    in
    ( (clientX - zoomRecord.translate.x - elementX) / zoomRecord.scale
    , (clientY - zoomRecord.translate.y - elementY) / zoomRecord.scale
    )



-- Subscriptions --


subs : Model -> Sub Msg
subs model =
    let
        dragSubscriptions : Sub Msg
        dragSubscriptions =
            Sub.batch
                [ Events.onMouseMove
                    (Decode.map (.clientPos >> DragAt) Mouse.eventDecoder)
                , Events.onMouseUp
                    (Decode.map (.clientPos >> DragEnd) Mouse.eventDecoder)
                , Events.onAnimationFrame (always Tick)
                ]

        readySubscriptions : ReadyState -> Sub Msg
        readySubscriptions { drag, simulation, zoom } =
            Sub.batch
                [ Zoom.subscriptions zoom ZoomMsg
                , case drag of
                    Nothing ->
                        if Force.isCompleted simulation then
                            Sub.none

                        else
                            Events.onAnimationFrame (always Tick)

                    Just _ ->
                        dragSubscriptions
                ]
    in
    Sub.batch
        [ -- Graph Subs
          case model.graph of
            Init _ ->
                Sub.none

            Ready state ->
                readySubscriptions state
        , Events.onResize WinResize
        ]



-- View --


view : Model -> Element Msg
view model =
    let
        zoomEvents : List (Ts.Attribute Msg)
        zoomEvents =
            case model.graph of
                Init _ ->
                    []

                Ready { zoom } ->
                    Zoom.events zoom ZoomMsg

        zoomTransformAttr : Ts.Attribute Msg
        zoomTransformAttr =
            case model.graph of
                Init _ ->
                    Tsa.class []

                Ready { zoom } ->
                    Zoom.transform zoom
    in
    El.html <|
        Ts.svg
            [ Tsa.id elementId
            , Tsa.width <| Ts.Percent 100
            , Tsa.height <| Ts.Percent 100
            ]
            [ Ts.defs [] [ arrowhead model.pal.fg ]
            , Ts.rect
                ([ Tsa.width <| Ts.Percent 100
                 , Tsa.height <| Ts.Percent 100
                 , Tsa.fill <| Ts.Paint model.pal.fg
                 , Tsa.cursor Ts.CursorMove
                 ]
                    ++ zoomEvents
                )
                []
            , Ts.g
                [ zoomTransformAttr ]
                [ renderGraph model.graph ]
            ]


elementId : String
elementId =
    "exercise-graph"



-- Types


{-| In order to correctly calculate the node positions, we need to know the
coordinates of the svg element. The simulation is started when we
receive them.
-}
type GraphModel
    = Init (Graph Entity ())
    | Ready ReadyState


type alias ReadyState =
    { drag : Maybe Drag
    , graph : Graph Entity ()
    , simulation : Force.State NodeId
    , zoom : Zoom

    -- The position and dimensions of the svg element.
    , element : SvgElement

    -- If you immediately show the graph when moving from `Init` to `Ready`,
    -- you will briefly see the nodes in the upper left corner before the first
    -- simulation tick positions them in the center. To avoid this sudden jump,
    -- `showGraph` is initialized with `False` and set to `True` with the first
    -- `Tick`.
    , showGraph : Bool
    }


type alias Drag =
    { current : ( Float, Float )
    , index : NodeId
    , start : ( Float, Float )
    }


type alias SvgElement =
    { height : Float
    , width : Float
    , x : Float
    , y : Float
    }


type alias Entity =
    Force.Entity NodeId { value : String }


renderGraph : GraphModel -> Svg Msg
renderGraph model =
    case model of
        Init _ ->
            Ts.text ""

        Ready { graph, showGraph } ->
            if showGraph then
                Ts.g
                    []
                    [ Graph.edges graph
                        |> List.map (linkElement graph)
                        |> Ts.g [ Tsa.class [ "links" ] ]
                    , Graph.nodes graph
                        |> List.map nodeElement
                        |> Ts.g [ Tsa.class [ "nodes" ] ]
                    ]

            else
                Ts.text ""


nodeRad : Float
nodeRad =
    25.0


{-| Draws a single vertex (node).
-}
nodeElement : Color -> Node Entity -> Svg Msg
nodeElement color node =
    Ts.g [ Tsa.class [ "node" ] ]
        [ Ts.circle
            [ TsPx.r <| nodeRad
            , Tsa.fill <| Ts.Paint color
            , Tsa.cursor Ts.CursorPointer

            -- The coordinates are initialized and updated by `Force.simulation`
            -- and `Force.tick`, respectively.
            , TsPx.cx node.label.x
            , TsPx.cy node.label.y

            -- Add event handler for starting a drag on the node.
            , onMouseDown node.id
            , Touch.onStart (touchCoords >> DragStart node.id)
            , Touch.onMove (touchCoords >> DragAt)
            , Touch.onEnd (touchCoords >> DragEnd)
            ]
            []

        --[ Ts.title [] [ Ts.text node.label.value ] ]
        {- , Ts.text_
           [ -- Align text label at the center of the circle.
             TsPx.dx <| node.label.x
           , TsPx.dy <| node.label.y
           , Tsa.alignmentBaseline Ts.AlignmentCentral
           , Tsa.textAnchor Ts.AnchorMiddle

           -- styling
           , Tsa.fontSize <| Ts.Px 12
           , Tsa.fill nodeTextColor

           -- Setting pointer events to none allows the user to click on the
           -- element behind the text, so in this case the circle. If you
           -- position the text label outside of the circle, you also should
           -- do this, so that drag and zoom operations are not interrupted
           -- when the cursor is above the text.
           , Tsa.pointerEvents "none"
           ]
           [ Ts.text node.label.value ]
        -}
        ]


{-| This function draws the lines between the vertices.
-}
linkElement : Color -> Graph Entity () -> Edge () -> Svg msg
linkElement color graph edge =
    let
        source =
            Maybe.withDefault (Force.entity 0 "") <|
                Maybe.map (.node >> .label) <|
                    Graph.get edge.from graph

        target =
            Maybe.withDefault (Force.entity 0 "") <|
                Maybe.map (.node >> .label) <|
                    Graph.get edge.to graph
    in
    Ts.line
        [ TsPx.x1 source.x
        , TsPx.y1 source.y
        , TsPx.x2 target.x
        , TsPx.y2 target.y
        , TsPx.strokeWidth 4
        , Tsa.stroke <|
            Ts.Paint <|
                color

        --, Tsa.markerEnd "url(#arrowhead)"
        ]
        []



-- Definitions


{-| This is the definition of the arrow head that is displayed at the end of
the edges.

It is a child of the svg `defs` element and can be referenced by its id with
`url(#arrowhead)`.

-}
arrowhead : Color -> Svg msg
arrowhead edgeColor =
    Ts.marker
        [ Tsa.id "arrowhead"
        , Tsa.orient "auto"
        , Tsa.markerWidth <| Ts.Px 8.0
        , Tsa.markerHeight <| Ts.Px 8.0
        , Tsa.refX <| String.fromFloat <| nodeRad * 1.25
        , Tsa.refY "4"
        ]
        [ Ts.polygon
            [ Tsa.points [ ( 0, 0 ), ( 8, 4 ), ( 0, 8 ) ]
            , Tsa.fill <| Ts.Paint edgeColor
            ]
            []
        ]



-- Events and tasks


{-| This is the event handler that handles clicks on the vertices (nodes).

The event catches the `clientPos`, which is a tuple with the
`MouseEvent.clientX` and `MouseEvent.clientY` values. These coordinates are
relative to the client area (browser viewport).

If the graph is positioned anywhere else than at the coordinates `(0, 0)`, the
svg element position must be subtracted when setting the node position. This is
handled in the update function by calling the `shiftPosition` function.

-}
onMouseDown : NodeId -> Ts.Attribute Msg
onMouseDown index =
    Mouse.onDown (.clientPos >> DragStart index)


{-| Generates coordinates from a Touch.Event.
-}
touchCoords : Touch.Event -> ( Float, Float )
touchCoords touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        -- .screenPos
        |> Maybe.withDefault ( 0, 0 )



--|> (\( x, y ) -> ( x - origX, y - origY ))


{-| This function returns a command to retrieve the position of the svg element.
-}
getElementPosition : Cmd Msg
getElementPosition =
    Task.attempt ReceiveElementPosition (Dom.getElement elementId)
