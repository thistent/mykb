module Pane exposing (Pane, SplitRenderer, hSplit, render, single, splitAbove, splitBelow, splitLeft, splitRight, vSplit)

import Browser.Dom exposing (Viewport)



{- type alias Size =
   { width : Float, height : Float }
-}


type alias SplitRenderer a =
    { h : Float -> Viewport -> a -> a -> a
    , v : Float -> Viewport -> a -> a -> a
    }


type Pane a
    = RenderPane (Viewport -> a)
    | HSplit Float (Pane a) (Pane a)
    | VSplit Float (Pane a) (Pane a)


render : SplitRenderer a -> Viewport -> Pane a -> a
render spr vp pane =
    let
        paneVp :
            { x : Float
            , y : Float
            , width : Float
            , height : Float
            }
        paneVp =
            vp.viewport
    in
    case pane of
        RenderPane fun ->
            fun vp

        HSplit f p1 p2 ->
            let
                ratio : Float
                ratio =
                    toRatio f
            in
            spr.h ratio
                vp
                (render spr
                    { vp
                        | viewport =
                            { paneVp
                                | height =
                                    paneVp.height * ratio
                            }
                    }
                    p1
                )
                (render spr
                    { vp
                        | viewport =
                            { paneVp
                                | height =
                                    paneVp.height * (1.0 - ratio)
                            }
                    }
                    p2
                )

        VSplit f p1 p2 ->
            let
                ratio : Float
                ratio =
                    toRatio f
            in
            spr.v ratio
                vp
                (render spr
                    { vp | viewport = { paneVp | width = paneVp.width * ratio } }
                    p1
                )
                (render spr
                    { vp | viewport = { paneVp | width = paneVp.width * (1.0 - ratio) } }
                    p2
                )


toRatio : Float -> Float
toRatio =
    clamp 0.05 0.95


single : (Viewport -> a) -> Pane a
single vFun =
    RenderPane vFun


hSplit : Float -> Pane a -> Pane a -> Pane a
hSplit ratio p1 p2 =
    HSplit ratio p1 p2


vSplit : Float -> Pane a -> Pane a -> Pane a
vSplit ratio p1 p2 =
    VSplit ratio p1 p2


splitLeft : Float -> (Viewport -> a) -> Pane a -> Pane a
splitLeft ratio newPaneView oldPane =
    VSplit ratio (RenderPane newPaneView) oldPane


splitRight : Float -> (Viewport -> a) -> Pane a -> Pane a
splitRight ratio newPaneView oldPane =
    VSplit (1.0 - ratio) oldPane (RenderPane newPaneView)


splitAbove : Float -> (Viewport -> a) -> Pane a -> Pane a
splitAbove ratio newPaneView oldPane =
    HSplit ratio oldPane (RenderPane newPaneView)


splitBelow : Float -> (Viewport -> a) -> Pane a -> Pane a
splitBelow ratio newPaneView oldPane =
    HSplit (1.0 - ratio) (RenderPane newPaneView) oldPane
