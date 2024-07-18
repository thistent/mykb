module Ui.Background exposing
    ( color
    , gradient
    , image
    , tiled
    , tiledX
    , tiledY
    , uncropped
    )

import Element.Background as Bg
import Ui exposing (..)
import Ui.Util exposing (cc)


color : Color -> Attr decorative msg
color clr =
    [ Bg.color <| cc clr ]


gradient :
    { angle : Float
    , steps : List Color
    }
    -> Attr decorative msg
gradient grad =
    [ Bg.gradient
        { angle = grad.angle
        , steps = List.map cc grad.steps
        }
    ]


image : String -> Attribute msg
image src =
    [ Bg.image src ]


tiled : String -> Attribute msg
tiled src =
    [ Bg.tiled src ]


tiledX : String -> Attribute msg
tiledX src =
    [ Bg.tiledX src ]


tiledY : String -> Attribute msg
tiledY src =
    [ Bg.tiledY src ]


uncropped : String -> Attribute msg
uncropped src =
    [ Bg.uncropped src ]
