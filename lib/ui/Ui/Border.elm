module Ui.Border exposing
    ( color
    , dashed
    , dotted
    , glow
    , innerGlow
    , innerShadow
    , roundEach
    , rounded
    , shadow
    , solid
    , width
    , widthEach
    , widthXY
    )

import Element.Border as Border
import Ui exposing (..)
import Ui.Util exposing (cc)


color : Color -> Attr decorative msg
color clr =
    [ Border.color <| cc clr ]


dashed : Attribute msg
dashed =
    [ Border.dashed ]


dotted : Attribute msg
dotted =
    [ Border.dotted ]


glow : Color -> Float -> Attr decorative msg
glow clr size =
    [ Border.glow (cc clr) size ]


innerGlow : Color -> Float -> Attr decorative msg
innerGlow clr size =
    [ Border.innerGlow (cc clr) size ]


innerShadow :
    { offset : ( Float, Float )
    , size : Float
    , blur : Float
    , color : Color
    }
    -> Attr decorative msg
innerShadow sh =
    [ Border.innerShadow
        { offset = sh.offset
        , size = sh.size
        , blur = sh.blur
        , color = cc sh.color
        }
    ]


roundEach :
    { topLeft : Int
    , topRight : Int
    , bottomLeft : Int
    , bottomRight : Int
    }
    -> Attribute msg
roundEach corners =
    [ Border.roundEach corners ]


rounded : Int -> Attribute msg
rounded radius =
    [ Border.rounded radius ]


shadow :
    { offset : ( Float, Float )
    , size : Float
    , blur : Float
    , color : Color
    }
    -> Attr decorative msg
shadow sh =
    [ Border.shadow
        { offset = sh.offset
        , size = sh.size
        , blur = sh.blur
        , color = cc sh.color
        }
    ]


solid : Attribute msg
solid =
    [ Border.solid ]


width : Int -> Attribute msg
width v =
    [ Border.width v ]


widthEach :
    { bottom : Int
    , left : Int
    , right : Int
    , top : Int
    }
    -> Attribute msg
widthEach edges =
    [ Border.widthEach edges ]


widthXY : Int -> Int -> Attribute msg
widthXY x y =
    [ Border.widthXY x y ]
