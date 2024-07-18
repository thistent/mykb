module Style exposing (..)

import Color.Manipulate as CM
import Html.Attributes as Attr
import Types exposing (..)
import Ui exposing (..)
import Ui.Border as Border



-- Size Values --


lineSize : number
lineSize =
    2


fontSizeMultiplier : Float -> Int
fontSizeMultiplier pixelRatio =
    12 * pixelRatio |> round



--Color Schemes --


black : Color
black =
    rgb 0 0 0


newspaper : Pal
newspaper =
    { name = "Newspaper"
    , fg = rgb255 0x2B 0x16 0x00 -- #2B1600
    , bg = rgb255 0x9D 0x99 0x92 -- #9D9992#9b9994#918f8b
    , error = rgb255 0x8D 0x19 0x00 -- #8D1900
    , link = rgb255 0x00 0x4D 0x1F -- #004D1F
    , extLink = rgb255 0x00 0x33 0x70 -- #003370
    }


dark : Pal
dark =
    { name = "Dark"
    , fg = rgb255 0xB1 0xB0 0xAF -- #B1B0AF#A1A09F
    , bg = rgb255 0x1A 0x1C 0x1F -- #1A1C1F
    , error = rgb255 0xE7 0x65 0x5B -- #E66363#e7655b
    , link = rgb255 0xC9 0x99 0x23 -- #C99923#779ef9
    , extLink = rgb255 0x20 0xB1 0x6B -- #20B080#20b16b#20b256
    }


blueNote : Pal
blueNote =
    { name = "Blue-Note"
    , fg = rgb255 0x28 0x36 0x40 -- #283649
    , bg = rgb255 0x7E 0xB3 0xC2 -- #7eb3c2
    , error = rgb255 0x44 0x89 0xBF -- #4489bf
    , link = rgb255 0x44 0x89 0xBF -- #004499
    , extLink = rgb255 0x44 0x89 0xBF -- #4489BF
    }


greenNote : Pal
greenNote =
    { name = "Green-Note"
    , fg = rgb255 0x2C 0x3A 0x16 -- #2C3A16
    , bg = rgb255 0x86 0xB9 0x61 -- #86B961
    , error = rgb255 0x4F 0x93 0x43 -- #4F9343
    , link = rgb255 0x4F 0x93 0x43 -- #226600
    , extLink = rgb255 0x4F 0x93 0x43 -- #4F9343
    }


yellowNote : Pal
yellowNote =
    { name = "Yellow-Note"
    , fg = rgb255 0x48 0x33 0x0C -- #48330C
    , bg = rgb255 0xBE 0xAA 0x52 -- #BEAA52
    , error = rgb255 0xA3 0x7E 0x24 -- #A37E24
    , link = rgb255 0xA3 0x7E 0x24 -- #884400
    , extLink = rgb255 0xA3 0x7E 0x24 -- #A37E24
    }


orangeNote : Pal
orangeNote =
    { name = "Orange-Note"
    , fg = rgb255 0x53 0x2D 0x0F -- #532D0F
    , bg = rgb255 0xDB 0x9F 0x61 -- #DB9F61
    , error = rgb255 0xC6 0x6C 0x2C -- #C66C2C
    , link = rgb255 0xC6 0x6C 0x2C -- #882200
    , extLink = rgb255 0xC6 0x6C 0x2C -- #C66C2C
    }



{-

   redNote : Pal
   redNote =
       { name = "Red-Note"
       , fg = rgb255 0x57 0x29 0x23 -- #572923
       , bg = rgb255 0xD8 0x9C 0x9B -- #D89C9B
       , error = rgb255 0xC6 0x6C 0x2C -- #C66C2C
       , link = rgb255 0xD0 0x61 0x6A -- #D0616A
       , extLink = rgb255 0xD0 0x61 0x6A -- #D0616A
       }


    purpleNote : Pal
    purpleNote =
        { name = "Purple-Note"
        , fg = rgb255 0x42 0x30 0x40 -- #423040
        , bg = rgb255 0xB4 0xA5 0xC8 -- #B4A5C8
        , link = rgb255 0x93 0x76 0xC1 -- #9376C1
        , extLink = rgb255 0x93 0x76 0xC1 -- #9376C1
        }


   blueprint : Pal
   blueprint =
       { name = "Blueprint"
       , fg = rgb255 0x70 0xB1 0xD8 -- #70B1D8
       , bg = rgb255 0x02 0x25 0x49 -- #022549
       , link = rgb255 0xDD 0xE8 0xEC -- #DDE8EC
       , extLink = rgb255 0xEC 0xD0 0xFF -- #ECD0FF
       }


   term : Pal
   term =
       { name = "Terminal"
       , fg = rgb255 0x54 0xAE 0x10 -- #54AE10
       , bg = rgb255 0x00 0x36 0x18 -- #003618
       , link = rgb255 0xBB 0xFF 0x77 -- #BBFF77
       , extLink = rgb255 0xEB 0xD0 0x5F -- #EBD05F
       }

-}
-- Attributes --


otherSide : Element Msg -> Attribute Msg
otherSide content =
    behindContent <|
        el
            [ fillSpace
            , style "-moz-transform" "scale(1, -1)"
            , style "-webkit-transform" "scale(1, -1)"
            , style "-o-transform" "scale(1, -1)"
            , style "-ms-transform" "scale(1, -1)"
            , style "transform" "scale(1, -1)"
            , style "pointer-events" "none"
            , noSelect
            ]
        <|
            content


noSelect : Attribute Msg
noSelect =
    batch
        [ style "-webkit-touch-callout" "none" -- iOS Safari
        , style "-webkit-user-select" "none" -- Safari
        , style "-khtml-user-select" "none" -- Konqueror HTML
        , style "-moz-user-select" "none" -- Old versions of Firefox
        , style "-ms-user-select" "none" -- Internet Explorer/Edge
        , style "user-select" "none" -- Non-prefixed version, currently supported by Chrome, Edge, Opera and Firefox
        ]


shadow : Attribute Msg
shadow =
    Border.shadow
        { offset = ( 1.0, 2.0 )
        , size = 0
        , blur = 5.0
        , color = addAlpha 0.5 black
        }


fillSpace : Attribute Msg
fillSpace =
    batch
        [ width fill
        , height fill
        ]


centerXY : Attribute Msg
centerXY =
    batch
        [ centerX
        , centerY
        ]


style : String -> String -> Attribute Msg
style s t =
    htmlAttribute <| Attr.style s t


addAlpha : Float -> Color -> Color
addAlpha alph color =
    let
        colorRec :
            { red : Float
            , green : Float
            , blue : Float
            , alpha : Float
            }
        colorRec =
            toRgb color
    in
    fromRgb { colorRec | alpha = alph }


mix : Float -> Color -> Color -> Color
mix f a b =
    CM.weightedMix b a f


saturate : Color -> Color
saturate c =
    CM.saturate 0.25 c
