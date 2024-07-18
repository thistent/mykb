module Ui.Font exposing
    ( Font
    , Variant
    , alignLeft
    , alignRight
    , bold
    , center
    , color
    , diagonalFractions
    , external
    , extraBold
    , extraLight
    , family
    , feature
    , glow
    , hairline
    , heavy
    , indexed
    , italic
    , justify
    , letterSpacing
    , ligatures
    , light
    , medium
    , monospace
    , ordinal
    , regular
    , sansSerif
    , semiBold
    , serif
    , shadow
    , size
    , slashedZero
    , smallCaps
    , stackedFractions
    , strike
    , swash
    , tabularNumbers
    , typeface
    , underline
    , unitalicized
    , variant
    , variantList
    , wordSpacing
    )

import Element.Font as F
import Ui exposing (..)
import Ui.Util exposing (cc)


type alias Font =
    F.Font


type alias Variant =
    F.Variant


alignLeft : Attribute msg
alignLeft =
    [ F.alignLeft ]


alignRight : Attribute msg
alignRight =
    [ F.alignRight ]


bold : Attribute msg
bold =
    [ F.bold ]


center : Attribute msg
center =
    [ F.center ]


color : Color -> Attr decorative msg
color fontColor =
    [ F.color <| cc fontColor ]


diagonalFractions : Variant
diagonalFractions =
    F.diagonalFractions


external : { url : String, name : String } -> Font
external =
    F.external


extraBold : Attribute msg
extraBold =
    [ F.extraBold ]


extraLight : Attribute msg
extraLight =
    [ F.extraLight ]


family : List Font -> Attribute msg
family families =
    [ F.family families ]


feature : String -> Bool -> Variant
feature =
    F.feature


glow : Color -> Float -> Attr decorative msg
glow clr i =
    [ F.glow (cc clr) i ]


hairline : Attribute msg
hairline =
    [ F.hairline ]


heavy : Attribute msg
heavy =
    [ F.heavy ]


indexed : String -> Int -> Variant
indexed =
    F.indexed


italic : Attribute msg
italic =
    [ F.italic ]


justify : Attribute msg
justify =
    [ F.justify ]


letterSpacing : Float -> Attribute msg
letterSpacing offset =
    [ F.letterSpacing offset ]


ligatures : Variant
ligatures =
    F.ligatures


light : Attribute msg
light =
    [ F.light ]


medium : Attribute msg
medium =
    [ F.medium ]


monospace : Font
monospace =
    F.monospace


ordinal : Variant
ordinal =
    F.ordinal


regular : Attribute msg
regular =
    [ F.regular ]


sansSerif : Font
sansSerif =
    F.sansSerif


semiBold : Attribute msg
semiBold =
    [ F.semiBold ]


serif : Font
serif =
    F.serif


shadow :
    { offset : ( Float, Float )
    , blur : Float
    , color : Color
    }
    -> Attr decorative msg
shadow opts =
    [ F.shadow
        { offset = opts.offset
        , blur = opts.blur
        , color = cc opts.color
        }
    ]


size : Int -> Attr decorative msg
size i =
    [ F.size i ]


slashedZero : Variant
slashedZero =
    F.slashedZero


smallCaps : Variant
smallCaps =
    F.smallCaps


stackedFractions : Variant
stackedFractions =
    F.stackedFractions


strike : Attribute msg
strike =
    [ F.strike ]


swash : Int -> Variant
swash =
    F.swash


tabularNumbers : Variant
tabularNumbers =
    F.tabularNumbers


typeface : String -> Font
typeface =
    F.typeface


underline : Attribute msg
underline =
    [ F.underline ]


unitalicized : Attribute msg
unitalicized =
    [ F.unitalicized ]


variant : Variant -> Attribute msg
variant var =
    [ F.variant var ]


variantList : List Variant -> Attribute msg
variantList vars =
    [ F.variantList vars ]


wordSpacing : Float -> Attribute msg
wordSpacing offset =
    [ F.wordSpacing offset ]
