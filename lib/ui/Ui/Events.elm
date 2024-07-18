module Ui.Events exposing
    ( onClick
    , onDoubleClick
    , onFocus
    , onLoseFocus
    , onMouseDown
    , onMouseEnter
    , onMouseLeave
    , onMouseMove
    , onMouseUp
    )

import Element.Events as E
import Ui exposing (..)


{-| -}
onClick : msg -> Attribute msg
onClick msg =
    [ E.onClick msg ]


{-| -}
onDoubleClick : msg -> Attribute msg
onDoubleClick msg =
    [ E.onDoubleClick msg ]


{-| -}
onMouseDown : msg -> Attribute msg
onMouseDown msg =
    [ E.onMouseDown msg ]


{-| -}
onMouseUp : msg -> Attribute msg
onMouseUp msg =
    [ E.onMouseUp msg ]


{-| -}
onMouseEnter : msg -> Attribute msg
onMouseEnter msg =
    [ E.onMouseEnter msg ]


{-| -}
onMouseLeave : msg -> Attribute msg
onMouseLeave msg =
    [ E.onMouseLeave msg ]


onMouseMove : msg -> Attribute msg
onMouseMove msg =
    [ E.onMouseMove msg ]


{-| -}
onLoseFocus : msg -> Attribute msg
onLoseFocus msg =
    [ E.onLoseFocus msg ]


{-| -}
onFocus : msg -> Attribute msg
onFocus msg =
    [ E.onFocus msg ]
