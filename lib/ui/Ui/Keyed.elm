module Ui.Keyed exposing (column, el, row)

import Element.Keyed as K
import Ui exposing (..)


{-| -}
el : List (Attribute msg) -> ( String, Element msg ) -> Element msg
el attrs child =
    K.el (batch attrs) child


{-| -}
row : List (Attribute msg) -> List ( String, Element msg ) -> Element msg
row attrs children =
    K.row (batch attrs) children


{-| -}
column : List (Attribute msg) -> List ( String, Element msg ) -> Element msg
column attrs children =
    K.column (batch attrs) children
