module Ui.Lazy exposing (lazy, lazy2, lazy3, lazy4, lazy5)

import Element.Lazy as L
import Ui exposing (..)


{-| -}
lazy : (a -> Element msg) -> a -> Element msg
lazy =
    L.lazy


{-| -}
lazy2 : (a -> b -> Element msg) -> a -> b -> Element msg
lazy2 =
    L.lazy2


{-| -}
lazy3 : (a -> b -> c -> Element msg) -> a -> b -> c -> Element msg
lazy3 =
    L.lazy3


{-| -}
lazy4 : (a -> b -> c -> d -> Element msg) -> a -> b -> c -> d -> Element msg
lazy4 =
    L.lazy4


{-| -}
lazy5 : (a -> b -> c -> d -> e -> Element msg) -> a -> b -> c -> d -> e -> Element msg
lazy5 =
    L.lazy5
