module Ui.Util exposing (..)

import Color as C
import Element as E


cc : C.Color -> E.Color
cc =
    C.toRgba >> E.fromRgb
