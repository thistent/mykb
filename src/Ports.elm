port module Ports exposing (inPort, incoming, outPort, outgoing)

import Date exposing (Date)
import Json.Decode as JD
import Json.Encode as JE
import Task
import Time
import Types exposing (..)



-- Outgoing --


port outPort : JE.Value -> Cmd msg


encoder : PortMsg -> JE.Value
encoder jm =
    JE.object
        [ ( "cmd", JE.string jm.cmd )
        , ( "payload", JE.string jm.payload )
        ]


outgoing : PortMsg -> Cmd msg
outgoing =
    encoder >> outPort



-- Incoming --


port inPort : (JE.Value -> msg) -> Sub msg


decoder : JD.Decoder PortMsg
decoder =
    JD.map2
        PortMsg
        (JD.field "cmd" JD.string)
        (JD.field "payload" JD.string)


incoming : (Result JD.Error PortMsg -> msg) -> (JD.Value -> msg)
incoming msgFun =
    \v ->
        JD.decodeValue decoder v
            |> msgFun



-- Ports --
-- port saveCache : Encode.Value -> Cmd msg
-- port clearCache : Encode.Value -> Cmd msg
---
-- port cache : Encode.Value -> Cmd msg
-- port fetch : (Encode.Value -> msg) -> Sub msg
-- Main --
