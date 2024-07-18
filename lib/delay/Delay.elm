module Delay exposing (Delay, Timer, forceReady, isReady, payload, reset, switch, tick, timer, update, wait)


type Delay a
    = Wait Timer (Maybe a)
    | Ready Float a


type alias Timer =
    { current : Float
    , original : Float
    }


isReady : Delay a -> Bool
isReady delay =
    case delay of
        Wait _ _ ->
            False

        Ready _ _ ->
            True


wait : Float -> Maybe a -> Delay a
wait time content =
    Wait (Timer time time) content


payload : Delay a -> Maybe a
payload delay =
    case delay of
        Wait _ mc ->
            mc

        Ready _ c ->
            Just c


forceReady : Delay a -> a -> Delay a
forceReady delay content =
    case delay of
        Wait tmr _ ->
            Ready tmr.original content

        Ready orig _ ->
            Ready orig content


update : Delay b -> b -> Delay b
update delay content =
    case delay of
        Wait tmr _ ->
            Wait tmr (Just content)

        Ready orig _ ->
            Ready orig content


tick : Float -> Delay a -> Delay a
tick delta delay =
    case delay of
        Wait tmr content ->
            let
                newTime : Float
                newTime =
                    tmr.current - delta
            in
            if newTime > 0 then
                Wait (Timer newTime tmr.original) content

            else
                case content of
                    Just c ->
                        Ready tmr.original c

                    Nothing ->
                        Wait (Timer tmr.original 0) Nothing

        Ready orig p ->
            Ready orig p


switch : (Timer -> a) -> (b -> a) -> Delay b -> a
switch notReadyState readyState delay =
    case delay of
        Wait tmr _ ->
            notReadyState tmr

        Ready _ c ->
            readyState c


timer : Delay b -> Maybe Timer
timer delay =
    case delay of
        Wait tmr _ ->
            Just tmr

        Ready _ _ ->
            Nothing


reset : Delay b -> Delay b
reset delay =
    case delay of
        Wait tmr mc ->
            Wait (Timer tmr.original tmr.original) mc

        Ready orig c ->
            Wait (Timer orig orig) (Just c)
