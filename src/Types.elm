module Types exposing (..)

import Array exposing (Array)
import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Date exposing (Date)
import Delay exposing (Delay)
import Dict exposing (Dict)
import Http
import Json.Decode as JD
import Time exposing (Month)
import Ui exposing (..)
import Url



-- Model --


type alias Model =
    { navKey : Nav.Key
    , url : Url.Url
    , page : Page
    , menu : Menu
    , pal : Pal
    , size : Delay Dom.Viewport
    , zone : Time.Zone
    , time : Time.Posix
    , focusMonth : Date --{ year : Int, month : Month }
    , currentSlide : String
    , slides : Dict String (Pal -> Element Msg)
    , docText : String
    , docName : String
    , hemisphere : Orientation
    , selectDate : Maybe Date
    , events : Array WeeklyEvent
    , fontSize : Float
    , dpi : Float
    , portTest : PortMsg
    }


type alias Flags =
    { dpi : Float, time : Int }


type Orientation
    = North
    | South


type Msg
    = GotoPage Page
    | ChangeMenu Menu
    | ChangeColor Pal
    | WindowResize Int Int
    | SceneInfo Dom.Viewport
    | NextSlide String
    | Tick Time.Posix
    | FrameDelta Float
    | AdjustTimeZone Time.Zone
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | RequestDoc Page String
    | ReceiveDoc Page String (Result Http.Error String)
    | ToggleClockOrientation
    | SelectDate (Maybe Date)
    | SetFocusMonth Date
    | FromPort (Result JD.Error PortMsg)
    | ToPort PortMsg


type Page
    = Home
    | Calendar
    | Blog
    | Solutions
    | Settings
    | Graph
    | StayHere


type Menu
    = MenuClosed
    | MenuOpen


type alias Pal =
    { name : String
    , fg : Color
    , bg : Color
    , error : Color
    , link : Color
    , extLink : Color
    }



-- Ports --


type alias PortMsg =
    { cmd : String, payload : String }



-- Events --


type alias Duration =
    { hours : Int, minutes : Int }


{-| every Wednesday 14:30-16:00 UTC
-}
type alias WeeklyEvent =
    { firstDate : Date
    , lastDate : Date
    , startTime : Duration -- Time since midnight UTC
    , duration : Duration -- Length of event
    , exceptions : List (RangeOrSingle Date)
    , title : String
    , description : String
    , color : Color
    }


type RangeOrSingle a
    = Range a a
    | Single a


durMins : Duration -> Int
durMins tod =
    tod.hours * 60 + tod.minutes


eventTimeString : Duration -> Duration -> String
eventTimeString startTime duration =
    let
        h1 =
            startTime.hours
                |> String.fromInt
                |> String.padLeft 2 '0'

        m1 =
            startTime.minutes
                |> String.fromInt
                |> String.padLeft 2 '0'

        h2 =
            (durMins startTime + durMins duration)
                // 60
                |> String.fromInt
                |> String.padLeft 2 '0'

        m2 =
            modBy 60 (durMins startTime + durMins duration)
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    h1 ++ ":" ++ m1 ++ "–" ++ h2 ++ ":" ++ m2 ++ " UTC"


eventsOfDay : Date -> Array WeeklyEvent -> List WeeklyEvent
eventsOfDay date allEvents =
    allEvents
        |> Array.filter
            (\ev ->
                (Date.weekdayNumber ev.firstDate == Date.weekdayNumber date)
                    && Date.isBetween ev.firstDate (Date.add Date.Days 1 ev.lastDate) date
                    && not (isInExceptions date ev.exceptions)
            )
        |> Array.toList


isInExceptions : Date -> List (RangeOrSingle Date) -> Bool
isInExceptions date rds =
    rds |> List.foldl (checkRange date) False


checkRange : Date -> RangeOrSingle Date -> Bool -> Bool
checkRange date range alreadyFound =
    if alreadyFound then
        True

    else
        case range of
            Single a ->
                Date.compare a date == EQ

            Range a b ->
                Date.isBetween a b date


dateRange : Date -> Date -> List Date
dateRange first last =
    Date.range Date.Day 7 first (Date.add Date.Days 1 last)


rangeToStringList : List Date -> List String
rangeToStringList range =
    range |> List.map (Date.format "y-MMMM-dd")



-- Misc --


corners :
    { topLeft : number
    , topRight : number
    , bottomLeft : number
    , bottomRight : number
    }
corners =
    { topLeft = 0
    , topRight = 0
    , bottomLeft = 0
    , bottomRight = 0
    }


edges : { left : number, right : number, top : number, bottom : number }
edges =
    { left = 0, right = 0, top = 0, bottom = 0 }
