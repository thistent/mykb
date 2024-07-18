module Main exposing (..)

-- import Pane
-- import GraphView as Gv

import Array
import Browser
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events as Events exposing (onResize)
import Browser.Navigation as Nav
import Calendar exposing (CalendarDate)
import Date exposing (Date)
import Delay exposing (Timer)
import Dict
import Docs exposing (..)
import Ease
import Html exposing (Html)
import Http
import Json.Decode as JD
import Markup exposing (dayView, hBar, iconButton, renderMd)
import Pic
import Ports
import Return exposing (Return)
import Style exposing (..)
import Task
import Time exposing (Month(..), Posix)
import Types exposing (..)
import Ui exposing (..)
import Ui.Background as Bg
import Ui.Border as Border
import Ui.Events as Ev
import Ui.Font as Font
import Url



-- Main --


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view =
            \model ->
                { title = "Gimbalabs"
                , body =
                    [ model.size
                        |> Delay.switch
                            (loadingPage model)
                            (view model)
                    ]
                }
        , subscriptions = subs
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


loadingPage : Model -> Timer -> Html Msg
loadingPage model t =
    let
        ease : Float
        ease =
            ((t.original - t.current) / t.original)
                |> Ease.outQuart

        bg : Color
        bg =
            Style.mix ease
                (rgb 0 0 0)
                model.pal.bg

        fg : Color
        fg =
            Style.mix ease
                (rgb 0 0 0)
                model.pal.link
    in
    layout
        [ fillSpace
        , Bg.color bg
        , Font.size <| round <| model.fontSize * 2
        , Font.family [ Font.serif ]
        ]
    <|
        el [ centerXY, moveUp <| ease * 60.0 ] <|
            column [ centerX, spacing <| round <| model.fontSize / 3 ]
                [ row
                    [ Font.color fg
                    , Font.letterSpacing 1.25
                    ]
                    [ el
                        [ Bg.color fg
                        , height <| px <| round <| model.fontSize * 3
                        , width <| px <| round <| model.fontSize * 3
                        , paddingXY 0 5
                        ]
                      <|
                        el [ centerXY, scale 1.4 ] <|
                            Pic.gimbalogo bg <|
                                model.fontSize
                                    * 1.6
                    , text " GIMBA"
                    , el [ Font.bold ] <| text "LABS"
                    ]
                , el
                    [ Font.color <| Style.mix 0.5 fg bg
                    , centerX
                    , Font.size <| round model.fontSize
                    , Font.letterSpacing 2.5
                    ]
                  <|
                    text "Loading..."
                ]



--Initial State --


init : Flags -> Url.Url -> Nav.Key -> Return Msg Model
init flags url key =
    let
        startDoc : String
        startDoc =
            "Main.md"

        time : Posix
        time =
            Time.millisToPosix flags.time
    in
    Return.return
        { navKey = key
        , url = url
        , page = Blog
        , menu = MenuClosed
        , pal = newspaper -- dark
        , size = Delay.wait 500 Nothing
        , zone = Time.utc
        , time = time
        , focusMonth =
            time |> Date.fromPosix Time.utc
        , currentSlide = "start"
        , slides = Dict.empty
        , docText = ""
        , docName = ""
        , hemisphere = North
        , selectDate = Nothing
        , events =
            Array.fromList <|
                []
        , fontSize = 16 -- 12 * logBase 2 (1 + flags.dpi)
        , dpi = flags.dpi
        , portTest = { cmd = "init", payload = "nothing" }
        }
    <|
        Cmd.batch
            [ Task.perform SceneInfo Dom.getViewport
            , Task.perform AdjustTimeZone Time.here
            , Http.get
                { url = "Notes/" ++ startDoc
                , expect =
                    Http.expectString <| ReceiveDoc StayHere startDoc
                }
            ]



-- Update --


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        GotoPage page ->
            Return.singleton
                { model
                    | page = page
                }

        ChangeMenu menu ->
            Return.singleton { model | menu = menu }

        ChangeColor scheme ->
            Return.singleton
                { model | pal = scheme }

        NextSlide ns ->
            Return.singleton
                { model | currentSlide = ns }

        WindowResize x y ->
            let
                newVp : Viewport
                newVp =
                    sizeToVp (toFloat x) (toFloat y)
            in
            Return.singleton
                { model
                    | size = Delay.update model.size newVp
                }

        SceneInfo viewport ->
            Return.singleton
                { model
                    | size = Delay.update model.size viewport
                }

        Tick time ->
            Return.return
                { model
                    | time = time
                }
            <|
                Cmd.batch
                    [ Task.perform SceneInfo Dom.getViewport
                    ]

        FrameDelta delta ->
            Return.singleton
                { model
                    | size = Delay.tick delta model.size
                }

        AdjustTimeZone zone ->
            Return.singleton { model | zone = zone }

        UrlChanged url ->
            Return.singleton { model | url = url }

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    Return.return model <|
                        Nav.pushUrl model.navKey <|
                            Url.toString url

                Browser.External url ->
                    Return.return model <|
                        Nav.load url

        RequestDoc navTo doc ->
            Return.return
                model
            <|
                Http.get
                    { url = "Notes/" ++ doc
                    , expect =
                        Http.expectString <| ReceiveDoc navTo doc
                    }

        ReceiveDoc navTo doc res ->
            -- https://raw.githubusercontent.com/thistent/gimbalabs/main/src/Markup.elm
            case res of
                Ok p ->
                    Return.singleton
                        { model
                            | docText = p
                            , docName = doc
                            , page =
                                if navTo == StayHere then
                                    model.page

                                else
                                    navTo
                        }

                Err _ ->
                    Return.singleton model

        ToggleClockOrientation ->
            case model.hemisphere of
                North ->
                    Return.singleton
                        { model | hemisphere = South }

                South ->
                    Return.singleton
                        { model | hemisphere = North }

        SelectDate maybeDate ->
            Return.singleton { model | selectDate = maybeDate }

        SetFocusMonth date ->
            Return.singleton { model | focusMonth = date }

        FromPort res ->
            res
                |> Result.mapError
                    (\e -> { cmd = "error", payload = JD.errorToString e })
                |> (\r ->
                        case r of
                            Ok v ->
                                v

                            Err v ->
                                v
                   )
                |> (\v -> Return.singleton { model | portTest = v })

        ToPort pm ->
            Return.return model <| Ports.outgoing pm



-- Subscriptions --


subs : Model -> Sub Msg
subs _ =
    Sub.batch
        [ onResize WindowResize
        , Events.onAnimationFrameDelta FrameDelta
        , Time.every 20 Tick
        , Ports.inPort <| Ports.incoming FromPort
        ]



-- View --


view : Model -> Viewport -> Html Msg
view model vp =
    layout
        [ Font.family [ Font.serif ]
        , Font.color model.pal.fg
        , Font.letterSpacing 0.2
        , Font.size <| round model.fontSize
        , Bg.color model.pal.bg
        , inFront <|
            el
                [ padding <| round <| model.fontSize / 2
                , width fill
                , Bg.color <| addAlpha 0.9 model.pal.bg
                , Style.shadow
                ]
            <|
                titleBar model vp
        ]
    <|
        column
            [ fillSpace
            , Bg.color model.pal.bg
            , spacing <| round <| model.fontSize
            , padding <| round <| model.fontSize * 0.75
            , clip
            , scrollbars
            ]
            [ el [ height <| px <| round <| model.fontSize * 3.5 ] none
            , case model.page of
                Home ->
                    blogView model vp

                Calendar ->
                    calendarPage model vp

                Blog ->
                    blogView model vp

                Solutions ->
                    blogView model vp

                Settings ->
                    settingsView model vp

                Graph ->
                    graphView model vp

                StayHere ->
                    el [ fillSpace ] <| el [ centerXY ] <| text "How'd we get here?"
            , hBar
            ]


navNotes : Model -> Element Msg
navNotes model =
    let
        m : Pal -> Pal -> Model
        m oldPal newPal =
            { model
                | pal =
                    { newPal
                        | bg = Style.mix 0.6 oldPal.bg newPal.bg
                        , fg = Style.mix 0.85 oldPal.bg newPal.fg
                        , link = Style.mix 0.85 oldPal.bg newPal.fg
                    }
            }
    in
    textColumn
        [ fillSpace
        , paddingEach
            { edges
                | top = round <| model.fontSize * 0.5
                , left = round <| model.fontSize * 0.5
                , right = round <| model.fontSize * 0.5
                , bottom = round <| model.fontSize * 2
            }
        ]
        [ wrappedRow
            [ fillSpace
            , spacing <| round <| model.fontSize * 2
            ]
            [ turningPage (m model.pal orangeNote) 0.02 <|
                el [ fillSpace ] <|
                    column [ centerX, spacing <| round model.fontSize ]
                        [ heading model "Learn"
                        , el [] <| text "Starter Kits"
                        , el [] <| text "Plutus"
                        , el [] <| text "Playground"
                        ]
            , turningPage (m model.pal yellowNote) 0 <|
                el [ fillSpace ] <|
                    column [ centerX, spacing <| round model.fontSize ]
                        [ heading model "APIs"
                        , el [] <| text "Dandelion"
                        , el [] <| text "Endpoints"
                        ]
            , turningPage (m model.pal greenNote) -0.02 <|
                el [ fillSpace ] <|
                    column [ centerX, spacing <| round model.fontSize ]
                        [ heading (m model.pal greenNote) "Updates"
                        , iconButton (m model.pal greenNote) (RequestDoc Blog "Updates.md") (Just Pic.location) <| text "Updates"
                        ]
            , turningPage (m model.pal blueNote) 0.01 <|
                el [ fillSpace ] <|
                    column [ centerX, spacing <| round model.fontSize ]
                        [ heading (m model.pal blueNote) "About Us"
                        , iconButton (m model.pal blueNote) (RequestDoc Blog "About.md") (Just Pic.location) <| text "About Us"
                        , iconButton (m model.pal blueNote) (GotoPage Calendar) (Just Pic.location) <| text "Calendar"
                        , el [] <| text "Stake Pool"
                        ]
            ]
        ]


graphView : Model -> Viewport -> Element Msg
graphView model vp =
    el [ centerXY ] <|
        --Gv.view model
        text "Graph View"


calendarPage : Model -> Viewport -> Element Msg
calendarPage model vp =
    rowOrColumn (vp.viewport.width > 1200)
        [ fillSpace
        , spacing <| round <| model.fontSize
        , paddingEach { edges | bottom = round <| model.fontSize }
        ]
        [ el
            [ fillSpace
            , inFront <|
                dayView model vp
            ]
          <|
            calendarView model
        , clockView model
        ]


clockView : Model -> Element Msg
clockView model =
    let
        time :
            { localHours : Int
            , hours : Int
            , minutes : Int
            , seconds : Int
            , millis : Int
            }
        time =
            model.time
                |> (\t ->
                        { localHours = Time.toHour model.zone t
                        , hours = Time.toHour Time.utc t
                        , minutes = Time.toMinute Time.utc t
                        , seconds = Time.toSecond Time.utc t
                        , millis = Time.toMillis Time.utc t
                        }
                   )

        c :
            { hourRotation : Float
            , localHourRotation : Float
            , minuteRotation : Float
            , secondRotation : Float
            , leftLabel : String
            , rightLabel : String
            , picUrl : String
            , hourLines : String
            , op : String
            }
        c =
            case model.hemisphere of
                North ->
                    { hourRotation = 0 - (toFloat time.hours + toFloat time.minutes / 60) / 12 * pi
                    , localHourRotation = 0 - (toFloat time.localHours + toFloat time.minutes / 60) / 12 * pi
                    , minuteRotation = 0 - (toFloat time.minutes + toFloat time.seconds / 60) / 30 * pi
                    , secondRotation = 0 - (toFloat time.seconds + toFloat time.millis / 1000) / 30 * pi
                    , leftLabel = "06"
                    , rightLabel = "18"
                    , picUrl = "assets/earth-north.png"
                    , hourLines = "assets/earth-hours-north.png"
                    , op = "Southern"
                    }

                South ->
                    { hourRotation = (toFloat time.hours + toFloat time.minutes / 60) / 12 * pi
                    , localHourRotation = (toFloat time.localHours + toFloat time.minutes / 60) / 12 * pi
                    , minuteRotation = (toFloat time.minutes + toFloat time.seconds / 60) / 30 * pi
                    , secondRotation = (toFloat time.seconds + toFloat time.millis / 1000) / 30 * pi
                    , leftLabel = "18"
                    , rightLabel = "06"
                    , picUrl = "assets/earth-south.png"
                    , hourLines = "assets/earth-hours-south.png"
                    , op = "Northern"
                    }
    in
    column
        [ spacing <| round model.fontSize
        , padding <| round model.fontSize
        , centerXY
        ]
        [ el [ centerX ] <|
            image
                [ centerXY
                , clip
                , inFront <|
                    image
                        [ rotate c.localHourRotation
                        ]
                        { src = "assets/earth-local-hour.png"
                        , description = "local hour hand"
                        }
                , behindContent <|
                    image
                        [ rotate c.hourRotation
                        , inFront <|
                            image
                                [ rotate c.secondRotation
                                ]
                                { src = "assets/earth-second.png"
                                , description = "second hand"
                                }
                        , inFront <|
                            image
                                [ rotate c.minuteRotation
                                ]
                                { src = "assets/earth-minute.png"
                                , description = "minute hand"
                                }
                        ]
                        { src = c.picUrl
                        , description = "earth clock"
                        }
                ]
                { src = c.hourLines, description = "earth" }
        , el [ centerX ] <|
            iconButton model ToggleClockOrientation Nothing <|
                text ("Flip Clock to " ++ c.op ++ " Hemisphere")
        , el
            [ centerX
            , Font.bold
            ]
          <|
            text "Current Time:"
        , el
            [ centerX
            ]
          <|
            text <|
                String.fromInt time.hours
                    ++ ":"
                    ++ (String.padLeft 2 '0' <| String.fromInt time.minutes)
                    ++ " UTC"
        , el
            [ centerX
            ]
          <|
            text <|
                String.fromInt time.localHours
                    ++ ":"
                    ++ (String.padLeft 2 '0' <| String.fromInt time.minutes)
                    ++ " Local"
        ]


rowOrColumn : Bool -> List (Attribute Msg) -> List (Element Msg) -> Element Msg
rowOrColumn pred attrs els =
    if pred then
        row attrs els
        -- row attrs <| List.intersperse vBar els

    else
        -- column attrs <| List.intersperse hBar els
        column attrs els


blogView : Model -> Viewport -> Element Msg
blogView model vp =
    column
        [ fillSpace
        , spacing <| round <| model.fontSize
        , paddingEach
            { edges
                | top = round <| model.fontSize / 4
                , bottom = round model.fontSize
            }
        ]
    <|
        surroundByFileInfo (model.page == Blog)
            model
            [ renderMd model vp model.docText ]


surroundByFileInfo : Bool -> Model -> List (Element Msg) -> List (Element Msg)
surroundByFileInfo pred model content =
    let
        fileInfo =
            wrappedRow
                [ width fill
                , paddingXY lineSize 0
                , spacing <| round model.fontSize
                ]
                [ wrappedRow [ Font.bold, width fill ]
                    [ text "File:"
                    , text <| "/Notes/" ++ model.docName
                    ]
                , el [ fillSpace ] <| el [ alignRight, alignTop ] <| iconButton model (RequestDoc Blog "Main.md") Nothing <| text "Main.md"
                ]
    in
    if pred then
        [ fileInfo, hBar ] ++ content ++ [ hBar, fileInfo ]

    else
        content


settingsView : Model -> Viewport -> Element Msg
settingsView model vp =
    column [ centerXY, spacing <| round model.fontSize ]
        [ el [ centerX, Font.bold, Font.size <| round <| model.fontSize * 2 ] <| text "Under Construction!"
        , el [ centerXY ] <| text "( Settings )"
        , vp.viewport
            |> (\{ height, width } ->
                    "{ width = "
                        ++ String.fromFloat width
                        ++ ", height = "
                        ++ String.fromFloat height
                        ++ " }"
               )
            |> text
            |> el [ centerX ]
        , model.fontSize
            |> String.fromFloat
            |> text
            |> el [ centerX ]
        , model.dpi
            |> String.fromFloat
            |> text
            |> el [ centerX ]
        ]


calendarView : Model -> Element Msg
calendarView model =
    let
        currentDate : Date
        currentDate =
            model.time
                |> Date.fromPosix Time.utc
    in
    column
        [ width fill
        , Bg.color <| Style.mix 0.075 model.pal.bg model.pal.fg
        , Border.width lineSize
        , Border.roundEach
            { corners
                | topLeft = round <| model.fontSize / 2
                , topRight = round <| model.fontSize / 2
            }
        , Style.shadow
        , scrollbars
        ]
        [ row
            [ width fill
            , spacing <| round <| model.fontSize
            ]
            [ el
                [ padding <| round <| model.fontSize * 0.5
                , alignLeft
                ]
              <|
                iconButton model
                    (SetFocusMonth <| Date.add Date.Months -1 model.focusMonth)
                    (Just Pic.leftArrow)
                    none
            , el [ width fill ] <|
                if (Date.year currentDate == Date.year model.focusMonth) && (Date.month currentDate == Date.month model.focusMonth) then
                    none

                else
                    el [ centerX ] <|
                        (iconButton model
                            (SetFocusMonth <| Date.fromCalendarDate (Date.year currentDate) (Date.month currentDate) 1)
                            Nothing
                         <|
                            text "Return"
                        )
            , el
                [ Font.size <| round <| model.fontSize * 1.5
                , Font.bold
                , paddingXY 0 <| round model.fontSize
                ]
              <|
                text <|
                    Date.format "MMMM y" model.focusMonth
            , el [ width fill ] none
            , el
                [ padding <| round <| model.fontSize * 0.5
                , alignRight
                ]
              <|
                iconButton model
                    (SetFocusMonth <| Date.add Date.Months 1 model.focusMonth)
                    (Just Pic.rightArrow)
                    none
            ]
        , table
            [ width fill
            , Bg.color model.pal.bg
            ]
            { data =
                Calendar.fromDate Nothing model.focusMonth
            , columns =
                [ calColumn model "Sun" 0
                , calColumn model "Mon" 1
                , calColumn model "Tue" 2
                , calColumn model "Wed" 3
                , calColumn model "Thu" 4
                , calColumn model "Fri" 5
                , calColumn model "Sat" 6
                ]
            }
        ]


calColumn : Model -> String -> Int -> { header : Element Msg, width : Length, view : List CalendarDate -> Element Msg }
calColumn model dayName index =
    { header =
        el
            [ width fill
            , Font.bold
            , Bg.color <| Style.mix 0.075 model.pal.bg model.pal.fg
            , Border.widthEach { edges | bottom = lineSize }
            ]
        <|
            el [ centerX ] <|
                text dayName
    , width = fill
    , view =
        \dates ->
            Array.fromList dates
                |> Array.get index
                |> Maybe.map (dayOnCalendar model)
                |> Maybe.withDefault
                    (el
                        [ fillSpace
                        , Bg.color <| rgb 1 0 0
                        ]
                        none
                    )
    }


dayOnCalendar : Model -> CalendarDate -> Element Msg
dayOnCalendar model day =
    let
        date :
            { year : Int
            , month : Time.Month
            , day : Int
            , posix : Time.Posix
            }
        date =
            model.time
                |> (\t ->
                        { year = Time.toYear Time.utc t
                        , month = Time.toMonth Time.utc t
                        , day = Time.toDay Time.utc t
                        , posix = t
                        }
                   )

        isToday : Bool
        isToday =
            Date.compare day.date (Date.fromPosix Time.utc date.posix) == EQ

        isSelected : Bool
        isSelected =
            case model.selectDate of
                Just sd ->
                    Date.compare day.date sd == EQ

                Nothing ->
                    False

        isFocusMonth : Bool
        isFocusMonth =
            (Date.month model.focusMonth == Date.month day.date)
                && (Date.year model.focusMonth == Date.year day.date)

        bg : Color
        bg =
            case ( isSelected, isFocusMonth ) of
                ( True, True ) ->
                    Style.mix 0.5 model.pal.link model.pal.bg

                ( True, False ) ->
                    Style.mix 0.5 model.pal.link <| Style.mix 0.35 model.pal.bg model.pal.fg

                ( False, True ) ->
                    model.pal.bg

                ( False, False ) ->
                    Style.mix 0.35 model.pal.bg model.pal.fg

        internalBorder : Color
        internalBorder =
            if isToday then
                model.pal.extLink

            else
                bg
    in
    el
        [ fillSpace
        , padding <| round <| model.fontSize / 6
        , Border.width 1
        , Border.color <|
            if isToday then
                Style.mix 0.85 bg internalBorder

            else
                model.pal.fg
        , Bg.color <| Style.mix 0.85 bg internalBorder
        , Font.size 10
        ]
    <|
        link
            [ fillSpace
            , Ev.onClick <|
                SelectDate <|
                    if isSelected then
                        Nothing

                    else
                        Just day.date
            ]
            { url = ""
            , label =
                column
                    [ fillSpace
                    , spacing <| round <| model.fontSize / 2 - model.fontSize / 6
                    , Bg.color <| Style.mix 0.1 bg internalBorder
                    ]
                <|
                    el
                        [ Font.size 16
                        , Font.bold
                        , Font.color <| model.pal.bg
                        , Bg.color <| Style.mix 0.75 model.pal.bg model.pal.fg
                        , padding <| lineSize * 2
                        , Border.roundEach
                            { corners
                                | topRight = lineSize * 4
                                , bottomLeft = lineSize * 4
                                , bottomRight = lineSize * 4
                            }
                        ]
                        (day.date
                            |> Date.day
                            |> String.fromInt
                            |> String.padLeft 2 '0'
                            |> text
                        )
                        :: (model.events
                                |> eventsOfDay day.date
                                |> List.map (calendarItem model)
                           )
            }


calendarItem : Model -> WeeklyEvent -> Element Msg
calendarItem model event =
    paragraph
        [ width fill
        , Bg.color <| Style.addAlpha 0.7 <| Style.mix 0.25 model.pal.bg event.color
        , Border.color <| Style.addAlpha 0.7 <| Style.mix 0.5 model.pal.fg event.color
        , Border.width 1
        , Border.rounded <| round <| model.fontSize / 3
        , padding <| round <| model.fontSize / 2 - model.fontSize / 6
        ]
        [ text event.title
        ]


turningPage : Model -> Float -> Element Msg -> Element Msg
turningPage model rot content =
    row
        [ Font.color model.pal.fg
        , Bg.color model.pal.bg
        , Border.roundEach { corners | bottomRight = round <| model.fontSize * 2 }
        , shadow
        , fillSpace
        , rotate rot
        ]
        [ el
            [ fillSpace
            , Bg.color model.pal.bg
            , paddingEach
                { edges
                    | left = round <| model.fontSize * 2
                    , top = round <| model.fontSize * 2
                    , bottom = round <| model.fontSize * 3
                }
            , otherSide <|
                el
                    [ fillSpace
                    , paddingEach
                        { edges
                            | left = round <| model.fontSize * 2
                            , top = round <| model.fontSize * 2
                            , bottom = round <| model.fontSize * 3
                        }
                    , alpha 0.03
                    , style "pointer-events" "none"
                    ]
                <|
                    content
            ]
          <|
            content
        , column
            [ height fill
            , width <| px <| round <| model.fontSize * 2
            ]
            [ el
                [ fillSpace
                , Bg.color model.pal.bg
                ]
                none
            , el
                [ width <| px <| round <| model.fontSize * 2
                , height <| px <| round <| model.fontSize * 2

                --, Bg.color model.pal.bg
                , Border.roundEach { corners | bottomRight = round <| model.fontSize * 2 }
                ]
              <|
                Pic.pageCurl model.pal <|
                    model.fontSize
                        * 2
            ]
        ]


titleBar : Model -> Viewport -> Element Msg
titleBar model vp =
    row
        [ width fill
        , spacing <| round <| model.fontSize / 2
        , case model.menu of
            MenuClosed ->
                batch []

            MenuOpen ->
                below <|
                    column
                        [ alignTop
                        , alignRight
                        , width <| px <| round <| vp.viewport.width * 0.95
                        , height <| px <| round <| vp.viewport.width * 0.95
                        ]
                        [ row [ fillSpace ]
                            [ el
                                [ fillSpace
                                , Ev.onClick <| ChangeMenu MenuClosed
                                ]
                                none
                            , mainMenu model vp
                            ]
                        , el
                            [ fillSpace
                            , Ev.onClick <| ChangeMenu MenuClosed
                            ]
                            none
                        ]
        ]
        [ link
            [ Ev.onClick <| GotoPage Home ]
            { url = ""
            , label =
                el
                    [ Bg.color model.pal.link
                    , height <| px <| round <| model.fontSize * 3
                    , width <| px <| round <| model.fontSize * 3
                    , paddingXY 0 5
                    ]
                <|
                    el [ centerXY, scale 1.4 ] <|
                        Pic.gimbalogo model.pal.bg <|
                            model.fontSize
                                * 1.6
            }
        , el [ fillSpace, paddingXY 0 lineSize ] <|
            row
                [ fillSpace
                , Border.widthEach { edges | top = lineSize, bottom = lineSize }
                , spacing <| round <| model.fontSize
                , paddingXY 0 <| round <| model.fontSize / 2
                ]
                [ row
                    [ spacing <| round <| model.fontSize
                    , width fill
                    , centerY
                    ]
                  <|
                    [ link [ Ev.onClick <| GotoPage Home ]
                        { url = ""
                        , label =
                            row
                                [ Font.size <| round <| model.fontSize * 1.5
                                , Font.color model.pal.link
                                , Font.letterSpacing 1.25
                                ]
                                [ text "GIMBA"
                                , el [ Font.heavy, Font.bold ] <| text "LABS"
                                ]
                        }
                    , el [ alignRight ] <|
                        text <|
                            viewTimeDate vp model.zone model.time
                    , case model.menu of
                        MenuClosed ->
                            iconButton model
                                (ChangeMenu MenuOpen)
                                (Just Pic.menuClosed)
                                none

                        MenuOpen ->
                            iconButton model
                                (ChangeMenu MenuClosed)
                                (Just Pic.menuOpen)
                                none
                    ]
                ]
        ]


mainMenu : Model -> Viewport -> Element Msg
mainMenu model vp =
    el
        [ alignTop
        , alignRight
        , Bg.color <| addAlpha 0.95 <| model.pal.bg
        , Font.size <| round model.fontSize
        , padding <| round <| model.fontSize
        , Style.shadow
        , height <| maximum (round <| vp.viewport.height * 0.75) shrink
        , scrollbarY
        ]
    <|
        column
            [ spacing <| round <| model.fontSize * 0.8
            , Font.letterSpacing 1.25
            ]
            [ iconButton model
                (RequestDoc Home "Home.md")
                (Just Pic.home)
              <|
                text "Home"
            , iconButton model
                (GotoPage Calendar)
                (Just Pic.calendar)
              <|
                text "Calendar"
            , iconButton model
                (RequestDoc Blog "Blog.md")
                (Just Pic.blog)
              <|
                text "Blog"
            , iconButton model
                (RequestDoc Solutions "Solutions.md")
                (Just Pic.solutions)
              <|
                text "Solutions"
            , iconButton model (GotoPage Graph) (Just Pic.dims) <| text "Graph"
            , hBar
            , if model.pal.name == "Newspaper" then
                iconButton model (ChangeColor dark) (Just Pic.dark) <| text "Mode"

              else
                iconButton model (ChangeColor newspaper) (Just Pic.light) <| text "Mode"
            , iconButton model (GotoPage Settings) (Just Pic.settings) <| text "Settings"
            ]


viewTimeDate : Viewport -> Time.Zone -> Time.Posix -> String
viewTimeDate vp zone time =
    if vp.viewport.width < 600 then
        ""

    else
        Date.fromPosix zone time
            |> Date.format "EEEE, MMMM ddd, y"



-- Helper Functions --


sizeToVp : Float -> Float -> Dom.Viewport
sizeToVp w h =
    { scene =
        { width = w
        , height = h
        }
    , viewport =
        { x = 0
        , y = 0
        , width = w
        , height = h
        }
    }
