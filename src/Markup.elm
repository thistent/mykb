module Markup exposing (..)

import Browser.Dom exposing (Viewport)
import Date
import Html
import Html.Attributes as Attrs
import Markdown.Block as Md
import Markdown.Html as MdHtml
import Markdown.Parser as Md
import Markdown.Renderer as Md
import Parser exposing (Parser)
import Pic
import Style exposing (..)
import Types exposing (..)
import Ui exposing (..)
import Ui.Background as Bg
import Ui.Border as Border
import Ui.Events as Ev
import Ui.Font as Font


type MdToken
    = Heading Float (List MdToken) (List MdToken)
    | Paragraph (List MdToken)
    | BlockQuote (List MdToken)
    | Text String
    | CodeSpan String
    | Strong (List MdToken)
    | Emphasis (List MdToken)
    | Strike (List MdToken)
    | LineBreak
    | Link { title : Maybe String, destination : String } (List MdToken)
    | Image { alt : String, src : String, title : Maybe String }
    | ListBlock (List MdToken)
    | ListItem { task : Md.Task, content : List MdToken, children : List MdToken }
    | CodeBlock { body : String, language : Maybe String }
    | ThematicBreak
    | Table (List MdToken)
    | TableHeader (List MdToken)
    | TableBody (List MdToken)
    | TableRow (List MdToken)
    | TableCell (Maybe Md.Alignment) (List MdToken)
    | TableHeaderCell (Maybe Md.Alignment) (List MdToken)


splitWhile : (a -> Bool) -> List a -> ( List a, List a )
splitWhile pred =
    let
        sw : List a -> List a -> ( List a, List a )
        sw acc list =
            case list of
                [] ->
                    ( List.reverse acc, [] )

                x :: xs ->
                    if pred x then
                        sw (x :: acc) xs

                    else
                        ( List.reverse acc, list )
    in
    sw []


tokenize : String -> List MdToken
tokenize mdDoc =
    mdDoc
        |> Md.parse
        |> Result.withDefault
            [ Md.Paragraph [ Md.Text "Markdown parsing error!" ] ]
        |> Md.render mdTokenizer
        |> Result.withDefault
            [ Text "Markdown rendering error!" ]


renderMd : Model -> Viewport -> String -> Element Msg
renderMd model vp str =
    str
        |> tokenize
        |> nestHeadings
        |> List.map (renderToken model vp)
        |> textColumn [ fillSpace, spacing <| round <| model.fontSize * 2 ]


mdTokenizer : Md.Renderer MdToken
mdTokenizer =
    { heading =
        {-
           { level : HeadingLevel
           , rawText : String
           , children : List view
           }
           -> view
        -}
        \{ level, children } ->
            case level of
                Md.H1 ->
                    Heading 1 children []

                Md.H2 ->
                    Heading 2 children []

                Md.H3 ->
                    Heading 3 children []

                Md.H4 ->
                    Heading 4 children []

                Md.H5 ->
                    Heading 5 children []

                Md.H6 ->
                    Heading 6 children []
    , paragraph =
        -- List view -> view
        Paragraph
    , blockQuote =
        -- List view -> view
        BlockQuote
    , html =
        -- Renderer (List view -> view)
        MdHtml.oneOf []
    , text =
        -- String -> view
        Text
    , codeSpan =
        -- String -> view
        CodeSpan
    , strong =
        -- List view -> view
        Strong
    , emphasis =
        -- List view -> view
        Emphasis
    , strikethrough =
        -- List view -> view
        Strike
    , hardLineBreak =
        -- view
        LineBreak
    , link =
        {-
           { title : Maybe String
           , destination : String
           }
           -> List view
           -> view
        -}
        Link
    , image =
        {-
           { alt : String
           , src : String
           , title : Maybe String
           }
           -> view
        -}
        Image
    , unorderedList =
        -- List (ListItem view) -> view
        \list ->
            let
                convertListItem : Md.ListItem MdToken -> MdToken
                convertListItem li =
                    case li of
                        Md.ListItem task kids ->
                            let
                                ( content, children ) =
                                    splitWhile isInline kids
                            in
                            ListItem
                                { task = task
                                , content = content
                                , children = children
                                }

                l2 : List MdToken
                l2 =
                    list
                        |> List.map convertListItem
            in
            ListBlock l2
    , orderedList =
        -- Int -> List (List view) -> view
        let
            toListItems : List (List MdToken) -> List MdToken
            toListItems ls =
                ls
                    |> List.map
                        (\l ->
                            let
                                ( content, children ) =
                                    splitWhile isInline l
                            in
                            ListItem
                                { task = Md.NoTask
                                , content = content
                                , children = children
                                }
                        )
        in
        \_ ls -> ListBlock (toListItems ls)
    , codeBlock =
        {- { body : String
           , language : Maybe String
           }
           -> view
        -}
        CodeBlock
    , thematicBreak =
        -- view
        ThematicBreak
    , table =
        -- List view -> view
        Table
    , tableHeader =
        -- List view -> view
        TableHeader
    , tableBody =
        -- List view -> view
        TableBody
    , tableRow =
        -- List view -> view
        TableRow
    , tableCell =
        -- Maybe Alignment -> List view -> view
        TableCell
    , tableHeaderCell =
        -- Maybe Alignment -> List view -> view
        TableHeaderCell
    }


nestHeadings : List MdToken -> List MdToken
nestHeadings tokens =
    case tokens of
        tok :: toks ->
            case tok of
                Heading level title subsections ->
                    let
                        ( inside, outside ) =
                            splitWhile (isUnderHeading tok) toks
                    in
                    Heading level title (subsections ++ nestHeadings inside) :: nestHeadings outside

                _ ->
                    tok :: nestHeadings toks

        [] ->
            []


isUnderHeading : MdToken -> MdToken -> Bool
isUnderHeading h x =
    case h of
        Heading hi _ _ ->
            case x of
                Heading xi _ _ ->
                    hi < xi

                _ ->
                    True

        _ ->
            False


renderToken : Model -> Viewport -> MdToken -> Element Msg
renderToken model vp tok =
    let
        render : List MdToken -> List (Element Msg)
        render =
            List.map (renderToken model vp)
    in
    case tok of
        Heading level title subsections ->
            if level == 1.0 then
                textColumn
                    [ width fill
                    , spacing <| round <| model.fontSize * 2
                    ]
                <|
                    column
                        [ width fill
                        , Bg.color <| Style.mix 0.9 model.pal.fg model.pal.bg
                        , Border.roundEach
                            { corners
                                | topLeft = round <| model.fontSize / 2
                                , topRight = round <| model.fontSize / 2
                            }
                        ]
                        [ paragraph
                            [ paddingXY (round model.fontSize * 2) <| round model.fontSize
                            , width fill
                            , Font.size <| round <| model.fontSize * 1.25
                            , spacing <| round <| model.fontSize
                            , Border.color <| Style.mix 0.5 model.pal.bg model.pal.fg
                            , Border.widthEach { edges | bottom = lineSize * 2 }
                            ]
                            (render title)
                        ]
                        :: render subsections

            else
                textColumn [ width fill ] <|
                    [ paragraph
                        [ Border.widthEach { edges | left = lineSize * 2 }
                        , Border.roundEach
                            { corners
                                | topLeft = round <| model.fontSize / 2
                                , topRight = round <| model.fontSize / 2
                                , bottomRight = round <| model.fontSize / 2
                            }
                        , Border.color <| Style.mix 0.5 model.pal.bg model.pal.fg
                        , paddingXY (round <| model.fontSize * 0.75) <| round model.fontSize
                        , Bg.color <| Style.mix 0.9 model.pal.fg model.pal.bg
                        , Font.size <| round <| model.fontSize * 1.25
                        , spacing <| round <| model.fontSize
                        , width fill
                        ]
                        (render title)
                    , el
                        [ height <| px <| round <| model.fontSize * 2
                        , Border.color <| Style.mix 0.5 model.pal.bg model.pal.fg
                        , Border.widthEach { edges | left = lineSize * 2 }
                        ]
                        none
                    , textColumn
                        [ Border.widthEach { edges | left = lineSize * 2 }
                        , Border.roundEach { corners | bottomLeft = round <| model.fontSize / 2 }
                        , Border.color <| Style.mix 0.5 model.pal.bg model.pal.fg
                        , paddingEach
                            { edges
                                | left = round <| model.fontSize * 0.75
                                , bottom = round <| model.fontSize
                            }
                        , spacing <| round <| model.fontSize * 2
                        , width fill
                        ]
                      <|
                        render subsections
                    ]

        Paragraph toks ->
            paragraph
                [ spacing <| round <| model.fontSize / 2
                , paddingXY lineSize 0
                , width fill
                ]
            <|
                List.map (renderToken model vp) toks

        BlockQuote toks ->
            paragraph
                [ spacing <| round <| model.fontSize / 2
                , width fill
                , Border.color <| Style.mix 0.75 model.pal.fg model.pal.bg
                , Border.widthEach { edges | left = lineSize * 2 }
                , Border.roundEach
                    { corners
                        | topLeft = round <| model.fontSize / 2
                        , bottomLeft = round <| model.fontSize / 2
                    }
                , paddingXY (round <| model.fontSize * 2) 0
                ]
            <|
                List.map (renderToken model vp) toks

        Text str ->
            paragraph [ width fill ]
                [ wrappedRow []
                    [ text str ]
                ]

        CodeSpan str ->
            paragraph
                [ width fill
                , Bg.color <| Style.mix 0.08 model.pal.bg model.pal.fg
                , paddingXY 5 2
                ]
                [ wrappedRow
                    [ Font.family [ Font.monospace ]
                    ]
                    [ text str ]
                ]

        Strong toks ->
            paragraph [ width fill ]
                [ wrappedRow [ Font.bold ] <|
                    List.map (renderToken model vp) toks
                ]

        Emphasis toks ->
            paragraph [ width fill ]
                [ wrappedRow [ Font.italic ] <|
                    List.map (renderToken model vp) toks
                ]

        Strike toks ->
            paragraph [ width fill ]
                [ wrappedRow [ Font.strike ] <|
                    List.map (renderToken model vp) toks
                ]

        LineBreak ->
            el [ width fill ] none

        Link { destination } toks ->
            -- { title, destination }
            iconButton model
                (RequestDoc StayHere destination)
                Nothing
            <|
                paragraph [] <|
                    render toks

        Image { alt, src, title } ->
            column
                [ width <| minimum 300 <| px <| round <| vp.viewport.width * 0.33
                , paddingEach { edges | right = round <| model.fontSize * 2 }
                ]
                [ image
                    [ width fill
                    ]
                    { src = src
                    , description = alt
                    }
                , case title of
                    Just t ->
                        paragraph
                            [ centerX
                            , Bg.color <| Style.mix 0.05 model.pal.bg model.pal.fg
                            , paddingXY (round <| model.fontSize) <| round <| model.fontSize / 2
                            , Font.size <| round <| model.fontSize
                            , Font.bold
                            , Font.italic
                            , width fill
                            , Border.roundEach
                                { corners
                                    | bottomLeft = round <| model.fontSize / 2
                                    , bottomRight = round <| model.fontSize / 4
                                }
                            , spacing <| round <| model.fontSize / 2
                            ]
                            [ text t ]

                    Nothing ->
                        none
                ]

        ListBlock toks ->
            -- ListBlock maybeIndex toks ->
            textColumn
                [ spacing <| round <| model.fontSize
                , paddingEach { edges | top = round <| model.fontSize }
                , width fill
                ]
                (render toks)

        ListItem { task, content, children } ->
            mdItem model task (render content) (render children)

        CodeBlock codeRec ->
            codeRec
                |> renderMeetings model vp
                |> Maybe.withDefault
                    (renderCodeBlock model codeRec)

        ThematicBreak ->
            hBar

        Table toks ->
            {- paragraph
               [ fillSpace
               ]
               [ text <| Debug.toString toks
               ]
            -}
            column
                [ Bg.color <| Style.mix 0.15 model.pal.bg model.pal.fg
                , Border.rounded <| round <| model.fontSize / 2
                , fillSpace
                , clip
                , scrollbars
                ]
                (render toks)

        TableHeader toks ->
            row
                [ fillSpace
                , Border.roundEach
                    { corners
                        | topLeft = round <| model.fontSize / 2
                        , topRight = round <| model.fontSize / 2
                    }
                , Bg.color <| Style.mix 0.15 model.pal.bg model.pal.fg
                ]
                (render toks)

        TableBody toks ->
            column
                [ fillSpace
                , Border.roundEach { corners | bottomRight = round model.fontSize }
                , Border.rounded <| round <| model.fontSize / 2
                , Bg.color <| Style.mix 0.05 model.pal.bg model.pal.fg
                ]
                (render toks)

        TableRow toks ->
            row
                [ fillSpace
                , Border.color <| Style.mix 0.2 model.pal.bg model.pal.fg
                , Border.roundEach
                    { corners
                        | topLeft = round <| model.fontSize / 2
                        , topRight = round <| model.fontSize / 2
                    }
                , Border.widthEach { edges | top = 3 }
                ]
                (render toks
                    |> List.intersperse
                        (el
                            [ height fill
                            , Border.color <| Style.mix 0.2 model.pal.bg model.pal.fg
                            , Border.widthEach { edges | left = 3, top = 3, bottom = 3 }
                            ]
                            none
                        )
                )

        TableCell maybeAlign toks ->
            paragraph
                [ width fill
                , centerY
                , spacing <| round model.fontSize
                , paddingXY (round model.fontSize * 2) <| round model.fontSize
                , Font.justify
                , case maybeAlign of
                    Just arg ->
                        case arg of
                            Md.AlignLeft ->
                                alignLeft

                            Md.AlignCenter ->
                                centerX

                            Md.AlignRight ->
                                alignRight

                    Nothing ->
                        batch []
                ]
            <|
                render toks

        TableHeaderCell maybeAlign toks ->
            paragraph
                [ width fill
                , Font.bold
                , spacing <| round model.fontSize
                , paddingXY (round <| model.fontSize * 2) <| round model.fontSize
                , case maybeAlign of
                    Just arg ->
                        case arg of
                            Md.AlignLeft ->
                                alignLeft

                            Md.AlignCenter ->
                                centerX

                            Md.AlignRight ->
                                alignRight

                    Nothing ->
                        batch []
                ]
                (render toks)


renderMeetings : Model -> Viewport -> { language : Maybe String, body : String } -> Maybe (Element Msg)
renderMeetings model vp codeRec =
    codeRec.language
        |> Maybe.andThen
            (\s ->
                case s of
                    "Meetings.today" ->
                        (model.events
                            |> eventsOfDay (Date.fromPosix model.zone model.time)
                            |> List.map (renderEvent model vp)
                            |> (\es ->
                                    if List.isEmpty es then
                                        [ el [ centerXY ] <| text "No events today!" ]

                                    else
                                        es
                               )
                        )
                            |> column
                                [ spacing (round (model.fontSize * 2))
                                , width fill
                                ]
                            |> Just

                    _ ->
                        Nothing
            )



-- Doc Stuff --


iconButton : Model -> Msg -> Maybe (Color -> Float -> Element Msg) -> Element Msg -> Element Msg
iconButton model msg maybeIcon content =
    let
        isLink : Bool
        isLink =
            case msg of
                GotoPage page ->
                    page /= model.page

                ChangeMenu menu ->
                    menu /= model.menu

                ChangeColor pal ->
                    pal /= model.pal

                RequestDoc page name ->
                    page /= model.page || model.docName /= name

                _ ->
                    True

        isExtLink : Bool
        isExtLink =
            case msg of
                RequestDoc _ str ->
                    let
                        testStr : String
                        testStr =
                            String.left 4 str
                    in
                    if testStr == "http" then
                        True

                    else
                        False

                _ ->
                    False

        color : Color
        color =
            if isLink then
                if isExtLink then
                    model.pal.extLink

                else
                    model.pal.link

            else
                model.pal.fg

        linkStyle : Attribute Msg
        linkStyle =
            batch
                [ paddingXY 0 3
                , Font.bold
                , Font.underline
                , Font.color color
                ]

        linkContent : Element Msg
        linkContent =
            case maybeIcon of
                Just icon ->
                    row
                        []
                        [ icon color <| model.fontSize * 1.6
                        , el [ width <| px <| lineSize * 3 ] none
                        , el [ linkStyle ] <| content
                        ]

                Nothing ->
                    el [ linkStyle ] <| content
    in
    if isLink then
        (if isExtLink then
            newTabLink

         else
            link
        )
            [ Ev.onClick msg
            ]
            { url =
                case msg of
                    RequestDoc _ str ->
                        if isExtLink then
                            str

                        else
                            ""

                    _ ->
                        ""
            , label = linkContent
            }

    else
        linkContent


dayView : Model -> Viewport -> Element Msg
dayView model vp =
    case model.selectDate of
        Nothing ->
            none

        Just date ->
            column
                [ height fill
                , width fill -- <| minimum 500 fill
                , padding <| round <| model.fontSize * 0.75
                , spacing <| round model.fontSize * 2
                , Border.width lineSize
                , Border.roundEach
                    { corners
                        | topLeft = round <| model.fontSize / 2
                        , topRight = round <| model.fontSize / 2
                    }
                , Bg.color <| Style.addAlpha 0.9 model.pal.bg
                , scrollbarY
                ]
                [ column
                    [ spacing <| round <| model.fontSize * 0.65
                    , width fill
                    , inFront <|
                        el
                            [ alignTop
                            , alignRight
                            ]
                        <|
                            iconButton model
                                (SelectDate Nothing)
                                Nothing
                            <|
                                text "Close"
                    ]
                    [ el
                        [ centerX
                        , Font.size <| round <| model.fontSize * 1.5
                        , Font.bold
                        ]
                      <|
                        text <|
                            Date.format "EEEE" date
                    , el
                        [ centerX
                        , Font.size <| round <| model.fontSize * 1.5
                        , Font.bold
                        ]
                      <|
                        text <|
                            Date.format "MMMM ddd, y" date
                    , hBar
                    ]
                , column
                    [ spacing <| round <| model.fontSize * 2
                    , paddingXY (round <| model.fontSize * 0.75) 0
                    , fillSpace
                    ]
                  <|
                    (model.events
                        |> eventsOfDay date
                        |> List.map (renderEvent model vp)
                    )
                        ++ [ el
                                [ alignBottom
                                , alignLeft
                                ]
                             <|
                                iconButton model
                                    (SelectDate Nothing)
                                    Nothing
                                <|
                                    text "Close"
                           ]
                ]


renderEvent : Model -> Viewport -> WeeklyEvent -> Element Msg
renderEvent model vp event =
    textColumn
        [ width fill
        , spacing <| round model.fontSize

        --, paddingEach { edges | left = round model.fontSize }
        , Border.widthEach { edges | left = lineSize * 2 }
        , Border.color <|
            Style.saturate <|
                Style.mix 0.75 event.color model.pal.fg
        , Border.roundEach
            { corners
                | topLeft = round <| model.fontSize / 2
                , bottomLeft = round <| model.fontSize / 2
            }
        ]
        [ paragraph
            [ width fill
            , Font.size <| round <| model.fontSize * 1.25
            , Font.bold
            , paddingXY (round <| model.fontSize * 0.75) <| round model.fontSize
            , spacing <| round <| model.fontSize
            , Border.width 1
            , Border.roundEach
                { corners
                    | topLeft = round <| model.fontSize / 4
                    , topRight = round <| model.fontSize / 2
                    , bottomRight = round <| model.fontSize / 2
                }
            , Bg.color <| Style.addAlpha 0.7 <| Style.mix 0.25 model.pal.bg event.color
            , Border.color <| Style.mix 0.75 model.pal.fg event.color
            ]
            [ text event.title ]
        , paragraph
            [ width fill
            , Font.bold
            , paddingXY (round <| model.fontSize * 0.75) 0
            ]
            [ text <| eventTimeString event.startTime event.duration ]
        , el
            [ paddingXY (round model.fontSize * 2) 0 ]
          <|
            renderMd model vp event.description
        ]


vBar : Element Msg
vBar =
    el
        [ height fill
        , width <| px <| lineSize * 2
        , Border.widthEach
            { edges
                | left = lineSize
                , right = lineSize // 2
            }
        ]
        none


hBar : Element Msg
hBar =
    el
        [ width fill
        , height <| px <| lineSize * 2
        , Border.widthEach
            { edges
                | top = lineSize
                , bottom = lineSize // 2
            }
        ]
        none


notFound : Element Msg
notFound =
    el
        [ centerXY
        , Font.italic
        ]
    <|
        text "Slide not found!"



-- Predicates --


isInline : MdToken -> Bool
isInline tok =
    case tok of
        Text _ ->
            True

        Link _ _ ->
            True

        Image _ ->
            True

        CodeSpan _ ->
            True

        Emphasis _ ->
            True

        Strike _ ->
            True

        _ ->
            False


mdItem : Model -> Md.Task -> List (Element Msg) -> List (Element Msg) -> Element Msg
mdItem model task content children =
    row
        [ width fill
        , spacing <| round <| model.fontSize / 2
        ]
        [ case task of
            Md.NoTask ->
                el
                    [ alignTop
                    , width <| px <| round <| model.fontSize * 1.25
                    , moveDown <| model.fontSize / 16
                    ]
                <|
                    Pic.bullet model.pal.fg model.fontSize

            Md.IncompleteTask ->
                el
                    [ alignTop
                    , width <| px <| round <| model.fontSize * 1.25
                    , moveDown <| model.fontSize / 16
                    ]
                <|
                    Pic.unchecked
                        (Style.mix 0.3 model.pal.bg model.pal.fg)
                        model.fontSize

            Md.CompletedTask ->
                el
                    [ alignTop
                    , width <| px <| round <| model.fontSize * 1.25
                    , moveDown <| model.fontSize / 16
                    ]
                <|
                    Pic.checked model.pal.link model.fontSize

        -- ■▨▣□⬚▨▩●▪
        , paragraph
            [ fillSpace
            , alignTop
            ]
          <|
            content
                ++ children
        ]


renderCodeBlock : Model -> { body : String, language : Maybe String } -> Element Msg
renderCodeBlock model { body, language } =
    el
        [ width fill ]
    <|
        textColumn
            [ Font.family [ Font.monospace ]
            , Bg.color <| Style.mix 0.2 model.pal.bg model.pal.fg
            , width fill -- <| px <| round <| vp.viewport.width * 0.85
            , Border.rounded <| round <| model.fontSize / 2

            --, centerX
            , alignRight
            ]
            [ paragraph
                [ Font.size <| round <| model.fontSize
                , padding <| round <| model.fontSize / 2
                , Bg.color <| Style.mix 0.2 model.pal.bg model.pal.fg
                , Border.rounded <| round <| model.fontSize / 2
                , noSelect
                ]
              <|
                case language of
                    Just l ->
                        [ text l ]

                    Nothing ->
                        []
            , el
                [ width fill
                , Bg.color <|
                    Style.mix 0.05 model.pal.bg model.pal.fg
                , clip
                , scrollbars
                ]
              <|
                column
                    [ Bg.color <|
                        Style.mix 0.05 model.pal.bg model.pal.fg
                    , width fill
                    ]
                    (body
                        |> String.split "\n"
                        |> (\xs ->
                                xs
                                    |> List.reverse
                                    |> splitWhile (\x -> x == "")
                                    |> Tuple.second
                                    |> List.reverse
                                    |> List.indexedMap
                                        (\i str ->
                                            let
                                                bg : { num : Color, text : Color }
                                                bg =
                                                    if modBy 2 i == 0 then
                                                        { num =
                                                            Style.mix 0.1 model.pal.bg model.pal.fg
                                                        , text =
                                                            Style.mix 0.05 model.pal.bg model.pal.fg
                                                        }

                                                    else
                                                        { num =
                                                            Style.mix 0.15 model.pal.bg model.pal.fg
                                                        , text =
                                                            Style.mix 0.1 model.pal.bg model.pal.fg
                                                        }
                                            in
                                            row
                                                [ width fill
                                                ]
                                                [ el
                                                    [ Bg.color bg.num
                                                    , Font.bold
                                                    , padding <| round <| model.fontSize / 2
                                                    , height fill
                                                    ]
                                                    (i
                                                        + 1
                                                        |> String.fromInt
                                                        |> String.padLeft
                                                            (List.length xs
                                                                |> toFloat
                                                                |> logBase 10
                                                                |> (\x -> floor x + 1)
                                                            )
                                                            '0'
                                                        |> (\s -> el [ centerY, noSelect ] (text s))
                                                    )
                                                , row
                                                    [ Bg.color bg.text
                                                    , paddingXY (model.fontSize / 2 |> round) 0
                                                    , height fill
                                                    , width fill
                                                    ]
                                                    [ if str /= "" then
                                                        spaceText str

                                                      else
                                                        spaceText "\n"
                                                    ]
                                                ]
                                        )
                           )
                    )
            ]


spaceText : String -> Element msg
spaceText str =
    html <|
        Html.pre
            [ Attrs.style "padding" "0px"
            , Attrs.style "margins" "0px"
            ]
            [ Html.text str ]
