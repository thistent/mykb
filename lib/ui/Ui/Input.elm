module Ui.Input exposing
    ( Label
    , Option
    , OptionState
    , Placeholder
    , Thumb
    , button
    , checkbox
    , currentPassword
    , defaultCheckbox
    , defaultThumb
    , email
    , focusedOnLoad
    , labelAbove
    , labelBelow
    , labelHidden
    , labelLeft
    , labelRight
    , multiline
    , newPassword
    , option
    , optionWith
    , placeholder
    , radio
    , radioRow
    , search
    , slider
    , spellChecked
    , text
    , thumb
    , username
    )

import Element as E
import Element.Input as I
import Ui exposing (..)


{-| -}
type alias Label msg =
    I.Label msg


{-| -}
type alias Option value msg =
    I.Option value msg


{-| -}
type alias OptionState =
    I.OptionState


{-| -}
type alias Placeholder msg =
    I.Placeholder msg


{-| -}
type alias Thumb =
    I.Thumb


{-| -}
button :
    List (Attribute msg)
    ->
        { onPress : Maybe msg
        , label : Element msg
        }
    -> Element msg
button a r =
    I.button (batch a) r


{-| -}
checkbox :
    List (Attribute msg)
    ->
        { onChange : Bool -> msg
        , icon : Bool -> Element msg
        , checked : Bool
        , label : Label msg
        }
    -> Element msg
checkbox a rec =
    I.checkbox (batch a) rec


{-| -}
currentPassword :
    List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Placeholder msg)
        , label : Label msg
        , show : Bool
        }
    -> Element msg
currentPassword a =
    I.currentPassword (batch a)


{-| The blue default checked box icon.

You'll likely want to make your own checkbox at some point that fits your design.

-}
defaultCheckbox : Bool -> Element msg
defaultCheckbox =
    I.defaultCheckbox


{-| -}
defaultThumb : Thumb
defaultThumb =
    I.defaultThumb


{-| -}
email :
    List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Placeholder msg)
        , label : Label msg
        }
    -> Element msg
email a =
    I.email (batch a)


{-| Attach this attribute to any `Input` that you would like to be automatically focused when the page loads.

You should only have a maximum of one per page.

-}
focusedOnLoad : Attribute msg
focusedOnLoad =
    [ I.focusedOnLoad ]


{-| -}
labelAbove : List (Attribute msg) -> E.Element msg -> Label msg
labelAbove attrs elem =
    I.labelAbove (batch attrs) elem


{-| -}
labelBelow : List (Attribute msg) -> E.Element msg -> Label msg
labelBelow attrs elem =
    I.labelBelow (batch attrs) elem


{-| -}
labelHidden : String -> Label msg
labelHidden str =
    I.labelHidden str


{-| -}
labelLeft : List (Attribute msg) -> E.Element msg -> Label msg
labelLeft attrs elem =
    I.labelLeft (batch attrs) elem


{-| -}
labelRight : List (Attribute msg) -> E.Element msg -> Label msg
labelRight attrs elem =
    I.labelRight (batch attrs) elem


{-| A multiline text input.

By default it will have a minimum height of one line and resize based on it's contents.

Use `Element.spacing` to change its line-height.

-}
multiline :
    List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Placeholder msg)
        , label : Label msg
        , spellcheck : Bool
        }
    -> Element msg
multiline attrs multi =
    I.multiline (batch attrs) multi


{-| A password input that allows the browser to autofill.

It's `newPassword` instead of just `password` because it gives the browser a hint on what type of password input it is.

A password takes all the arguments a normal `Input.text` would, and also **show**, which will remove the password mask (e.g. `****` vs `pass1234`)

-}
newPassword :
    List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Placeholder msg)
        , label : Label msg
        , show : Bool
        }
    -> Element msg
newPassword attrs pass =
    I.newPassword (batch attrs) pass


{-| Add a choice to your radio element. This will be rendered with the default radio icon.
-}
option : value -> Element msg -> Option value msg
option val txt =
    I.option val txt


{-| Customize exactly what your radio option should look like in different states.
-}
optionWith : value -> (OptionState -> Element msg) -> Option value msg
optionWith val view =
    I.optionWith val view


{-| -}
placeholder : List (Attribute msg) -> E.Element msg -> Placeholder msg
placeholder attrs elem =
    I.placeholder (batch attrs) elem


{-| -}
radio :
    List (Attribute msg)
    ->
        { onChange : option -> msg
        , options : List (Option option msg)
        , selected : Maybe option
        , label : Label msg
        }
    -> Element msg
radio attrs rec =
    I.radio (batch attrs) rec


{-| Same as radio, but displayed as a row
-}
radioRow :
    List (Attribute msg)
    ->
        { onChange : option -> msg
        , options : List (Option option msg)
        , selected : Maybe option
        , label : Label msg
        }
    -> Element msg
radioRow attrs rec =
    I.radioRow (batch attrs) rec


{-| -}
search :
    List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Placeholder msg)
        , label : Label msg
        }
    -> Element msg
search attrs rec =
    I.search (batch attrs) rec


{-| -}
slider :
    List (Attribute msg)
    ->
        { onChange : Float -> msg
        , label : Label msg
        , min : Float
        , max : Float
        , value : Float
        , thumb : Thumb
        , step : Maybe Float
        }
    -> Element msg
slider attrs input =
    I.slider (batch attrs) input


{-| If spell checking is available, this input will be spellchecked.
-}
spellChecked :
    List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Placeholder msg)
        , label : Label msg
        }
    -> Element msg
spellChecked attrs rec =
    I.spellChecked (batch attrs) rec


{-| -}
text :
    List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Placeholder msg)
        , label : Label msg
        }
    -> Element msg
text attrs rec =
    I.text (batch attrs) rec


{-| -}
thumb : List (Attribute Never) -> Thumb
thumb attrs =
    I.thumb (batch attrs)


{-| -}
username :
    List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Placeholder msg)
        , label : Label msg
        }
    -> Element msg
username attrs rec =
    I.username (batch attrs) rec
