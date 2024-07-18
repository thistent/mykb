module Ui.Region exposing
    ( announce
    , announceUrgently
    , aside
    , description
    , footer
    , heading
    , mainContent
    , navigation
    )

import Element.Region as R
import Ui exposing (..)


{-| Screen readers will announce when changes to this element are made.
-}
announce : Attribute msg
announce =
    [ R.announce ]


{-| Screen readers will announce changes to this element and potentially interrupt any other announcement.
-}
announceUrgently : Attribute msg
announceUrgently =
    [ R.announceUrgently ]


{-| -}
aside : Attribute msg
aside =
    [ R.aside ]


{-| Adds an `aria-label`, which is used by accessibility software to identity otherwise unlabeled elements.

A common use for this would be to label buttons that only have an icon.

-}
description : String -> Attribute msg
description str =
    [ R.description str ]


{-| -}
footer : Attribute msg
footer =
    [ R.footer ]


{-| This will mark an element as `h1`, `h2`, etc where possible.

Though it's also smart enough to not conflict with existing nodes.

So, this code

    link [ Region.heading 1 ]
        { url = "http://fruits.com"
        , label = text "Best site ever"
        }

will generate

    <a href="http://fruits.com">
        <h1>Best site ever</h1>
    </a>

-}
heading : Int -> Attribute msg
heading i =
    [ R.heading i ]


{-| -}
mainContent : Attribute msg
mainContent =
    [ R.mainContent ]


{-| -}
navigation : Attribute msg
navigation =
    [ R.navigation ]
