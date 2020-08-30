module Main exposing (..)

import Ascii
import Element
import Element.Font
import Html exposing (Html)
import Html.Attributes
import List.Extra as List


main : Html msg
main =
    Element.layout
        [ Element.Font.family [ Element.Font.monospace ], Element.Font.size 16, Element.moveDown 3 ]
        (Ascii.asciiChars
            |> List.greedyGroupsOf 25
            |> List.map
                (List.map
                    (String.fromChar
                        >> Element.text
                        >> Element.el [ Element.width (Element.px 10), Element.height (Element.px 16) ]
                    )
                    >> Element.row []
                )
            |> Element.column [ Element.spacing 2, Element.htmlAttribute <| Html.Attributes.style "white-space" "pre" ]
        )
