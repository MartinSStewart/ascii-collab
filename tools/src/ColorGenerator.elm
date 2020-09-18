module ColorGenerator exposing (..)

import Element
import Element.Background
import User


main =
    Element.layout [] colors


colors =
    List.range 0 1000
        |> List.map
            (\userId ->
                Element.el
                    [ Element.Background.color <| User.color <| User.userId userId, Element.width <| Element.px 200 ]
                    (Element.text (String.fromInt userId))
            )
        |> Element.column []
