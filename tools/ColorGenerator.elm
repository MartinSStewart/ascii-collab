module ColorGenerator exposing (..)

import Element
import Element.Background
import Shaders
import User


main =
    Element.layout [] colors


colors =
    List.range 0 1000
        |> List.map
            (\userId ->
                Element.row
                    []
                    [ Element.el
                        [ Element.Background.color <|
                            Shaders.userColor False <|
                                User.userId userId
                        , Element.width <| Element.px 100
                        ]
                        (Element.text (String.fromInt userId))
                    , Element.el
                        [ Element.Background.color <|
                            Shaders.userColor True <|
                                User.userId userId
                        , Element.width <| Element.px 100
                        ]
                        (Element.text "highlight")
                    ]
            )
        |> Element.column []
