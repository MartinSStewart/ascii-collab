module HtmlExtra exposing (column, columnWithBorder, none, row, rowWithBorder, td, wrappedRow)

import Html exposing (Html)
import Html.Attributes


none : Html msg
none =
    Html.text ""


row : List (Html.Html msg) -> Html msg
row children =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "row"
        , Html.Attributes.style "width" "fit-content"
        , Html.Attributes.style "align-items" "flex-start"
        ]
        children


wrappedRow : List (Html msg) -> Html msg
wrappedRow children =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "row"
        , Html.Attributes.style "width" "fit-content"
        , Html.Attributes.style "flex-wrap" "wrap"
        , Html.Attributes.style "align-items" "flex-start"
        ]
        children


rowWithBorder : List (Html msg) -> Html msg
rowWithBorder children =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "row"
        , Html.Attributes.style "border" "1px white solid"
        , Html.Attributes.style "padding" "2px"
        , Html.Attributes.style "width" "fit-content"
        , Html.Attributes.style "align-items" "flex-start"
        ]
        children


column : List (Html msg) -> Html msg
column children =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "width" "fit-content"
        , Html.Attributes.style "align-items" "flex-start"
        ]
        children


columnWithBorder : List (Html msg) -> Html msg
columnWithBorder children =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "border" "1px white solid"
        , Html.Attributes.style "padding" "2px"
        , Html.Attributes.style "width" "fit-content"
        , Html.Attributes.style "align-items" "flex-start"
        ]
        children


td : List (Html msg) -> Html msg
td children =
    Html.td [ Html.Attributes.style "padding" "2px", Html.Attributes.style "vertical-align" "top" ] children
