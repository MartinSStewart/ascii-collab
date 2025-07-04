module Fusion.Colors exposing
    ( error
    , errorText
    , orange
    , orangeText
    , purpleText
    , quote
    , quoteText
    , textInputBackground
    , value
    , valueText
    )

import Html exposing (Html)
import Html.Attributes


valueText : String -> Html msg
valueText input =
    colored value input


value : String
value =
    "#4196ad"


quoteText : String -> Html msg
quoteText input =
    colored quote input


quote : String
quote =
    "#73c990"


purpleText : String -> Html msg
purpleText string =
    colored purple string


purple : String
purple =
    "#c677dd"


errorText : String -> Html msg
errorText text =
    colored error text


error : String
error =
    "#fb726d"


textInputBackground : String
textInputBackground =
    "#333"


orangeText : String -> Html msg
orangeText string =
    colored orange string


orange : String
orange =
    "#d19a66"


colored : String -> String -> Html msg
colored color string =
    Html.span [ Html.Attributes.style "color" color ] [ Html.text string ]
