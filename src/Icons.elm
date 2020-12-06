module Icons exposing (dragTool, highlightTool)

import Element exposing (Element)
import Svg
import Svg.Attributes exposing (..)


dragTool : Element msg
dragTool =
    Svg.svg
        [ viewBox "0 0 200 200", height "22" ]
        [ Svg.path
            [ d
                "m100 1.2c3.08 0 34.09 41.2 38 48.8h-25v37h37v-25s48.8 34.64 48.8 37.95c0 3.07-48.8 38.05-48.8 38.05v-25h-37v37h25s-34.77 48.8-37.95 48.8c-3.14 0-38.05-48.8-38.05-48.8h25v-37h-36v27c-5.61-2.18-49.8-34.94-49.8-38s44.19-35.82 49.8-38v23h36v-37h-25s34.94-48.8 38-48.8z"
            ]
            []
        ]
        |> Element.html


highlightTool : Element msg
highlightTool =
    Svg.svg [ viewBox "100 100 450 450", width "24" ]
        [ Svg.path [ d "M75.8 115.55h290.95V252H75.8V115.55z", fill "#fe5555" ] []
        , Svg.path [ d "M174.53 252h290.94v136.45H174.53V252z", fill "#73449e" ] []
        , Svg.path [ d "M75.8 388.45H564.2v136H75.8v-136z", fill "#09dc95" ] []
        ]
        |> Element.html
