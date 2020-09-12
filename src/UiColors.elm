module UiColors exposing (adminText, background, border, button, buttonActive, colorSquareBorder, text, warning)

import Element


background : Element.Color
background =
    Element.rgb255 235 230 230


border : Element.Color
border =
    Element.rgb255 50 50 55


colorSquareBorder : Element.Color
colorSquareBorder =
    Element.rgba 0 0 0 0.2


buttonActive : Element.Color
buttonActive =
    Element.rgb255 240 240 240


button : Element.Color
button =
    Element.rgb255 190 180 170


text : Element.Color
text =
    Element.rgb255 0 0 0


adminText : Element.Color
adminText =
    Element.rgb255 255 0 0


warning : Element.Color
warning =
    Element.rgb255 255 210 212
