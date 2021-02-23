module Email.Html.Attributes exposing (alt, attribute, backgroundColor, backgroundImage, backgroundPosition, backgroundRepeat, backgroundSize, border, borderRadius, fontColor, fontFamily, fontStyle, fontVariant, height, href, letterSpacing, lineHeight, margin, padding, src, style, textAlign, width)

{-| <https://caniuse.email/>
-}

import Internal.Types exposing (Attribute(..))


{-| Use this if there's a style you want to add that isn't present in this module.
Note that there's an increased risk that it isn't supported in many email clients.
-}
style : String -> String -> Attribute
style =
    StyleAttribute


{-| Use this if there's a attribute you want to add that isn't present in this module.
Note that there's an increased risk that it isn't supported in many email clients.
-}
attribute : String -> String -> Attribute
attribute =
    Attribute


{-| -}
fontColor : String -> Attribute
fontColor =
    StyleAttribute "color"


{-| -}
backgroundColor : String -> Attribute
backgroundColor =
    StyleAttribute "background-color"


{-| -}
backgroundImage : String -> Attribute
backgroundImage =
    StyleAttribute "background-image"


{-| -}
backgroundPosition : String -> Attribute
backgroundPosition =
    StyleAttribute "background-position"


{-| -}
backgroundRepeat : String -> Attribute
backgroundRepeat =
    StyleAttribute "background-repeat"


{-| -}
backgroundSize : String -> Attribute
backgroundSize =
    StyleAttribute "background-size"


{-| -}
border : String -> Attribute
border =
    StyleAttribute "border"


borderRadius : String -> Attribute
borderRadius =
    StyleAttribute "border-radius"


{-| -}
width : String -> Attribute
width =
    StyleAttribute "width"


{-| -}
maxWidth : String -> Attribute
maxWidth =
    StyleAttribute "max-width"


{-| -}
minWidth : String -> Attribute
minWidth =
    StyleAttribute "min-width"


{-| -}
height : String -> Attribute
height =
    StyleAttribute "height"


{-| -}
padding : String -> Attribute
padding =
    StyleAttribute "padding"


{-| -}
margin : String -> Attribute
margin =
    StyleAttribute "margin"


lineHeight : String -> Attribute
lineHeight =
    StyleAttribute "line-height"


{-| -}
fontFamily : String -> Attribute
fontFamily =
    StyleAttribute "font-family"


{-| -}
fontStyle : String -> Attribute
fontStyle =
    StyleAttribute "font-style"


{-| -}
fontVariant : String -> Attribute
fontVariant =
    StyleAttribute "font-variant"


{-| -}
letterSpacing : String -> Attribute
letterSpacing =
    StyleAttribute "letter-spacing"


{-| -}
textAlign : String -> Attribute
textAlign =
    StyleAttribute "text-align"


{-| -}
src : String -> Attribute
src =
    Attribute "src"


{-| -}
alt : String -> Attribute
alt =
    Attribute "alt"


{-| -}
href : String -> Attribute
href =
    Attribute "href"
