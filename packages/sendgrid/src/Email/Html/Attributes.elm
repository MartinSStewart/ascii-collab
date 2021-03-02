module Email.Html.Attributes exposing
    ( alt
    , attribute
    , backgroundColor
    , backgroundImage
    , backgroundPosition
    , backgroundRepeat
    , backgroundSize
    , border
    , borderBottom
    , borderBottomColor
    , borderBottomStyle
    , borderBottomWidth
    , borderColor
    , borderLeft
    , borderLeftColor
    , borderLeftStyle
    , borderLeftWidth
    , borderRadius
    , borderRight
    , borderRightColor
    , borderRightStyle
    , borderRightWidth
    , borderStyle
    , borderTop
    , borderTopColor
    , borderWidth
    , color
    , fontFamily
    , fontSize
    , fontStyle
    , fontVariant
    , height
    , href
    , letterSpacing
    , lineHeight
    , padding
    , paddingBottom
    , paddingLeft
    , paddingRight
    , paddingTop
    , src
    , style
    , textAlign
    , verticalAlign
    , width
    )

{-| <https://caniuse.email/>
<https://www.pinpointe.com/blog/email-campaign-html-and-css-support>
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


{-| -}
borderRadius : String -> Attribute
borderRadius =
    StyleAttribute "border-radius"


{-| -}
borderBottom : String -> Attribute
borderBottom =
    StyleAttribute "border-bottom"


{-| -}
borderBottomColor : String -> Attribute
borderBottomColor =
    StyleAttribute "border-bottom-color"


{-| -}
borderBottomStyle : String -> Attribute
borderBottomStyle =
    StyleAttribute "border-bottom-style"


{-| -}
borderBottomWidth : String -> Attribute
borderBottomWidth =
    StyleAttribute "border-bottom-width"


{-| -}
borderColor : String -> Attribute
borderColor =
    StyleAttribute "border-color"


{-| -}
borderLeft : String -> Attribute
borderLeft =
    StyleAttribute "border-left"


{-| -}
borderLeftColor : String -> Attribute
borderLeftColor =
    StyleAttribute "border-left-color"


{-| -}
borderLeftStyle : String -> Attribute
borderLeftStyle =
    StyleAttribute "border-left-style"


{-| -}
borderLeftWidth : String -> Attribute
borderLeftWidth =
    StyleAttribute "border-left-width"


{-| -}
borderRight : String -> Attribute
borderRight =
    StyleAttribute "border-right"


{-| -}
borderRightColor : String -> Attribute
borderRightColor =
    StyleAttribute "border-right-color"


{-| -}
borderRightStyle : String -> Attribute
borderRightStyle =
    StyleAttribute "border-right-style"


{-| -}
borderRightWidth : String -> Attribute
borderRightWidth =
    StyleAttribute "border-right-width"


{-| -}
borderStyle : String -> Attribute
borderStyle =
    StyleAttribute "border-style"


{-| -}
borderTop : String -> Attribute
borderTop =
    StyleAttribute "border-top"


{-| -}
borderTopColor : String -> Attribute
borderTopColor =
    StyleAttribute "border-top-color"


{-| -}
borderWidth : String -> Attribute
borderWidth =
    StyleAttribute "border-width"


{-| -}
color : String -> Attribute
color =
    StyleAttribute "color"


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
paddingLeft : String -> Attribute
paddingLeft =
    StyleAttribute "padding-left"


{-| -}
paddingRight : String -> Attribute
paddingRight =
    StyleAttribute "padding-right"


{-| -}
paddingBottom : String -> Attribute
paddingBottom =
    StyleAttribute "padding-bottom"


{-| -}
paddingTop : String -> Attribute
paddingTop =
    StyleAttribute "padding-top"


lineHeight : String -> Attribute
lineHeight =
    StyleAttribute "line-height"


{-| -}
fontSize : String -> Attribute
fontSize =
    StyleAttribute "font-size"


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


{-| -}
verticalAlign : String -> Attribute
verticalAlign =
    Attribute "vertical-align"
