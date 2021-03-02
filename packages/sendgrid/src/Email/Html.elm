module Email.Html exposing (Attribute, Html, a, b, br, div, font, h1, h2, h3, h4, h5, h6, hr, img, inlineGifImg, inlineJpegImg, inlinePngImg, label, li, node, ol, p, span, strong, table, td, text, th, toHtml, toString, tr, u, ul)

import Bytes exposing (Bytes)
import Html
import Internal.Types


type alias Html =
    Internal.Types.Html


type alias Attribute =
    Internal.Types.Attribute


toHtml : Html -> Html.Html msg
toHtml =
    Internal.Types.toHtml


toString : Html -> String
toString =
    Internal.Types.toString >> Tuple.first


node : String -> List Attribute -> List Html -> Html
node =
    Internal.Types.Node


div : List Attribute -> List Html -> Html
div =
    Internal.Types.Node "div"


table : List Attribute -> List Html -> Html
table =
    Internal.Types.Node "table"


tr : List Attribute -> List Html -> Html
tr =
    Internal.Types.Node "tr"


td : List Attribute -> List Html -> Html
td =
    Internal.Types.Node "tr"


th : List Attribute -> List Html -> Html
th =
    Internal.Types.Node "th"


br : List Attribute -> List Html -> Html
br =
    Internal.Types.Node "br"


hr : List Attribute -> List Html -> Html
hr =
    Internal.Types.Node "hr"


a : List Attribute -> List Html -> Html
a =
    Internal.Types.Node "a"


b : List Attribute -> List Html -> Html
b =
    Internal.Types.Node "b"


font : List Attribute -> List Html -> Html
font =
    Internal.Types.Node "font"


h1 : List Attribute -> List Html -> Html
h1 =
    Internal.Types.Node "h1"


h2 : List Attribute -> List Html -> Html
h2 =
    Internal.Types.Node "h2"


h3 : List Attribute -> List Html -> Html
h3 =
    Internal.Types.Node "h3"


h4 : List Attribute -> List Html -> Html
h4 =
    Internal.Types.Node "h4"


h5 : List Attribute -> List Html -> Html
h5 =
    Internal.Types.Node "h5"


h6 : List Attribute -> List Html -> Html
h6 =
    Internal.Types.Node "h6"


img : List Attribute -> List Html -> Html
img =
    Internal.Types.Node "img"


label : List Attribute -> List Html -> Html
label =
    Internal.Types.Node "label"


li : List Attribute -> List Html -> Html
li =
    Internal.Types.Node "li"


ol : List Attribute -> List Html -> Html
ol =
    Internal.Types.Node "ol"


p : List Attribute -> List Html -> Html
p =
    Internal.Types.Node "p"


span : List Attribute -> List Html -> Html
span =
    Internal.Types.Node "span"


strong : List Attribute -> List Html -> Html
strong =
    Internal.Types.Node "strong"


u : List Attribute -> List Html -> Html
u =
    Internal.Types.Node "u"


ul : List Attribute -> List Html -> Html
ul =
    Internal.Types.Node "ul"


{-| If you want to embed a png image within the email body, use this function.
The normal approach of using a base64 string as the image src won't work with emails.
-}
inlinePngImg : Bytes -> List Attribute -> List Html -> Html
inlinePngImg content =
    { content = content
    , imageType = Internal.Types.Png
    }
        |> Internal.Types.InlineImage


{-| If you want to embed a jpeg image within the email body, use this function.
The normal approach of using a base64 string as the image src won't work with emails.
-}
inlineJpegImg : Bytes -> List Attribute -> List Html -> Html
inlineJpegImg content =
    { content = content
    , imageType = Internal.Types.Jpeg
    }
        |> Internal.Types.InlineImage


{-| If you want to embed a gif animation within the email body, use this function.
The normal approach of using a base64 string as the image src won't work with emails.
-}
inlineGifImg : Bytes -> List Attribute -> List Html -> Html
inlineGifImg content =
    { content = content
    , imageType = Internal.Types.Gif
    }
        |> Internal.Types.InlineImage


text : String -> Internal.Types.Html
text =
    Internal.Types.TextNode
