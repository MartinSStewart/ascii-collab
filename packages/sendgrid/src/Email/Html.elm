module Email.Html exposing (Attribute, Html, a, br, div, hr, img, inlineGifImg, inlineJpegImg, inlinePngImg, node, table, td, text, th, toHtml, toString, tr)

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


img : List Attribute -> List Html -> Html
img =
    Internal.Types.Node "img"


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
