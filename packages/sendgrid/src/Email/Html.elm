module Email.Html exposing (Attribute, Html, a, br, div, img, inlineImg, node, table, td, text, th, toHtml, toString, tr)

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
    Internal.Types.toString


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


a : List Attribute -> List Html -> Html
a =
    Internal.Types.Node "a"


img : List Attribute -> List Html -> Html
img =
    Internal.Types.Node "img"


inlineImg : { name : String, content : Bytes, mimeType : String } -> List Attribute -> List Html -> Html
inlineImg { name, content, mimeType } =
    { name =
        List.filterMap
            (\( mimeType_, extension ) ->
                if mimeType == mimeType_ && not (String.endsWith extension name) then
                    Just (name ++ extension)

                else
                    Nothing
            )
            [ ( "image/jpeg", ".jpeg" )
            , ( "image/jpg", ".jpg" )
            , ( "image/png", ".png" )
            , ( "image/gif", ".gif" )
            ]
            |> List.head
            |> Maybe.withDefault name
    , content = content
    , mimeType = mimeType
    }
        |> Internal.Types.InlineImage


text : String -> Internal.Types.Html
text =
    Internal.Types.TextNode
