module EmailTest exposing (main)

import Browser
import Element
import Element.Background
import Element.Input
import Html exposing (Html)
import Html.String
import Html.String.Attributes
import List.Nonempty
import SendGrid
import String.Nonempty exposing (NonemptyString(..))
import UiColors


type alias Model =
    { sendGridKey : String, emailAddress : String, result : Maybe (Result SendGrid.Error ()) }


type Msg
    = UserTypedSendGridKey String
    | UserTypedEmailAddress String
    | EmailResponse (Result SendGrid.Error ())
    | SendEmail


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( { sendGridKey = "", emailAddress = "", result = Nothing }, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserTypedSendGridKey key ->
            ( { model | sendGridKey = key }, Cmd.none )

        EmailResponse result ->
            ( { model | result = Just result }, Cmd.none )

        SendEmail ->
            ( { model | result = Nothing }
            , SendGrid.sendEmail
                EmailResponse
                (SendGrid.apiKey model.sendGridKey)
                (email model.emailAddress)
            )

        UserTypedEmailAddress address ->
            ( { model | emailAddress = address }, Cmd.none )


content : Html.String.Html msg
content =
    Html.String.div
        []
        [ Html.String.img [ Html.String.Attributes.src "https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Ftse1.mm.bing.net%2Fth%3Fid%3DOIP.IO2MvrG7m1-23Q3wPtMKSAHaEK%26pid%3DApi&f=1" ] [] ]


email : String -> SendGrid.Email a
email address =
    { subject = NonemptyString 'T' "est"
    , content = SendGrid.htmlContent content
    , to = List.Nonempty.fromElement address
    , cc = []
    , bcc = []
    , nameOfSender = "test"
    , emailAddressOfSender = "test@test.test"
    }


view : Model -> Html Msg
view model =
    Element.layout [ Element.padding 16 ] <|
        Element.column
            [ Element.spacing 16, Element.width Element.fill ]
            [ Element.Input.text
                []
                { onChange = UserTypedSendGridKey
                , label = Element.Input.labelAbove [] (Element.text "SendGrid key")
                , text = model.sendGridKey
                , placeholder = Nothing
                }
            , Element.Input.text
                []
                { onChange = UserTypedEmailAddress
                , label = Element.Input.labelAbove [] (Element.text "Email address")
                , text = model.emailAddress
                , placeholder = Nothing
                }
            , Element.Input.button
                [ Element.padding 16, Element.Background.color UiColors.button ]
                { onPress = Just SendEmail
                , label = Element.text "Send email"
                }
            , Element.paragraph
                []
                [ Element.text "Result: ", Element.text (Debug.toString model.result) ]
            , Element.column
                [ Element.width Element.fill ]
                [ Element.row [ Element.width Element.fill, Element.spacing 8 ]
                    [ Element.text "Preview"
                    , Element.el
                        [ Element.height <| Element.px 2
                        , Element.width Element.fill
                        , Element.Background.color UiColors.border
                        ]
                        Element.none
                    ]
                , Element.html (Html.String.toHtml content)
                ]
            ]
