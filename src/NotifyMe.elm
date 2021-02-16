module NotifyMe exposing (Frequency(..), Model, ThreeHours, Validated, confirmSubmit, duration, emailConfirmed, frequencies, frequencyToString, inProgress, init, unsubscribed, unsubscribing, view)

import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Email
import Helper exposing (Coord)
import Html
import Html.Attributes
import Html.Events
import Pixels exposing (Pixels)
import Quantity exposing (Quantity(..), Rate)
import UiColors
import Units exposing (WorldPixel)


type Status
    = Form
    | FormWithError
    | SendingToBackend
    | WaitingOnConfirmation


type Model
    = InProgress InProgressModel
    | Completed
    | BackendError
    | Unsubscribing
    | Unsubscribed


type alias InProgressModel =
    { status : Status
    , email : String
    , frequency : Maybe Frequency
    }


type alias Validated =
    { email : Email.Email
    , frequency : Frequency
    }


type Frequency
    = Every3Hours
    | Every12Hours
    | Daily
    | Weekly
    | Monthly


frequencies : List Frequency
frequencies =
    [ Every3Hours
    , Every12Hours
    , Daily
    , Weekly
    , Monthly
    ]


type ThreeHours
    = ThreeHours Never


duration : Frequency -> Quantity Int ThreeHours
duration frequency =
    case frequency of
        Every3Hours ->
            Quantity 1

        Every12Hours ->
            Quantity 4

        Daily ->
            Quantity 8

        Weekly ->
            Quantity (8 * 7)

        Monthly ->
            Quantity (8 * 30)


frequencyToString : Frequency -> String
frequencyToString frequency =
    case frequency of
        Every3Hours ->
            "Every 3 hours"

        Every12Hours ->
            "Every 12 hours"

        Daily ->
            "Once a day"

        Weekly ->
            "Once a week"

        Monthly ->
            "Once a month"


validate : InProgressModel -> Maybe Validated
validate model =
    Maybe.map2 Validated
        (Email.fromString model.email)
        model.frequency


init : Model
init =
    InProgress { status = Form, email = "", frequency = Nothing }


view :
    { a | devicePixelRatio : Quantity Float (Rate WorldPixel Pixels), windowSize : Coord Pixels }
    -> (Model -> msg)
    -> (Validated -> msg)
    -> msg
    -> Model
    -> Element msg
view config modelChangedMsg submitMsg closeMsg model =
    let
        rawDevicePixelRatio =
            Quantity.unwrap config.devicePixelRatio

        scale : Float
        scale =
            toFloat (round rawDevicePixelRatio) / rawDevicePixelRatio

        width value =
            Html.Attributes.style "width" (String.fromFloat (value * scale) ++ "px")

        height value =
            Element.htmlAttribute <| Html.Attributes.style "height" (String.fromFloat (value * scale) ++ "px")

        spacing value =
            Element.spacing (round (value * scale))
    in
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.behindContent <|
            Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.Events.onClick closeMsg
                , Element.Background.color <| Element.rgba 0 0 0 0.5
                ]
                Element.none
        , Element.Font.family [ Element.Font.typeface "ascii" ]
        , Element.htmlAttribute <|
            Html.Attributes.style
                "font-size"
                (String.fromFloat (18 * scale) ++ "px")
        ]
        (Element.column
            [ padding config.devicePixelRatio 16
            , Element.Border.color <| Element.rgb 0 0 0
            , Element.Background.color <| Element.rgb 1 1 1
            , Element.behindContent <|
                Element.el
                    [ padding config.devicePixelRatio 1
                    , Element.width Element.fill
                    , Element.height Element.fill
                    ]
                    (Element.el
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.Border.color <| Element.rgb 0 0 0
                        , borderWidth config.devicePixelRatio 1
                        ]
                        Element.none
                    )
            , borderWidth config.devicePixelRatio 1
            ]
            (case model of
                InProgress inProgress_ ->
                    [ emailField config.devicePixelRatio inProgress_ (InProgress >> modelChangedMsg)
                    , frequencyField inProgress_ (InProgress >> modelChangedMsg)
                    , Element.row
                        []
                        [ button
                            config.devicePixelRatio
                            (case validate inProgress_ of
                                Just validated ->
                                    submitMsg validated

                                Nothing ->
                                    modelChangedMsg (InProgress { inProgress_ | status = FormWithError })
                            )
                            (case inProgress_.status of
                                Form ->
                                    "Submit"

                                FormWithError ->
                                    "Submit"

                                WaitingOnConfirmation ->
                                    "Try again"

                                SendingToBackend ->
                                    "Sending..."
                            )
                        , Element.el [] (Element.html <| Html.div [ width 16 ] [])
                        , button config.devicePixelRatio closeMsg "Cancel"
                        ]
                    , case inProgress_.status of
                        WaitingOnConfirmation ->
                            Element.paragraph
                                [ paragraphWidth ]
                                [ Element.text "A confirmation email has been sent. Click on the link in it to begin getting notifications. If you don't see it, check your spam folder or try sending it again." ]

                        _ ->
                            Element.none
                    ]

                Completed ->
                    [ Element.paragraph [] [ Element.text "Email confirmed! You should now receive notifications." ]
                    , Element.el
                        [ Element.centerX ]
                        (button config.devicePixelRatio closeMsg "Done")
                    ]

                BackendError ->
                    [ Element.paragraph [] [ Element.text "Something went wrong... try again later maybe?" ]
                    , Element.el
                        [ Element.centerX ]
                        (button config.devicePixelRatio closeMsg "Close")
                    ]

                Unsubscribed ->
                    [ Element.paragraph [] [ Element.text "Your email is successfully unsubscribed." ]
                    , Element.el
                        [ Element.centerX ]
                        (button config.devicePixelRatio closeMsg "Close")
                    ]

                Unsubscribing ->
                    [ Element.paragraph [] [ Element.text "Unsubscribing..." ]
                    , Element.el
                        [ Element.centerX ]
                        (button config.devicePixelRatio closeMsg "Close")
                    ]
            )
        )


noPadding =
    { left = 0, right = 0, top = 0, bottom = 0 }


errorMessage : String -> Element msg
errorMessage message =
    Element.el
        [ Element.Font.color UiColors.error
        , Element.Font.size 16
        , Element.paddingEach { noPadding | top = 4 }
        ]
        (Element.text message)


confirmSubmit : { isSuccessful : Bool } -> Model -> Model
confirmSubmit { isSuccessful } model =
    case ( model, isSuccessful ) of
        ( InProgress inProgress_, True ) ->
            InProgress { inProgress_ | status = WaitingOnConfirmation }

        ( InProgress _, False ) ->
            BackendError

        _ ->
            model


inProgress : Model -> Model
inProgress model =
    case model of
        InProgress inProgress_ ->
            InProgress { inProgress_ | status = SendingToBackend }

        Completed ->
            Completed

        BackendError ->
            BackendError

        Unsubscribed ->
            Unsubscribed

        Unsubscribing ->
            Unsubscribing


emailConfirmed : Model -> Model
emailConfirmed model =
    case model of
        InProgress _ ->
            Completed

        Completed ->
            Completed

        BackendError ->
            BackendError

        Unsubscribed ->
            Unsubscribed

        Unsubscribing ->
            Unsubscribing


unsubscribed : Model -> Model
unsubscribed _ =
    Unsubscribed


unsubscribing : Model -> Model
unsubscribing _ =
    Unsubscribing


field : Element msg -> Element msg -> Element msg
field error fieldPart =
    Element.el
        [ Element.width Element.fill, Element.paddingEach { noPadding | bottom = 16 } ]
        (Element.el
            [ Element.below error
            , Element.width Element.fill
            ]
            fieldPart
        )


emailField : Quantity Float (Rate WorldPixel Pixels) -> InProgressModel -> (InProgressModel -> msg) -> Element msg
emailField devicePixelRatio model modelChangedMsg =
    let
        rawDevicePixelRatio =
            Quantity.unwrap devicePixelRatio

        scale : Float
        scale =
            toFloat (round rawDevicePixelRatio) / rawDevicePixelRatio
    in
    field
        (if model.status == FormWithError && Email.fromString model.email == Nothing then
            errorMessage "Invalid email address"

         else
            Element.none
        )
        (Element.el
            [ borderWidth devicePixelRatio 1, padding devicePixelRatio 1, Element.width Element.fill ]
            (Element.html
                (Html.input
                    [ Html.Events.onInput (\email -> modelChangedMsg { model | email = email })
                    , Html.Attributes.value model.email
                    , Html.Attributes.style
                        "font-size"
                        (String.fromFloat (18 * scale) ++ "px")
                    , Html.Attributes.type_ "email"
                    , Html.Attributes.style "font-family" "ascii"
                    , borderWidthHtml devicePixelRatio 1
                    , Html.Attributes.style "border-color" "#000"
                    , Html.Attributes.style "outline" "none"
                    , Html.Attributes.style "appearance" "none"
                    , Html.Attributes.style "-moz-appearance" "none"
                    , Html.Attributes.style "-webkit-appearance" "none"
                    , Html.Attributes.style "border-image-width" "0"
                    , paddingHtml devicePixelRatio 2
                    ]
                    []
                )
            )
        )


paragraphWidth =
    Element.width <| Element.maximum 600 Element.fill


privacyPolicy : Element msg
privacyPolicy =
    Element.paragraph
        [ paragraphWidth ]
        [ Element.el [ Element.Font.bold ] (Element.text "Privacy policy: ")
        , Element.text "Your email is only used for notifications. If this privacy policy changes, an email will be sent out in advance."
        ]


frequencyField : InProgressModel -> (InProgressModel -> msg) -> Element msg
frequencyField model modelChangedMsg =
    field
        (if model.status == FormWithError && model.frequency == Nothing then
            errorMessage "Pick an option"

         else
            Element.none
        )
        (Element.Input.radio [ Element.spacing 8 ]
            { onChange = \frequency -> modelChangedMsg { model | frequency = Just frequency }
            , options =
                List.map
                    (\option -> Element.Input.option option (Element.text (frequencyToString option)))
                    frequencies
            , selected = model.frequency
            , label =
                Element.Input.labelAbove
                    [ Element.paddingEach { noPadding | bottom = 12 } ]
                    (Element.paragraph
                        []
                        [ Element.text "How often do you want to be shown changes people have made?" ]
                    )
            }
        )


button : Quantity Float (Rate WorldPixel Pixels) -> msg -> String -> Element msg
button devicePixelRatio onPress labelText =
    Element.el
        [ padding devicePixelRatio 1, borderWidth devicePixelRatio 1 ]
        (Element.Input.button
            [ padding devicePixelRatio 8
            , borderWidth devicePixelRatio 1
            , Element.mouseOver [ Element.Border.color <| Element.rgb 0 0 0 ]
            , Element.focused [ Element.Border.color <| Element.rgb 0 0 0 ]
            , Element.Border.color <| Element.rgba 0 0 0 0
            ]
            { onPress = Just onPress
            , label = Element.text labelText
            }
        )


paddingHtml devicePixelRatio value =
    let
        rawDevicePixelRatio =
            Quantity.unwrap devicePixelRatio

        scale : Float
        scale =
            toFloat (round rawDevicePixelRatio) / rawDevicePixelRatio
    in
    Html.Attributes.style "padding" (String.fromFloat (value * scale) ++ "px")


padding devicePixelRatio value =
    paddingHtml devicePixelRatio value |> Element.htmlAttribute


borderWidthHtml : Quantity Float (Rate WorldPixel Pixels) -> Float -> Html.Attribute msg
borderWidthHtml devicePixelRatio value =
    let
        rawDevicePixelRatio =
            Quantity.unwrap devicePixelRatio

        scale : Float
        scale =
            toFloat (round rawDevicePixelRatio) / rawDevicePixelRatio
    in
    Html.Attributes.style "border-width" (String.fromFloat (value * scale) ++ "px")


borderWidth devicePixelRatio value =
    borderWidthHtml devicePixelRatio value |> Element.htmlAttribute
