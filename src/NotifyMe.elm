module NotifyMe exposing (Frequency(..), Model, Validated, confirmSubmit, emailConfirmed, frequencyToString, init, view)

import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Email
import UiColors


type Status
    = Form
    | FormWithError
    | WaitingOnConfirmation


type Model
    = InProgress InProgressModel
    | Completed


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


view : (Model -> msg) -> (Validated -> msg) -> msg -> Model -> Element msg
view modelChangedMsg submitMsg closeMsg model =
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
        ]
        (case model of
            InProgress inProgress ->
                form (InProgress >> modelChangedMsg) submitMsg closeMsg inProgress

            Completed ->
                Element.column
                    [ Element.centerX
                    , Element.centerY
                    , Element.padding 16
                    , Element.Background.color UiColors.background
                    , Element.Border.color UiColors.border
                    , Element.Border.width 1
                    , Element.Border.rounded 4
                    , Element.spacing 16
                    ]
                    [ Element.text "Email confirmed! You should now receive notifications."
                    , Element.Input.button
                        buttonStyle
                        { onPress = Just closeMsg
                        , label = Element.text "Done"
                        }
                    ]
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


confirmSubmit : Model -> Model
confirmSubmit model =
    case model of
        InProgress inProgress ->
            InProgress { inProgress | status = WaitingOnConfirmation }

        Completed ->
            Completed


emailConfirmed : Model -> Model
emailConfirmed model =
    case model of
        InProgress _ ->
            Completed

        Completed ->
            Completed


form : (InProgressModel -> msg) -> (Validated -> msg) -> msg -> InProgressModel -> Element msg
form modelChangedMsg submitMsg closeMsg model =
    Element.column
        [ Element.centerX
        , Element.centerY
        , Element.padding 16
        , Element.Background.color UiColors.background
        , Element.Border.color UiColors.border
        , Element.Border.width 1
        , Element.Border.rounded 4
        , Element.spacing 16
        ]
        [ emailField model modelChangedMsg
        , frequencyField model modelChangedMsg
        , Element.row
            [ Element.centerX, Element.spacing 16 ]
            [ Element.Input.button
                buttonStyle
                { onPress =
                    Just <|
                        case validate model of
                            Just validated ->
                                submitMsg validated

                            Nothing ->
                                modelChangedMsg { model | status = FormWithError }
                , label =
                    case model.status of
                        Form ->
                            Element.text "Submit"

                        FormWithError ->
                            Element.text "Submit"

                        WaitingOnConfirmation ->
                            Element.text "Try again"
                }
            , Element.Input.button
                buttonStyle
                { onPress = Just closeMsg, label = Element.text "Cancel" }
            ]
        , case model.status of
            WaitingOnConfirmation ->
                Element.paragraph
                    []
                    [ Element.text "An confirmation email has been sent. Click on the link in it to begin getting notifications. If you don't see it, check your spam folder or try sending it again." ]

            _ ->
                Element.none
        ]


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


emailField : InProgressModel -> (InProgressModel -> msg) -> Element msg
emailField model modelChangedMsg =
    field
        (if model.status == FormWithError && Email.fromString model.email == Nothing then
            errorMessage "Invalid email address"

         else
            Element.none
        )
        (Element.Input.email [ Element.width Element.fill ]
            { onChange = \email -> modelChangedMsg { model | email = email }
            , text = model.email
            , placeholder = Nothing
            , label =
                Element.Input.labelAbove
                    [ Element.paddingEach { noPadding | bottom = 8 } ]
                    (Element.text "Email address")
            }
        )


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
                    [ Every3Hours, Every12Hours, Daily, Weekly, Monthly ]
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


buttonStyle =
    [ Element.padding 8, Element.Background.color UiColors.button ]
