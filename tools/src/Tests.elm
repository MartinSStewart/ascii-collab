module Tests exposing (..)

import BackendLogic exposing (Effect(..))
import Bounds
import Dict
import Element exposing (Element)
import Element.Background
import Html exposing (Html)
import Time
import Types exposing (BackendModel, ClientId, FrontendModel, SessionId, ToBackend(..), ToFrontend(..))
import Units
import User


type TestResult
    = Passed
    | Failed String


main : Html msg
main =
    Element.layout [] <|
        Element.column [ Element.padding 16 ]
            [ test
                "Request Data creates user"
                (let
                    ( model, effect ) =
                        newUserState
                 in
                 case effect of
                    [] ->
                        Failed "No response sent"

                    _ :: _ :: _ ->
                        Failed "Too many responses sent"

                    (Effect clientId head) :: [] ->
                        if clientId == "client0" then
                            case head of
                                LoadingData loadingData ->
                                    let
                                        ( userId, _ ) =
                                            loadingData.user
                                    in
                                    case Dict.get (User.rawId userId) model.users of
                                        Just _ ->
                                            Passed

                                        Nothing ->
                                            Failed "User not found"

                                _ ->
                                    Failed "Wrong ToFrontend msg"

                        else
                            Failed "Response sent to wrong client"
                )
            , testUndo
            ]


test : String -> TestResult -> Element msg
test name testResult =
    Element.row
        [ Element.spacing 8 ]
        [ Element.text name
        , case testResult of
            Failed error ->
                Element.paragraph
                    [ Element.Background.color (Element.rgb 1 0 0), Element.padding 8 ]
                    [ Element.text error ]

            Passed ->
                Element.el
                    [ Element.Background.color (Element.rgb 0 1 0), Element.padding 8 ]
                    (Element.text "Passed")
        ]


newUserState : ( BackendModel, List BackendLogic.Effect )
newUserState =
    BackendLogic.init
        |> BackendLogic.updateFromFrontend
            "session0"
            "client0"
            (RequestData (Bounds.bounds ( Units.cellUnit 0, Units.cellUnit 0 ) ( Units.cellUnit 2, Units.cellUnit 2 )))


type alias Event =
    { eventType : EventType, startAt : Time.Posix, endAt : Time.Posix }


type EventType
    = BackendEffect BackendLogic.Effect
    | FrontendEffect


type alias Simulation =
    { backend : BackendModel, frontend : List ( SessionId, ClientId, FrontendModel ), pendingEvents : List EventType }



--simulate : List Event -> Simulation -> Simulation


testUndo : Element msg
testUndo =
    test "Test undo"
        Passed
