module Tests exposing (..)

import Array
import Ascii
import BackendLogic exposing (Effect(..))
import Bounds
import Change exposing (LocalChange(..))
import Dict
import Element exposing (Element)
import Element.Background
import EverySet
import Grid
import GridCell
import Html exposing (Html)
import List.Nonempty as Nonempty
import LocalGrid
import LocalModel
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
                 (case effect of
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
                    |> testSingle
                )
            , testUndo
            ]


test : String -> Test model -> Element msg
test name (Test testResults _) =
    Element.row
        [ Element.width Element.fill ]
        [ Element.el [ Element.alignTop, Element.padding 8 ] (Element.text name)
        , List.map
            (\testResult ->
                case testResult of
                    Failed error ->
                        Element.paragraph
                            [ Element.Background.color (Element.rgb 1 0 0)
                            , Element.padding 8
                            , Element.width Element.fill
                            ]
                            [ Element.text error ]

                    Passed ->
                        Element.el
                            [ Element.Background.color (Element.rgb 0 1 0)
                            , Element.padding 8
                            , Element.width Element.fill
                            ]
                            (Element.text "Passed")
            )
            testResults
            |> Element.column [ Element.width Element.fill ]
        ]


newUserState : ( BackendModel, List BackendLogic.Effect )
newUserState =
    BackendLogic.init
        |> BackendLogic.updateFromFrontend
            "session0"
            "client0"
            (RequestData smallViewBounds)


smallViewBounds =
    Bounds.bounds ( Units.cellUnit 0, Units.cellUnit 0 ) ( Units.cellUnit 2, Units.cellUnit 2 )


time seconds =
    Time.millisToPosix ((seconds * 1000) + 10000000)


type Test model
    = Test (List TestResult) model


testAssert : (model -> TestResult) -> Test model -> Test model
testAssert assertion (Test results model) =
    Test (results ++ [ assertion model ]) model


testInit : model -> Test model
testInit model =
    Test [] model


testMap : (a -> b) -> Test a -> Test b
testMap mapFunc (Test results model) =
    Test results (mapFunc model)


testSingle : TestResult -> Test ()
testSingle result =
    Test [ result ] ()


asciiA =
    Ascii.fromChar 'a' |> Maybe.withDefault Ascii.default


testUndo : Element msg
testUndo =
    test "Test undo"
        (LocalGrid.init Grid.empty [] [] (User.newUser 0) EverySet.empty [] smallViewBounds
            |> testInit
            |> testMap (LocalGrid.update (time 2) (Change.LocalChange LocalAddUndo))
            |> testAssert (checkGridValue Nothing)
            |> testMap
                (LocalGrid.update (time 0)
                    ({ cellPosition = ( Units.cellUnit 0, Units.cellUnit 0 ), localPosition = 0, change = Nonempty.fromElement asciiA }
                        |> Change.LocalGridChange
                        |> Change.LocalChange
                    )
                )
            |> testAssert (checkGridValue (Just asciiA))
            |> testMap (LocalGrid.update (time 3) (Change.LocalChange LocalUndo))
            |> testAssert (checkGridValue (Just Ascii.default))
            |> testMap (LocalGrid.update (time 4) (Change.LocalChange LocalRedo))
            |> testAssert (checkGridValue (Just asciiA))
        )



--asdf =
--    Test [ Passed, Passed, Passed ]
--        (LocalModel
--            { localModel =
--                LocalGrid
--                    { grid =
--                        Grid
--                            (Dict.fromList
--                                [ ( ( 0, 0 )
--                                  , Cell
--                                        { history = [ { line = Nonempty (Ascii 109) [], position = 0, userId = UserId 0 } ]
--                                        , undoPoint = Dict.fromList [ ( 0, 1 ) ]
--                                        }
--                                  )
--                                ]
--                            )
--                    , hiddenUsers = EverySet (D [])
--                    , otherUsers = []
--                    , redoHistory = [ Dict.fromList [ ( ( 0, 0 ), 1 ) ] ]
--                    , undoHistory = []
--                    , user = ( UserId 0, User { color = Green } )
--                    , viewBounds = Bounds { max = ( Quantity 2, Quantity 2 ), min = ( Quantity 0, Quantity 0 ) }
--                    }
--            , localMsgs = [ ( Posix 10000000, LocalChange (LocalGridChange { cellPosition = ( Quantity 0, Quantity 0 ), change = Nonempty (Ascii 109) [], localPosition = 0 }) ), ( Posix 10002000, LocalChange LocalAddUndo ), ( Posix 10003000, LocalChange LocalUndo ) ]
--            , model = LocalGrid { grid = Grid (Dict.fromList []), hiddenUsers = EverySet (D []), otherUsers = [], redoHistory = [], undoHistory = [], user = ( UserId 0, User { color = Green } ), viewBounds = Bounds { max = ( Quantity 2, Quantity 2 ), min = ( Quantity 0, Quantity 0 ) } }
--            }
--        )


checkGridValue value =
    LocalGrid.localModel
        >> .grid
        >> Grid.getCell ( Units.cellUnit 0, Units.cellUnit 0 )
        >> Maybe.andThen (GridCell.flatten EverySet.empty >> Array.get 0 >> Maybe.map Tuple.second)
        >> (\ascii ->
                if ascii == value then
                    Passed

                else
                    Failed ("Wrong value found in grid " ++ Debug.toString ascii)
           )
