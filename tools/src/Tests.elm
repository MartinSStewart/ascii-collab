module Tests exposing (..)

import Array
import Ascii exposing (Ascii)
import BackendLogic exposing (Effect(..))
import Bounds exposing (Bounds)
import Change exposing (LocalChange(..))
import Dict
import Element exposing (Element)
import Element.Background
import EverySet
import Grid
import GridCell
import Helper exposing (Coord)
import Html exposing (Html)
import List.Extra as List
import List.Nonempty as Nonempty exposing (Nonempty(..))
import LocalGrid
import LocalModel
import NonemptyExtra as Nonempty
import Quantity exposing (Quantity(..))
import Time
import Types exposing (BackendModel, ClientId, FrontendModel, SessionId, ToBackend(..), ToFrontend(..))
import Units exposing (CellUnit)
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

                    (SendToFrontend clientId head) :: [] ->
                        if clientId == "client0" then
                            case head of
                                LoadingData loadingData ->
                                    let
                                        userId =
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

                    (SendEmail _ _) :: [] ->
                        Failed "Wrong effect"
                 )
                    |> testSingle
                )
            , test "Test undo"
                (LocalGrid.init
                    { user = User.userId 0
                    , grid = Grid.empty
                    , hiddenUsers = EverySet.empty
                    , adminHiddenUsers = EverySet.empty
                    , undoHistory = []
                    , redoHistory = []
                    , undoCurrent = Dict.empty
                    , viewBounds = smallViewBounds
                    }
                    |> testInit
                    |> testMap (LocalGrid.update (time 2) (Change.LocalChange LocalAddUndo))
                    |> testAssert (checkGridValue ( ( Units.cellUnit 0, Units.cellUnit 0 ), 0 ) Nothing)
                    |> testMap
                        (LocalGrid.update (time 0)
                            ({ cellPosition = ( Units.cellUnit 0, Units.cellUnit 0 ), localPosition = 0, change = Nonempty.fromElement asciiA }
                                |> Change.LocalGridChange
                                |> Change.LocalChange
                            )
                        )
                    |> testAssert (checkGridValue ( ( Units.cellUnit 0, Units.cellUnit 0 ), 0 ) (Just asciiA))
                    |> testMap (LocalGrid.update (time 3) (Change.LocalChange LocalUndo))
                    |> testAssert (checkGridValue ( ( Units.cellUnit 0, Units.cellUnit 0 ), 0 ) (Just Ascii.default))
                    |> testMap (LocalGrid.update (time 4) (Change.LocalChange LocalRedo))
                    |> testAssert (checkGridValue ( ( Units.cellUnit 0, Units.cellUnit 0 ), 0 ) (Just asciiA))
                )
            , test "Test undo multiple"
                (LocalGrid.init
                    { user = User.userId 0
                    , grid = Grid.empty
                    , hiddenUsers = EverySet.empty
                    , adminHiddenUsers = EverySet.empty
                    , undoHistory = []
                    , redoHistory = []
                    , undoCurrent = Dict.empty
                    , viewBounds = smallViewBounds
                    }
                    |> testInit
                    |> testMap (LocalGrid.update (time 0) (Change.LocalChange LocalAddUndo))
                    |> testMap
                        (LocalGrid.update (time 1)
                            ({ cellPosition = ( Units.cellUnit 0, Units.cellUnit 0 ), localPosition = 0, change = Nonempty.fromElement asciiA }
                                |> Change.LocalGridChange
                                |> Change.LocalChange
                            )
                        )
                    |> testMap (LocalGrid.update (time 2) (Change.LocalChange LocalAddUndo))
                    |> testMap
                        (LocalGrid.update (time 3)
                            ({ cellPosition = ( Units.cellUnit 0, Units.cellUnit 0 ), localPosition = 1, change = Nonempty.fromElement asciiA }
                                |> Change.LocalGridChange
                                |> Change.LocalChange
                            )
                        )
                    |> testMap (LocalGrid.update (time 4) (Change.LocalChange LocalAddUndo))
                    |> testMap
                        (LocalGrid.update (time 5)
                            ({ cellPosition = ( Units.cellUnit 0, Units.cellUnit 0 ), localPosition = 2, change = Nonempty.fromElement asciiA }
                                |> Change.LocalGridChange
                                |> Change.LocalChange
                            )
                        )
                    |> testAssert (checkGridValue ( ( Units.cellUnit 0, Units.cellUnit 0 ), 0 ) (Just asciiA))
                    |> testAssert (checkGridValue ( ( Units.cellUnit 0, Units.cellUnit 0 ), 1 ) (Just asciiA))
                    |> testAssert (checkGridValue ( ( Units.cellUnit 0, Units.cellUnit 0 ), 2 ) (Just asciiA))
                    |> testMap (LocalGrid.update (time 6) (Change.LocalChange LocalUndo))
                    |> testAssert (checkGridValue ( ( Units.cellUnit 0, Units.cellUnit 0 ), 0 ) (Just asciiA))
                    |> testAssert (checkGridValue ( ( Units.cellUnit 0, Units.cellUnit 0 ), 1 ) (Just asciiA))
                    |> testAssert (checkGridValue ( ( Units.cellUnit 0, Units.cellUnit 0 ), 2 ) (Just Ascii.default))
                    |> testMap (LocalGrid.update (time 7) (Change.LocalChange LocalUndo))
                    |> testAssert (checkGridValue ( ( Units.cellUnit 0, Units.cellUnit 0 ), 0 ) (Just asciiA))
                    |> testAssert (checkGridValue ( ( Units.cellUnit 0, Units.cellUnit 0 ), 1 ) (Just Ascii.default))
                    |> testAssert (checkGridValue ( ( Units.cellUnit 0, Units.cellUnit 0 ), 2 ) (Just Ascii.default))
                    |> testMap (LocalGrid.update (time 8) (Change.LocalChange LocalUndo))
                    |> testAssert (checkGridValue ( ( Units.cellUnit 0, Units.cellUnit 0 ), 0 ) (Just Ascii.default))
                    |> testAssert (checkGridValue ( ( Units.cellUnit 0, Units.cellUnit 0 ), 1 ) (Just Ascii.default))
                    |> testAssert (checkGridValue ( ( Units.cellUnit 0, Units.cellUnit 0 ), 2 ) (Just Ascii.default))
                )
            , test "Don't show changes outside of view bounds"
                (LocalGrid.init
                    { user = User.userId 0
                    , grid = Grid.empty
                    , hiddenUsers = EverySet.empty
                    , adminHiddenUsers = EverySet.empty
                    , undoHistory = []
                    , redoHistory = []
                    , undoCurrent = Dict.empty
                    , viewBounds = Bounds.translate ( Units.cellUnit 1, Units.cellUnit 0 ) smallViewBounds
                    }
                    |> testInit
                    |> testMap
                        (LocalGrid.update (time 0)
                            ({ cellPosition = ( Units.cellUnit 0, Units.cellUnit 0 ), localPosition = 0, change = Nonempty.fromElement asciiA }
                                |> Change.LocalGridChange
                                |> Change.LocalChange
                            )
                        )
                    |> testAssert (checkGridValue ( ( Units.cellUnit 0, Units.cellUnit 0 ), 0 ) Nothing)
                )
            , boundsTest "Bounds fold 0x0"
                [ Helper.fromRawCoord ( 0, 0 ) ]
                (Bounds.bounds (Helper.fromRawCoord ( 0, 0 )) (Helper.fromRawCoord ( 0, 0 )))
            , boundsTest "Bounds fold 2x1"
                (List.reverse [ ( Quantity 0, Quantity 0 ), ( Quantity 1, Quantity 0 ), ( Quantity 2, Quantity 0 ), ( Quantity 0, Quantity 1 ), ( Quantity 1, Quantity 1 ), ( Quantity 2, Quantity 1 ) ])
                (Bounds.bounds (Helper.fromRawCoord ( 0, 0 )) (Helper.fromRawCoord ( 2, 1 )))
            , boundsTest "Bounds fold 1x2"
                (List.reverse [ ( Quantity 0, Quantity 0 ), ( Quantity 1, Quantity 0 ), ( Quantity 0, Quantity 1 ), ( Quantity 1, Quantity 1 ), ( Quantity 0, Quantity 2 ), ( Quantity 1, Quantity 2 ) ])
                (Bounds.bounds (Helper.fromRawCoord ( 0, 0 )) (Helper.fromRawCoord ( 1, 2 )))
            , boundsTest "Bounds fold 1x1"
                (List.reverse [ ( Quantity 0, Quantity 0 ), ( Quantity 1, Quantity 0 ), ( Quantity 0, Quantity 1 ), ( Quantity 1, Quantity 1 ) ])
                (Bounds.bounds (Helper.fromRawCoord ( 0, 0 )) (Helper.fromRawCoord ( 1, 1 )))
            , boundsReverseTest "Bounds reverse fold 0x0"
                [ Helper.fromRawCoord ( 0, 0 ) ]
                (Bounds.bounds (Helper.fromRawCoord ( 0, 0 )) (Helper.fromRawCoord ( 0, 0 )))
            , boundsReverseTest "Bounds reverse fold 2x1"
                [ ( Quantity 0, Quantity 0 ), ( Quantity 1, Quantity 0 ), ( Quantity 2, Quantity 0 ), ( Quantity 0, Quantity 1 ), ( Quantity 1, Quantity 1 ), ( Quantity 2, Quantity 1 ) ]
                (Bounds.bounds (Helper.fromRawCoord ( 0, 0 )) (Helper.fromRawCoord ( 2, 1 )))
            , boundsReverseTest "Bounds reverse fold 1x2"
                [ ( Quantity 0, Quantity 0 ), ( Quantity 1, Quantity 0 ), ( Quantity 0, Quantity 1 ), ( Quantity 1, Quantity 1 ), ( Quantity 0, Quantity 2 ), ( Quantity 1, Quantity 2 ) ]
                (Bounds.bounds (Helper.fromRawCoord ( 0, 0 )) (Helper.fromRawCoord ( 1, 2 )))
            , boundsReverseTest "Bounds reverse fold 1x1"
                [ ( Quantity 0, Quantity 0 ), ( Quantity 1, Quantity 0 ), ( Quantity 0, Quantity 1 ), ( Quantity 1, Quantity 1 ) ]
                (Bounds.bounds (Helper.fromRawCoord ( 0, 0 )) (Helper.fromRawCoord ( 1, 1 )))
            , test "statistics empty 1x1" <|
                (BackendLogic.statistics
                    EverySet.empty
                    (Bounds.bounds ( Quantity 0, Quantity 0 ) ( Quantity 1, Quantity 1 ))
                    Grid.empty
                    |> (\a ->
                            if Nonempty.find (Tuple.first >> (==) Ascii.default) a == Just ( Ascii.default, 4 ) then
                                Passed

                            else
                                Failed "Expected 4 spaces"
                       )
                    |> testSingle
                )
            , test "statistics single a" <|
                (BackendLogic.statistics
                    EverySet.empty
                    (Bounds.bounds ( Quantity 0, Quantity 0 ) ( Quantity -15, Quantity -15 ))
                    (Grid.empty
                        |> Grid.addChange
                            { cellPosition = Helper.fromRawCoord ( 0, 0 )
                            , localPosition = 0
                            , change = Nonempty asciiA []
                            , userId = User.userId 0
                            }
                    )
                    |> (\a ->
                            if Nonempty.find (Tuple.first >> (==) asciiA) a == Just ( asciiA, 1 ) then
                                Passed

                            else
                                Failed "Expected single a"
                       )
                    |> testSingle
                )
            , test "statistics single a at min coord" <|
                let
                    ( cell, local ) =
                        Grid.asciiToCellAndLocalCoord ( Quantity -16, Quantity -16 )
                in
                BackendLogic.statistics
                    EverySet.empty
                    (Bounds.bounds ( Quantity 0, Quantity 0 ) ( Quantity -16, Quantity -16 ))
                    (Grid.empty
                        |> Grid.addChange
                            { cellPosition = cell
                            , localPosition = local
                            , change = Nonempty asciiA []
                            , userId = User.userId 0
                            }
                    )
                    |> (\a ->
                            if Nonempty.find (Tuple.first >> (==) asciiA) a == Just ( asciiA, 1 ) then
                                Passed

                            else
                                Failed "Expected single a"
                       )
                    |> testSingle
            , test "statistics single a at min coord, not aligned vertically" <|
                let
                    ( cell, local ) =
                        Grid.asciiToCellAndLocalCoord ( Quantity -16, Quantity -15 )
                in
                BackendLogic.statistics
                    EverySet.empty
                    (Bounds.bounds ( Quantity 0, Quantity 0 ) ( Quantity -16, Quantity -15 ))
                    (Grid.empty
                        |> Grid.addChange
                            { cellPosition = cell
                            , localPosition = local
                            , change = Nonempty asciiA []
                            , userId = User.userId 0
                            }
                    )
                    |> (\a ->
                            if Nonempty.find (Tuple.first >> (==) asciiA) a == Just ( asciiA, 1 ) then
                                Passed

                            else
                                Failed "Expected single a"
                       )
                    |> testSingle
            , test "statistics single a at min coord, not aligned horziontally" <|
                let
                    ( cell, local ) =
                        Grid.asciiToCellAndLocalCoord ( Quantity -15, Quantity -16 )
                in
                BackendLogic.statistics
                    EverySet.empty
                    (Bounds.bounds ( Quantity 0, Quantity 0 ) ( Quantity -15, Quantity -16 ))
                    (Grid.empty
                        |> Grid.addChange
                            { cellPosition = cell
                            , localPosition = local
                            , change = Nonempty asciiA []
                            , userId = User.userId 0
                            }
                    )
                    |> (\a ->
                            if Nonempty.find (Tuple.first >> (==) asciiA) a == Just ( asciiA, 1 ) then
                                Passed

                            else
                                Failed "Expected single a"
                       )
                    |> testSingle
            ]


boundsTest : String -> List (Coord unit) -> Bounds unit -> Element msg
boundsTest name expected bounds =
    Bounds.coordRangeFold
        (::)
        identity
        bounds
        []
        |> (\a ->
                if a == expected then
                    Passed

                else
                    Failed (Debug.toString a ++ " is incorrect. Expected " ++ Debug.toString expected)
           )
        |> testSingle
        |> test name


boundsReverseTest : String -> List (Coord unit) -> Bounds unit -> Element msg
boundsReverseTest name expected bounds =
    Bounds.coordRangeFoldReverse
        (::)
        identity
        bounds
        []
        |> (\a ->
                if a == expected then
                    Passed

                else
                    Failed (Debug.toString a ++ " is incorrect. Expected " ++ Debug.toString expected)
           )
        |> testSingle
        |> test name


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


smallViewBounds : Bounds Units.CellUnit
smallViewBounds =
    Bounds.bounds ( Units.cellUnit 0, Units.cellUnit 0 ) ( Units.cellUnit 1, Units.cellUnit 1 )


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


checkGridValue : ( Coord CellUnit, Int ) -> Maybe Ascii -> LocalModel.LocalModel a LocalGrid.LocalGrid -> TestResult
checkGridValue ( cellPosition, localPosition ) value =
    LocalGrid.localModel
        >> .grid
        >> Grid.getCell cellPosition
        >> Maybe.andThen (GridCell.flatten EverySet.empty EverySet.empty >> Array.get localPosition >> Maybe.map Tuple.second)
        >> (\ascii ->
                if ascii == value then
                    Passed

                else
                    Failed
                        ("Wrong value found in grid "
                            ++ Debug.toString ascii
                            ++ " at cell position "
                            ++ Debug.toString cellPosition
                            ++ " and local position "
                            ++ Debug.toString localPosition
                        )
           )
