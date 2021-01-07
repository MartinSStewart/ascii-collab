module Tests exposing (Test(..), TestResult(..), asciiA, boundsReverseTest, boundsTest, checkGridValue, main, newUserState, smallViewBounds, test, testAssert, testInit, testMap, testSingle, time)

import Array
import Ascii exposing (Ascii)
import BackendLogic exposing (Effect(..))
import Bounds exposing (Bounds)
import Change exposing (LocalChange(..))
import Dict
import Duration
import Element exposing (Element)
import Element.Background
import Email
import EverySet
import Frontend
import Grid
import GridCell
import Helper exposing (Coord)
import Html exposing (Html)
import Hyperlink exposing (Hyperlink)
import List.Nonempty as Nonempty exposing (Nonempty(..))
import LocalGrid
import LocalModel
import NonemptyExtra as Nonempty
import NotifyMe exposing (Frequency(..))
import Parser
import Quantity exposing (Quantity(..))
import Time
import Types exposing (BackendModel, BackendMsg(..), FrontendModel, ToBackend(..), ToFrontend(..))
import Units exposing (CellUnit)
import UrlHelper exposing (ConfirmEmailKey(..))
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
            , boundsTest "Bounds fold 1x1 offset"
                [ ( Quantity 0, Quantity -1 ), ( Quantity -1, Quantity -1 ), ( Quantity 0, Quantity -2 ), ( Quantity -1, Quantity -2 ) ]
                (Bounds.bounds (Helper.fromRawCoord ( -1, -2 )) (Helper.fromRawCoord ( 0, -1 )))
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
            , test "statistics single a at min coord, not aligned horizontally" <|
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
            , test "statistics single a outside below" <|
                let
                    ( cell, local ) =
                        Grid.asciiToCellAndLocalCoord ( Quantity 1, Quantity 0 )
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
                            if Nonempty.find (Tuple.first >> (==) asciiA) a == Just ( asciiA, 0 ) then
                                Passed

                            else
                                Failed "Expected no a"
                       )
                    |> testSingle
            , test "statistics single a outside to the right" <|
                let
                    ( cell, local ) =
                        Grid.asciiToCellAndLocalCoord ( Quantity 0, Quantity 1 )
                in
                BackendLogic.statistics
                    EverySet.empty
                    (Bounds.bounds ( Quantity 0, Quantity 0 ) ( Quantity -1, Quantity -1 ))
                    (Grid.empty
                        |> Grid.addChange
                            { cellPosition = cell
                            , localPosition = local
                            , change = Nonempty asciiA []
                            , userId = User.userId 0
                            }
                    )
                    |> (\a ->
                            if Nonempty.find (Tuple.first >> (==) asciiA) a == Just ( asciiA, 0 ) then
                                Passed

                            else
                                Failed "Expected no a"
                       )
                    |> testSingle
            , test "Parse no hyperlink" <| parseHyperlinkTest "test" []
            , test "Parse coordinate hyperlink" <|
                parseHyperlinkTest "testx=-5&y=99"
                    [ { position = ( Quantity 4, Quantity.zero )
                      , length = String.length "x=-5&y=99"
                      , route = Hyperlink.Coordinate ( Quantity -5, Quantity 99 )
                      }
                    ]
            , test "Parse coordinate hyperlink edge case" <|
                parseHyperlinkTest "testx=-5&y=99."
                    [ { position = ( Quantity 4, Quantity.zero )
                      , length = String.length "x=-5&y=99"
                      , route = Hyperlink.Coordinate ( Quantity -5, Quantity 99 )
                      }
                    ]
            , test "Parse coordinate hyperlink edge case 2" <|
                parseHyperlinkTest "testx=-05&y=099."
                    [ { position = ( Quantity 4, Quantity.zero )
                      , length = String.length "x=-05&y=099"
                      , route = Hyperlink.Coordinate ( Quantity -5, Quantity 99 )
                      }
                    ]
            , test "Parse coordinate hyperlink edge case 3" <|
                parseHyperlinkTest "testx=5&y=123456789" []
            , test "Parse coordinate hyperlink" <|
                parseHyperlinkTest "http://ascii-collab.lamdera.app/?x=-5&y=99"
                    [ { position = ( Quantity 0, Quantity.zero )
                      , length = String.length "http://ascii-collab.lamdera.app/?x=-5&y=99"
                      , route = Hyperlink.Coordinate ( Quantity -5, Quantity 99 )
                      }
                    ]
            , test "Parse white listed url" <|
                parseHyperlinkTest "testro-box.netlify.appzxc"
                    [ { position = ( Quantity 4, Quantity.zero )
                      , length = String.length "ro-box.netlify.app"
                      , route = Hyperlink.External "https://ro-box.netlify.app"
                      }
                    ]
            , test "Parse notify-me hyperlink" <|
                parseHyperlinkTest "http://ascii-collab.lamdera.app/notify-me"
                    [ { position = ( Quantity 0, Quantity.zero )
                      , length = String.length "http://ascii-collab.lamdera.app/notify-me"
                      , route = Hyperlink.NotifyMe
                      }
                    ]
            , test "Parse notify-me hyperlink with trailing slash" <|
                parseHyperlinkTest "http://ascii-collab.lamdera.app/notify-me/"
                    [ { position = ( Quantity 0, Quantity.zero )
                      , length = String.length "http://ascii-collab.lamdera.app/notify-me/"
                      , route = Hyperlink.NotifyMe
                      }
                    ]
            , test "Selection test" <|
                (Grid.empty
                    |> Grid.addChange
                        { cellPosition = Helper.fromRawCoord ( 0, 0 )
                        , localPosition = 0
                        , change =
                            Nonempty asciiA
                                (List.repeat (GridCell.cellSize - 1) asciiA
                                    ++ List.repeat GridCell.cellSize asciiB
                                    ++ List.repeat GridCell.cellSize asciiC
                                )
                        , userId = User.userId 0
                        }
                    |> Frontend.selectionToString
                        (Bounds.bounds ( Quantity.zero, Quantity 1 ) ( Quantity 4, Quantity 1 ))
                        EverySet.empty
                        EverySet.empty
                    |> (\a ->
                            if a == "bbbbb" then
                                testSingle Passed

                            else
                                testSingle <| Failed ("Expected bbbbb but got " ++ a)
                       )
                )
            , Maybe.map
                (\email ->
                    let
                        confirmationKey =
                            ConfirmEmailKey "56abfbd7d2ea606e667945422de5a368b8b0272b8f29081cb058b594dd7e3249"
                    in
                    testInit BackendLogic.init
                        |> testMap
                            (BackendLogic.updateFromFrontend
                                (Time.millisToPosix 0)
                                "sessionId0"
                                "clientId0"
                                (RequestData
                                    (Bounds.bounds (Helper.fromRawCoord ( 0, 0 )) (Helper.fromRawCoord ( 5, 5 )))
                                )
                                >> Tuple.first
                            )
                        |> testMap
                            (BackendLogic.updateFromFrontend
                                (Time.millisToPosix 1000)
                                "sessionId0"
                                "clientId0"
                                (NotifyMeSubmitted { email = email, frequency = Every3Hours })
                            )
                        |> testAssert
                            (\( _, effect ) ->
                                case effect of
                                    (SendEmail _ _) :: [] ->
                                        Passed

                                    _ ->
                                        Failed "No confirmation email attempted"
                            )
                        |> testMap Tuple.first
                        |> testMap
                            (BackendLogic.update
                                (ConfirmationEmailSent "sessionId0" (Time.millisToPosix 2000) (Ok ()))
                            )
                        |> testAssert
                            (\( model, _ ) ->
                                case model.pendingEmails of
                                    subscribed :: [] ->
                                        expectEqual
                                            { email = email
                                            , frequency = Every3Hours
                                            , userId = User.userId 0
                                            , creationTime = Time.millisToPosix 1000
                                            , key = confirmationKey
                                            }
                                            subscribed

                                    _ ->
                                        Failed "Missing email notification configuration"
                            )
                        |> testAssert
                            (\( _, effect ) ->
                                expectEqual
                                    [ SendToFrontend "clientId0" (NotifyMeEmailSent { isSuccessful = True }) ]
                                    effect
                            )
                        |> testMap Tuple.first
                        |> testMap
                            (BackendLogic.updateFromFrontend
                                (Time.millisToPosix 3000)
                                "sessionId0"
                                "clientId0"
                                (ConfirmationEmailConfirmed_ confirmationKey)
                            )
                        |> testAssert
                            (\( model, _ ) ->
                                case model.subscribedEmails of
                                    subscribed :: [] ->
                                        expectEqual
                                            { email = email
                                            , frequency = Every3Hours
                                            , userId = User.userId 0
                                            , confirmTime = Time.millisToPosix 3000
                                            }
                                            subscribed

                                    _ ->
                                        Failed "Missing email notification configuration"
                            )
                        |> testAssert
                            (\( _, effect ) ->
                                expectEqual
                                    [ SendToFrontend "clientId0" NotifyMeConfirmed ]
                                    effect
                            )
                        |> testMap (always ())
                )
                (Email.fromString "test@test.com")
                |> Maybe.withDefault (Failed "Invalid email" |> testSingle)
                |> test "Email confirmation happy path"
            , Maybe.map
                (\email ->
                    let
                        confirmationKey =
                            ConfirmEmailKey "56abfbd7d2ea606e667945422de5a368b8b0272b8f29081cb058b594dd7e3249"
                    in
                    testInit BackendLogic.init
                        |> testMap
                            (BackendLogic.updateFromFrontend
                                (Time.millisToPosix 0)
                                "sessionId0"
                                "clientId0"
                                (RequestData
                                    (Bounds.bounds (Helper.fromRawCoord ( 0, 0 )) (Helper.fromRawCoord ( 5, 5 )))
                                )
                                >> Tuple.first
                            )
                        |> testMap
                            (BackendLogic.updateFromFrontend
                                (Time.millisToPosix 1000)
                                "sessionId0"
                                "clientId0"
                                (NotifyMeSubmitted { email = email, frequency = Every3Hours })
                                >> Tuple.first
                            )
                        |> testMap
                            (BackendLogic.updateFromFrontend
                                (Time.millisToPosix 2000)
                                "sessionId0"
                                "clientId0"
                                (NotifyMeSubmitted { email = email, frequency = Every3Hours })
                                >> Tuple.first
                            )
                        |> testMap
                            (BackendLogic.updateFromFrontend
                                (Time.millisToPosix
                                    (1000 + round (Duration.inMilliseconds BackendLogic.sendConfirmationEmailRateLimit))
                                )
                                "sessionId0"
                                "clientId0"
                                (NotifyMeSubmitted { email = email, frequency = Every3Hours })
                            )
                        |> testAssert
                            (\( _, effect ) ->
                                expectEqual [] effect
                            )
                        |> testMap (always ())
                )
                (Email.fromString "test@test.com")
                |> Maybe.withDefault (Failed "Invalid email" |> testSingle)
                |> test "Email confirmation rate limit"
            ]


expectEqual : a -> a -> TestResult
expectEqual expected actual =
    if expected == actual then
        Passed

    else
        Failed <|
            "Expected: "
                ++ Debug.toString expected
                ++ " but got "
                ++ Debug.toString actual


parseHyperlinkTest : String -> List Hyperlink -> Test ()
parseHyperlinkTest input expected =
    let
        actual =
            Parser.run (Hyperlink.urlsParser (Helper.fromRawCoord ( 0, 0 ))) input
    in
    expectEqual (Ok expected) actual |> testSingle


boundsTest : String -> List (Coord unit) -> Bounds unit -> Element msg
boundsTest name expected bounds =
    Bounds.coordRangeFold
        (::)
        identity
        bounds
        []
        |> expectEqual expected
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
            (Time.millisToPosix 100000)
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


asciiB =
    Ascii.fromChar 'b' |> Maybe.withDefault Ascii.default


asciiC =
    Ascii.fromChar 'c' |> Maybe.withDefault Ascii.default


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
