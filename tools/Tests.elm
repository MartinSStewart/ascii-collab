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
import Email.Html
import EverySet
import Frontend
import Grid
import GridCell
import Helper exposing (Coord)
import Hyperlink exposing (Hyperlink)
import List.Extra as List
import List.Nonempty as Nonempty exposing (Nonempty(..))
import LocalGrid
import LocalModel
import NonemptyExtra as Nonempty
import NotifyMe exposing (Frequency(..))
import Parser
import Quantity exposing (Quantity(..))
import String.Nonempty exposing (NonemptyString)
import Time
import Types exposing (BackendModel, BackendMsg(..), EmailEvent(..), FrontendModel, ToBackend(..), ToFrontend(..))
import Units exposing (AsciiUnit, CellUnit)
import UrlHelper exposing (ConfirmEmailKey(..), UnsubscribeEmailKey(..))
import User


type TestResult
    = Passed
    | Inconclusive (Element Never)
    | Failed (Element Never)


failed : String -> TestResult
failed =
    Element.text >> List.singleton >> Element.paragraph [] >> Failed


main =
    Element.layout [] <|
        Element.column [ Element.padding 16, Element.spacing 16 ]
            (tests ())


test2 : () -> List (Element Never)
test2 () =
    [ Maybe.map
        (\email ->
            notifyMeEvery Every3Hours email
                |> testModel
                |> testInit
                |> testMap
                    (Tuple.first
                        >> BackendLogic.updateFromFrontend
                            (Time.millisToPosix
                                (3000 + round (Duration.inMilliseconds BackendLogic.sendConfirmationEmailRateLimit))
                            )
                            "sessionId0"
                            "clientId0"
                            (changes ( Units.asciiUnit 0, Units.asciiUnit 0 ) "o  oaaaaaaaaaaaaaaaaaaaaaaaa++++++++++++\n  > \n\\__/" |> GridChange)
                    )
                |> testMap
                    (Tuple.first
                        >> BackendLogic.update
                            (Duration.addTo
                                (Time.millisToPosix
                                    (3000 + round (Duration.inMilliseconds BackendLogic.sendConfirmationEmailRateLimit))
                                )
                                (Duration.hours 3)
                                |> NotifyAdminTimeElapsed
                            )
                    )
                |> testAssert
                    (\( _, effect ) ->
                        case getEmails effect of
                            ( _, content, _ ) :: [] ->
                                Email.Html.toHtml content |> Element.html |> Inconclusive

                            _ ->
                                failed "Expected a single SendEmail effect"
                    )
                |> testMap (always ())
        )
        (Email.fromString "test@test.com")
        |> Maybe.withDefault (failed "Invalid email" |> testSingle)
        |> test "Email notification every 3 hours"
    ]


tests : () -> List (Element Never)
tests () =
    [ test
        "Request Data creates user"
        (let
            ( model, effect ) =
                newUserState
         in
         (case effect of
            [] ->
                failed "No response sent"

            _ :: _ :: _ ->
                failed "Too many responses sent"

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
                                    failed "User not found"

                        _ ->
                            failed "Wrong ToFrontend msg"

                else
                    failed "Response sent to wrong client"

            (SendEmail _ _ _ _) :: [] ->
                failed "Wrong effect"
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
                        failed "Expected 4 spaces"
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
                        failed "Expected single a"
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
                        failed "Expected single a"
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
                        failed "Expected single a"
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
                        failed "Expected single a"
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
                        failed "Expected no a"
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
                        failed "Expected no a"
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
    , test "Parse poster hyperlink" <|
        parseHyperlinkTest "http://ascii-collab.lamdera.app/poster.png"
            [ { position = ( Quantity 0, Quantity.zero )
              , length = String.length "http://ascii-collab.lamdera.app/poster.png"
              , route = Hyperlink.Resource "poster.png"
              }
            ]
    , test "Parse color poster hyperlink" <|
        parseHyperlinkTest "http://ascii-collab.lamdera.app/poster-color.png"
            [ { position = ( Quantity 0, Quantity.zero )
              , length = String.length "http://ascii-collab.lamdera.app/poster-color.png"
              , route = Hyperlink.Resource "poster-color.png"
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
                        testSingle <| failed ("Expected bbbbb but got " ++ a)
               )
        )
    , Maybe.map
        (\email ->
            notifyMeEvery Every3Hours email
                |> testMap (always ())
        )
        (Email.fromString "test@test.com")
        |> Maybe.withDefault (failed "Invalid email" |> testSingle)
        |> test "Email subscription happy path"
    , Maybe.map
        (\email ->
            notifyMeEvery Every3Hours email
                |> testModel
                |> testInit
                |> testMap
                    (Tuple.first
                        >> BackendLogic.updateFromFrontend
                            (Time.millisToPosix
                                (3000 + round (Duration.inMilliseconds BackendLogic.sendConfirmationEmailRateLimit))
                            )
                            "sessionId0"
                            "clientId0"
                            (changes ( Units.asciiUnit -1, Units.asciiUnit 0 ) "o  o\n  > \n\\__/" |> GridChange)
                    )
                |> testMap
                    (Tuple.first
                        >> BackendLogic.update
                            (Duration.addTo
                                (Time.millisToPosix
                                    (3000 + round (Duration.inMilliseconds BackendLogic.sendConfirmationEmailRateLimit))
                                )
                                (Duration.hours 3)
                                |> NotifyAdminTimeElapsed
                            )
                    )
                |> testAssert
                    (\( _, effect ) ->
                        case getEmails effect of
                            ( _, content, _ ) :: [] ->
                                Email.Html.toHtml content |> Element.html |> Inconclusive

                            _ ->
                                failed "Expected a single SendEmail effect"
                    )
                |> testMap
                    (Tuple.first
                        >> BackendLogic.updateFromFrontend
                            (Time.millisToPosix
                                (3000 + round (Duration.inMilliseconds BackendLogic.sendConfirmationEmailRateLimit))
                            )
                            "sessionId0"
                            "clientId0"
                            (changes ( Units.asciiUnit -2, Units.asciiUnit 1 ) "  < \n~~~~~~" |> GridChange)
                    )
                |> testMap
                    (Tuple.first
                        >> BackendLogic.update
                            (Duration.addTo
                                (Time.millisToPosix
                                    (3000 + round (Duration.inMilliseconds BackendLogic.sendConfirmationEmailRateLimit))
                                )
                                (Duration.hours 6)
                                |> NotifyAdminTimeElapsed
                            )
                    )
                |> testAssert
                    (\( _, effect ) ->
                        case getEmails effect of
                            ( _, content, _ ) :: [] ->
                                Email.Html.toHtml content |> Element.html |> Inconclusive

                            _ ->
                                failed "Expected a single SendEmail effect"
                    )
                |> testMap (always ())
        )
        (Email.fromString "test@test.com")
        |> Maybe.withDefault (failed "Invalid email" |> testSingle)
        |> test "Email notification every 3 hours"
    , Maybe.map
        (\email ->
            notifyMeEvery Every12Hours email
                |> testModel
                |> testInit
                |> testMap
                    (Tuple.first
                        >> BackendLogic.updateFromFrontend
                            (Time.millisToPosix
                                (3000 + round (Duration.inMilliseconds BackendLogic.sendConfirmationEmailRateLimit))
                            )
                            "sessionId0"
                            "clientId0"
                            (changes ( Units.asciiUnit -1, Units.asciiUnit 0 ) "o  o\n  > \n\\__/" |> GridChange)
                    )
                |> testMap
                    (Tuple.first
                        >> BackendLogic.update
                            (Duration.addTo
                                (Time.millisToPosix
                                    (3000 + round (Duration.inMilliseconds BackendLogic.sendConfirmationEmailRateLimit))
                                )
                                (Duration.hours 3)
                                |> NotifyAdminTimeElapsed
                            )
                    )
                |> expectNoEmails
                |> testMap
                    (Tuple.first
                        >> BackendLogic.updateFromFrontend
                            (Time.millisToPosix
                                (3000 + round (Duration.inMilliseconds BackendLogic.sendConfirmationEmailRateLimit))
                            )
                            "sessionId0"
                            "clientId0"
                            (changes ( Units.asciiUnit -2, Units.asciiUnit 1 ) "  < \n~~~~~~" |> GridChange)
                    )
                |> testMap
                    (Tuple.first
                        >> BackendLogic.update
                            (Duration.addTo
                                (Time.millisToPosix
                                    (3000 + round (Duration.inMilliseconds BackendLogic.sendConfirmationEmailRateLimit))
                                )
                                (Duration.hours 6)
                                |> NotifyAdminTimeElapsed
                            )
                    )
                |> expectNoEmails
                |> testMap
                    (Tuple.first
                        >> BackendLogic.update
                            (Duration.addTo
                                (Time.millisToPosix
                                    (3000 + round (Duration.inMilliseconds BackendLogic.sendConfirmationEmailRateLimit))
                                )
                                (Duration.hours 9)
                                |> NotifyAdminTimeElapsed
                            )
                    )
                |> expectNoEmails
                |> testMap
                    (Tuple.first
                        >> BackendLogic.update
                            (Duration.addTo
                                (Time.millisToPosix
                                    (3000 + round (Duration.inMilliseconds BackendLogic.sendConfirmationEmailRateLimit))
                                )
                                (Duration.hours 12)
                                |> NotifyAdminTimeElapsed
                            )
                    )
                |> testAssert
                    (\( _, effect ) ->
                        case getEmails effect of
                            ( _, content, _ ) :: [] ->
                                Email.Html.toHtml content |> Element.html |> Inconclusive

                            _ ->
                                failed "Expected a single SendEmail effect"
                    )
                |> testMap
                    (Tuple.first
                        >> BackendLogic.update
                            (Duration.addTo
                                (Time.millisToPosix
                                    (3000 + round (Duration.inMilliseconds BackendLogic.sendConfirmationEmailRateLimit))
                                )
                                (Duration.hours 15)
                                |> NotifyAdminTimeElapsed
                            )
                    )
                |> expectNoEmails
                |> testMap (always ())
        )
        (Email.fromString "test@test.com")
        |> Maybe.withDefault (failed "Invalid email" |> testSingle)
        |> test "Email notification every 12 hours"
    , Maybe.map
        (\email ->
            notifyMeEvery Every3Hours email
                |> testModel
                |> testInit
                |> testMap
                    (Tuple.first
                        >> BackendLogic.updateFromFrontend
                            (Time.millisToPosix
                                (3000 + round (Duration.inMilliseconds BackendLogic.sendConfirmationEmailRateLimit))
                            )
                            "sessionId0"
                            "clientId0"
                            (changes ( Units.asciiUnit -1, Units.asciiUnit 0 ) "o  o\n  > \n\\__/" |> GridChange)
                    )
                |> testMap
                    (Tuple.first
                        >> BackendLogic.updateFromFrontend
                            (Time.millisToPosix
                                (3000 + round (Duration.inMilliseconds BackendLogic.sendConfirmationEmailRateLimit))
                            )
                            "sessionId0"
                            "clientId0"
                            (changes ( Units.asciiUnit -1000, Units.asciiUnit 0 ) "2" |> GridChange)
                    )
                |> testMap
                    (Tuple.first
                        >> BackendLogic.update
                            (Duration.addTo
                                (Time.millisToPosix
                                    (3000 + round (Duration.inMilliseconds BackendLogic.sendConfirmationEmailRateLimit))
                                )
                                (Duration.hours 3)
                                |> NotifyAdminTimeElapsed
                            )
                    )
                |> testAssert
                    (\( _, effect ) ->
                        case getEmails effect of
                            ( _, content, _ ) :: [] ->
                                Email.Html.toHtml content |> Element.html |> Inconclusive

                            _ ->
                                failed "Expected a single SendEmail effect"
                    )
                |> testMap (always ())
        )
        (Email.fromString "test@test.com")
        |> Maybe.withDefault (failed "Invalid email" |> testSingle)
        |> test "Two images in email notification"
    , Maybe.map
        (\email ->
            notifyMeEvery Every3Hours email
                |> testModel
                |> testInit
                |> canvasChange
                    ( Units.asciiUnit -10, Units.asciiUnit -10 )
                    (String.repeat 100 "a")
                    (Time.millisToPosix
                        (3000 + round (Duration.inMilliseconds BackendLogic.sendConfirmationEmailRateLimit))
                    )
                |> canvasChange
                    ( Units.asciiUnit 0, Units.asciiUnit -20 )
                    (String.repeat 100 "bb\n")
                    (Time.millisToPosix
                        (3000 + round (Duration.inMilliseconds BackendLogic.sendConfirmationEmailRateLimit))
                    )
                |> testMap
                    (Tuple.first
                        >> BackendLogic.update
                            (Duration.addTo
                                (Time.millisToPosix
                                    (3000 + round (Duration.inMilliseconds BackendLogic.sendConfirmationEmailRateLimit))
                                )
                                (Duration.hours 3)
                                |> NotifyAdminTimeElapsed
                            )
                    )
                |> testAssert
                    (\( _, effect ) ->
                        case getEmails effect of
                            ( _, content, _ ) :: [] ->
                                Email.Html.toHtml content |> Element.html |> Inconclusive

                            _ ->
                                failed "Expected a single SendEmail effect"
                    )
                |> testMap (always ())
        )
        (Email.fromString "test@test.com")
        |> Maybe.withDefault (failed "Invalid email" |> testSingle)
        |> test "Many changes in email notification"
    , Maybe.map
        (\email ->
            notifyMeEvery Every3Hours email
                |> testModel
                |> testInit
                |> testMap
                    (Tuple.first
                        >> BackendLogic.updateFromFrontend
                            (Time.millisToPosix
                                (3000 + round (Duration.inMilliseconds BackendLogic.sendConfirmationEmailRateLimit))
                            )
                            "sessionId0"
                            "clientId0"
                            (GridChange (Nonempty.fromElement LocalAddUndo))
                    )
                |> canvasChange
                    ( Units.asciiUnit 0, Units.asciiUnit 0 )
                    "a"
                    (Time.millisToPosix
                        (3000 + round (Duration.inMilliseconds BackendLogic.sendConfirmationEmailRateLimit))
                    )
                |> testMap
                    (Tuple.first
                        >> BackendLogic.updateFromFrontend
                            (Time.millisToPosix
                                (3000 + round (Duration.inMilliseconds BackendLogic.sendConfirmationEmailRateLimit))
                            )
                            "sessionId0"
                            "clientId0"
                            (GridChange (Nonempty.fromElement LocalUndo))
                    )
                |> testMap
                    (Tuple.first
                        >> BackendLogic.update
                            (Duration.addTo
                                (Time.millisToPosix
                                    (3000 + round (Duration.inMilliseconds BackendLogic.sendConfirmationEmailRateLimit))
                                )
                                (Duration.hours 3)
                                |> NotifyAdminTimeElapsed
                            )
                    )
                |> expectNoEmails
                |> testMap (always ())
        )
        (Email.fromString "test@test.com")
        |> Maybe.withDefault (failed "Invalid email" |> testSingle)
        |> test "No notification if changes are undone"
    , Maybe.map
        (\email ->
            notifyMeEvery Every3Hours email
                |> testModel
                |> testInit
                |> testMap
                    (Tuple.first
                        >> BackendLogic.updateFromFrontend
                            (Time.millisToPosix
                                (3000 + round (Duration.inMilliseconds BackendLogic.sendConfirmationEmailRateLimit))
                            )
                            "sessionId0"
                            "clientId0"
                            (GridChange (Nonempty.fromElement LocalAddUndo))
                    )
                |> canvasChange
                    ( Units.asciiUnit 0, Units.asciiUnit 0 )
                    "a"
                    (Time.millisToPosix
                        (3000 + round (Duration.inMilliseconds BackendLogic.sendConfirmationEmailRateLimit))
                    )
                |> testMap
                    (Tuple.first
                        >> BackendLogic.update
                            (Duration.addTo
                                (Time.millisToPosix
                                    (3000 + round (Duration.inMilliseconds BackendLogic.sendConfirmationEmailRateLimit))
                                )
                                (Duration.hours 3)
                                |> NotifyAdminTimeElapsed
                            )
                    )
                |> testAssert
                    (\( _, effect ) ->
                        case getEmails effect of
                            ( _, content, _ ) :: [] ->
                                let
                                    (UnsubscribeEmailKey key) =
                                        unsubscribeKey
                                in
                                if Debug.toString content |> String.contains key then
                                    Passed

                                else
                                    failed "Missing unsubscribe key"

                            _ ->
                                failed "Expected one email to be sent"
                    )
                |> testMap
                    (Tuple.first
                        >> BackendLogic.updateFromFrontend
                            (Time.millisToPosix 0)
                            "sessionId0"
                            "clientId1"
                            (ConnectToBackend
                                (Bounds.bounds (Helper.fromRawCoord ( 0, 0 )) (Helper.fromRawCoord ( 5, 5 )))
                                (Just (UnsubscribeEmail unsubscribeKey))
                            )
                    )
                |> testAssert
                    (\( _, effect ) ->
                        if List.count ((==) (SendToFrontend "clientId1" UnsubscribeEmailConfirmed)) effect == 1 then
                            Passed

                        else
                            Failed (Element.text "Expected 1 UnsubscribeEmailConfirmed message")
                    )
                |> testAssert
                    (\( model, _ ) ->
                        expectEqual [] model.subscribedEmails
                    )
                |> testMap (always ())
        )
        (Email.fromString "test@test.com")
        |> Maybe.withDefault (failed "Invalid email" |> testSingle)
        |> test "Unsubscribe"
    , Maybe.map
        (\email ->
            testInit BackendLogic.init
                |> testMap
                    (BackendLogic.updateFromFrontend
                        (Time.millisToPosix 0)
                        "sessionId0"
                        "clientId0"
                        (ConnectToBackend
                            (Bounds.bounds (Helper.fromRawCoord ( 0, 0 )) (Helper.fromRawCoord ( 5, 5 )))
                            Nothing
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
        |> Maybe.withDefault (failed "Invalid email" |> testSingle)
        |> test "Email confirmation rate limit"
    ]


canvasChange : Coord AsciiUnit -> String -> Time.Posix -> Test ( BackendModel, List Effect ) -> Test ( BackendModel, List Effect )
canvasChange coord text time_ =
    testMap
        (Tuple.first
            >> BackendLogic.updateFromFrontend
                time_
                "sessionId0"
                "clientId0"
                (changes coord text |> GridChange)
        )


getEmails : List Effect -> List ( NonemptyString, Email.Html.Html, Email.Email )
getEmails effects =
    List.filterMap
        (\effect ->
            case effect of
                SendEmail _ subject content to ->
                    Just ( subject, content, to )

                _ ->
                    Nothing
        )
        effects


expectNoEmails =
    testAssert
        (\( _, effect ) ->
            case getEmails effect of
                [] ->
                    Passed

                _ ->
                    failed "Expected no emails"
        )


notifyMeEvery : Frequency -> Email.Email -> Test ( BackendModel, List Effect )
notifyMeEvery frequency email =
    testInit BackendLogic.init
        |> testMap
            (BackendLogic.updateFromFrontend
                (Time.millisToPosix 0)
                "sessionId0"
                "clientId0"
                (ConnectToBackend
                    (Bounds.bounds (Helper.fromRawCoord ( 0, 0 )) (Helper.fromRawCoord ( 5, 5 )))
                    Nothing
                )
                >> Tuple.first
            )
        |> testMap
            (BackendLogic.updateFromFrontend
                (Time.millisToPosix 1000)
                "sessionId0"
                "clientId0"
                (NotifyMeSubmitted { email = email, frequency = frequency })
            )
        |> testAssert
            (\( _, effect ) ->
                case effect of
                    (SendEmail _ _ _ _) :: [] ->
                        Passed

                    _ ->
                        failed "No confirmation email attempted"
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
                            , frequency = frequency
                            , userId = User.userId 0
                            , creationTime = Time.millisToPosix 1000
                            , key = confirmationKey
                            }
                            subscribed

                    _ ->
                        failed "Missing email notification configuration"
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
                "sessionId1"
                "clientId1"
                (ConnectToBackend
                    (Bounds.bounds (Helper.fromRawCoord ( 0, 0 )) (Helper.fromRawCoord ( 5, 5 )))
                    (Just (ConfirmationEmailConfirmed_ confirmationKey))
                )
            )
        |> testAssert
            (\( model, _ ) ->
                case model.subscribedEmails of
                    subscribed :: [] ->
                        expectEqual
                            { email = email
                            , frequency = frequency
                            , userId = User.userId 0
                            , confirmTime = Time.millisToPosix 3000
                            , unsubscribeKey = unsubscribeKey
                            }
                            subscribed

                    _ ->
                        failed "Missing email notification configuration"
            )
        |> testAssert
            (\( _, effect ) ->
                if
                    (List.count ((==) (SendToFrontend "clientId0" NotifyMeConfirmed)) effect == 1)
                        && (List.count ((==) (SendToFrontend "clientId1" NotifyMeConfirmed)) effect == 1)
                then
                    Passed

                else
                    failed "Expected two NotifyMeConfirmed messages"
            )


confirmationKey =
    ConfirmEmailKey "56abfbd7d2ea606e667945422de5a368b8b0272b8f29081cb058b594dd7e3249"


unsubscribeKey =
    UnsubscribeEmailKey "dbfcfd0d87220f629339bd3adcf452d083fde3246625fb3a93e314f833e20d37"


changes : Coord AsciiUnit -> String -> Nonempty Change.LocalChange
changes coord text =
    String.split "\n" text
        |> List.map (String.toList >> List.filterMap Ascii.fromChar)
        |> Nonempty.fromList
        |> Maybe.withDefault (Nonempty [] [])
        |> Grid.textToChange coord
        |> Nonempty.map LocalGridChange


expectEqual : a -> a -> TestResult
expectEqual expected actual =
    if expected == actual then
        Passed

    else
        failed <|
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


boundsTest : String -> List (Coord unit) -> Bounds unit -> Element Never
boundsTest name expected bounds =
    Bounds.coordRangeFold
        (::)
        identity
        bounds
        []
        |> expectEqual expected
        |> testSingle
        |> test name


boundsReverseTest : String -> List (Coord unit) -> Bounds unit -> Element Never
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
                    failed (Debug.toString a ++ " is incorrect. Expected " ++ Debug.toString expected)
           )
        |> testSingle
        |> test name


test : String -> Test model -> Element Never
test name (Test testResults _) =
    Element.column
        [ Element.width Element.fill ]
        [ Element.paragraph [ Element.alignTop, Element.padding 8 ] [ Element.text name ]
        , List.map
            (\testResult ->
                case testResult of
                    Failed error ->
                        Element.el
                            [ Element.Background.color (Element.rgb 1 0 0)
                            , Element.padding 8
                            , Element.width Element.fill
                            ]
                            error

                    Inconclusive content ->
                        Element.el
                            [ Element.Background.color (Element.rgb 0.9 0.8 0.3)
                            , Element.padding 8
                            , Element.width Element.fill
                            ]
                            content

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
            (ConnectToBackend smallViewBounds Nothing)


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


testModel : Test model -> model
testModel (Test _ model) =
    model


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
                    failed
                        ("Wrong value found in grid "
                            ++ Debug.toString ascii
                            ++ " at cell position "
                            ++ Debug.toString cellPosition
                            ++ " and local position "
                            ++ Debug.toString localPosition
                        )
           )
