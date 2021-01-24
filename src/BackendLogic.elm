module BackendLogic exposing (Effect(..), init, notifyAdminWait, sendConfirmationEmailRateLimit, statistics, update, updateFromFrontend)

import Array exposing (Array)
import Ascii exposing (Ascii)
import Bounds exposing (Bounds)
import Change exposing (ClientChange(..), ServerChange(..))
import Cluster
import Crypto.Hash
import Dict
import Duration exposing (Duration)
import Element
import Email
import Env
import EverySet exposing (EverySet)
import Grid exposing (Grid)
import GridCell
import Helper exposing (Coord, RawCellCoord)
import Html.String
import Html.String.Attributes
import Lamdera exposing (ClientId, SessionId)
import List.Extra as List
import List.Nonempty as Nonempty exposing (Nonempty(..))
import LocalGrid
import NonemptyExtra as Nonempty
import NotifyMe
import Quantity exposing (Quantity(..))
import RecentChanges
import SendGrid
import Set
import Shaders
import String.Nonempty exposing (NonemptyString(..))
import Time
import Types exposing (..)
import Undo
import Units exposing (AsciiUnit, CellUnit)
import UrlHelper exposing (ConfirmEmailKey(..), InternalRoute(..), UnsubscribeEmailKey(..))
import User exposing (UserId)


type Effect
    = SendToFrontend ClientId ToFrontend
    | SendEmail (Result SendGrid.Error () -> BackendMsg) NonemptyString (Html.String.Html Never) Email.Email


init : BackendModel
init =
    { grid = Grid.empty
    , userSessions = Dict.empty
    , users =
        Dict.empty
    , usersHiddenRecently = []
    , userChangesRecently = RecentChanges.init
    , subscribedEmails = []
    , pendingEmails = []
    , secretLinkCounter = 0
    , errors = []
    }


notifyAdminWait : Duration
notifyAdminWait =
    String.toFloat Env.notifyAdminWaitInHours |> Maybe.map Duration.hours |> Maybe.withDefault (Duration.hours 3)


update : BackendMsg -> BackendModel -> ( BackendModel, List Effect )
update msg model =
    case msg of
        UserDisconnected sessionId clientId ->
            ( { model
                | userSessions =
                    Dict.update sessionId
                        (Maybe.map
                            (\session ->
                                { clientIds = Dict.remove clientId session.clientIds
                                , userId = session.userId
                                }
                            )
                        )
                        model.userSessions
              }
            , []
            )

        NotifyAdminTimeElapsed time ->
            let
                ( newModel, cmd ) =
                    notifyAdmin model

                ( newModel2, cmd2 ) =
                    sendChangeEmails time newModel

                ( newModel4, cmd3 ) =
                    case Dict.get (User.rawId backendUserId) newModel2.users of
                        Just userData ->
                            drawStatistics time ( backendUserId, userData ) newModel2

                        Nothing ->
                            let
                                ( newModel3, userData ) =
                                    createUser backendUserId newModel2
                            in
                            drawStatistics time ( backendUserId, userData ) newModel3
            in
            ( newModel4, cmd ++ cmd2 ++ cmd3 )

        NotifyAdminEmailSent ->
            ( model, [] )

        ConfirmationEmailSent sessionId timeSent result ->
            ( case result of
                Ok () ->
                    model

                Err error ->
                    addError timeSent (SendGridError Env.adminEmail error) model
            , broadcast
                (\sessionId_ _ ->
                    if sessionId_ == sessionId then
                        NotifyMeEmailSent { isSuccessful = result == Ok () } |> Just

                    else
                        Nothing
                )
                model
            )

        UpdateFromFrontend sessionId clientId toBackendMsg time ->
            updateFromFrontend time sessionId clientId toBackendMsg model

        ChangeEmailSent time email result ->
            case result of
                Ok _ ->
                    ( model, [] )

                Err error ->
                    ( addError time (SendGridError email error) model, [] )


sendChangeEmails time model =
    let
        ( frequencyChanges, recentChangeState ) =
            RecentChanges.threeHoursElapsed model.userChangesRecently

        getActualChanges : Dict.Dict RawCellCoord GridCell.Cell -> Maybe (Nonempty ( RawCellCoord, Array ( Maybe UserId, Ascii ) ))
        getActualChanges changes =
            Dict.toList changes
                |> List.filterMap
                    (\( coord, originalCell ) ->
                        let
                            diff : Array ( Maybe UserId, Ascii )
                            diff =
                                diffCells
                                    model
                                    originalCell
                                    (Grid.getCell (Helper.fromRawCoord coord) model.grid
                                        |> Maybe.withDefault GridCell.empty
                                    )
                        in
                        if Array.toList diff |> List.any (Tuple.first >> (/=) Nothing) then
                            Just ( coord, diff )

                        else
                            Nothing
                    )
                |> Nonempty.fromList

        clusters :
            Nonempty ( RawCellCoord, Array ( Maybe UserId, Ascii ) )
            -> List ( Bounds CellUnit, Nonempty (Coord CellUnit) )
        clusters actualChanges =
            Nonempty.map Tuple.first actualChanges |> Nonempty.toList |> Set.fromList |> Cluster.cluster

        content :
            Nonempty ( RawCellCoord, Array ( Maybe UserId, Ascii ) )
            -> UnsubscribeEmailKey
            -> Html.String.Html msg
        content actualChanges =
            let
                images =
                    List.map (\( bounds, _ ) -> clusterToTextImage model actualChanges bounds) (clusters actualChanges)
            in
            \unsubscribeKey ->
                Html.String.div
                    [ Html.String.Attributes.style "background-color" "rgb(230, 230, 225)"
                    , Html.String.Attributes.style "padding" "8px"
                    , Html.String.Attributes.style "font-size" "16px"
                    , Html.String.Attributes.style "line-height" "100%"
                    ]
                    [ Html.String.text "Click on an image to view it in ascii-collab"
                    , Html.String.div
                        [ Html.String.Attributes.style "font-family" "monospace" ]
                        images
                    , Html.String.node "hr" [] []
                    , Html.String.a
                        [ UrlHelper.encodeUrl (EmailUnsubscribeRoute unsubscribeKey)
                            |> (++) (Env.domain ++ "/")
                            |> Html.String.Attributes.href
                        ]
                        [ Html.String.text "Click here to unsubscribe" ]
                    ]

        subject frequency_ =
            case frequency_ of
                NotifyMe.Every3Hours ->
                    NonemptyString 'C' "hanges over the past 3 hours"

                NotifyMe.Every12Hours ->
                    NonemptyString 'C' "hanges over the past 12 hours"

                NotifyMe.Daily ->
                    NonemptyString 'C' "hanges over the past day"

                NotifyMe.Weekly ->
                    NonemptyString 'C' "hanges over the past week"

                NotifyMe.Monthly ->
                    NonemptyString 'C' "hanges over the past month"
    in
    ( { model | userChangesRecently = recentChangeState }
    , List.concatMap
        (\( frequency, changes ) ->
            case getActualChanges changes of
                Just actualChanges_ ->
                    let
                        content_ =
                            content actualChanges_

                        subject_ =
                            subject frequency
                    in
                    List.filter (.frequency >> (==) frequency)
                        model.subscribedEmails
                        |> List.map
                            (\email ->
                                SendEmail
                                    (ChangeEmailSent time email.email)
                                    subject_
                                    (content_ email.unsubscribeKey)
                                    email.email
                            )

                Nothing ->
                    []
        )
        frequencyChanges
    )


clusterToTextImage :
    { a | grid : Grid, users : Dict.Dict Int { b | hiddenForAll : Bool } }
    -> Nonempty ( RawCellCoord, Array ( Maybe UserId, Ascii ) )
    -> Bounds CellUnit
    -> Html.String.Html msg
clusterToTextImage model actualChanges bounds =
    let
        url : String
        url =
            bounds
                |> Bounds.addToMax ( Units.cellUnit 1, Units.cellUnit 1 )
                |> Bounds.center
                |> Units.cellToAscii_
                |> Helper.roundPoint
                |> UrlHelper.internalRoute False
                |> UrlHelper.encodeUrl
                |> (++) Env.domain
    in
    Bounds.coordRangeFold
        (\coord ( value, a ) ->
            let
                rawCoord =
                    Helper.toRawCoord coord

                array : Array ( Maybe UserId, Ascii )
                array =
                    case List.find (Tuple.first >> (==) rawCoord) (Nonempty.toList actualChanges) of
                        Just ( _, original ) ->
                            original

                        Nothing ->
                            Grid.getCell coord model.grid
                                |> Maybe.withDefault GridCell.empty
                                |> GridCell.flatten EverySet.empty (hiddenUsers Nothing model)

                slices : List (List ( Maybe UserId, Ascii ))
                slices =
                    List.range 0 (GridCell.cellSize - 1)
                        |> List.map
                            (\index ->
                                Array.slice
                                    (GridCell.cellSize * index)
                                    (GridCell.cellSize * (index + 1))
                                    array
                                    |> Array.toList
                            )
            in
            ( List.map2 (\slice rest -> rest ++ slice) slices value
            , a
            )
        )
        (\( a, b ) -> ( List.repeat GridCell.cellSize [], b ++ [ a ] ))
        bounds
        ( List.repeat GridCell.cellSize [], [] )
        |> (\( a, b ) -> b ++ [ a ])
        |> List.concat
        |> List.map
            (\row ->
                let
                    startsWithChange =
                        List.head row |> Maybe.map Tuple.first |> Maybe.withDefault Nothing

                    textToHtml currentUserId text =
                        let
                            text_ =
                                List.reverse text |> String.fromList
                        in
                        case currentUserId of
                            Just userId ->
                                let
                                    { red, green, blue } =
                                        Shaders.userColor userId
                                            |> Shaders.lch2rgb
                                            |> Element.toRgb
                                in
                                Html.String.span
                                    [ Html.String.Attributes.style
                                        "background-color"
                                        ("rgb("
                                            ++ (String.fromInt <| round <| red * 255)
                                            ++ ","
                                            ++ (String.fromInt <| round <| green * 255)
                                            ++ ","
                                            ++ (String.fromInt <| round <| blue * 255)
                                            ++ ")"
                                        )
                                    ]
                                    [ Html.String.text text_ ]

                            Nothing ->
                                Html.String.text text_
                in
                List.foldl
                    (\( maybeUserId, ascii ) ( currentUserId, text, html ) ->
                        let
                            char =
                                Ascii.toChar ascii
                                    |> (\a ->
                                            if a == ' ' then
                                                '\u{00A0}'

                                            else
                                                a
                                       )
                        in
                        if maybeUserId == currentUserId then
                            ( currentUserId, char :: text, html )

                        else
                            ( maybeUserId
                            , [ char ]
                            , textToHtml currentUserId text :: html
                            )
                    )
                    ( startsWithChange, [], [] )
                    row
                    |> (\( currentUserId, text, html ) ->
                            textToHtml currentUserId text :: html |> List.reverse
                       )
            )
        |> List.intersperse [ Html.String.br [] [] ]
        |> List.concat
        |> Html.String.a
            [ Html.String.Attributes.href url
            , Html.String.Attributes.style "color" "black"
            , Html.String.Attributes.style "text-decoration" "none"
            ]
        |> List.singleton
        |> Html.String.div
            [ Html.String.Attributes.style "background-color" "white"
            , Html.String.Attributes.style "width" "max-content"
            , Html.String.Attributes.style "margin" "8px 0"
            , Html.String.Attributes.style "padding" "1px 0"
            ]


addError : Time.Posix -> BackendError -> BackendModel -> BackendModel
addError time error model =
    { model | errors = ( time, error ) :: model.errors }


notifyAdmin : BackendModel -> ( BackendModel, List Effect )
notifyAdmin model =
    let
        idToString =
            User.rawId >> String.fromInt

        fullUrl point =
            Env.domain ++ "/" ++ UrlHelper.encodeUrl (UrlHelper.internalRoute False point)

        hidden =
            List.map
                (\{ reporter, hiddenUser, hidePoint } ->
                    "User "
                        ++ idToString reporter
                        ++ " hid user "
                        ++ idToString hiddenUser
                        ++ "'s text at "
                        ++ fullUrl hidePoint
                )
                model.usersHiddenRecently
                |> String.join "\n"
    in
    if List.isEmpty model.usersHiddenRecently then
        ( model, [] )

    else
        ( { model | usersHiddenRecently = [] }
        , [ SendEmail
                (always NotifyAdminEmailSent)
                (String.Nonempty.append_
                    (String.Nonempty.fromInt (List.length model.usersHiddenRecently))
                    " users hidden"
                )
                (Html.String.text hidden)
                Env.adminEmail
          ]
        )


diffCells : BackendModel -> GridCell.Cell -> GridCell.Cell -> Array ( Maybe UserId, Ascii )
diffCells model before after =
    List.map2
        (\before_ after_ ->
            if before_ == after_ then
                Tuple.mapFirst (always Nothing) after_

            else
                after_
        )
        (GridCell.flatten EverySet.empty (hiddenUsers Nothing model) before |> Array.toList)
        (GridCell.flatten EverySet.empty (hiddenUsers Nothing model) after |> Array.toList)
        |> Array.fromList


backendUserId : UserId
backendUserId =
    User.userId -1


drawStatistics : Time.Posix -> ( UserId, BackendUserData ) -> BackendModel -> ( BackendModel, List Effect )
drawStatistics currentTime ( userId, userData ) model =
    let
        stats : Nonempty ( Ascii, Int )
        stats =
            statistics (hiddenUsers (Just userId) model) Env.statisticsBounds model.grid

        map : Nonempty (List Ascii)
        map =
            generateMap
                (hiddenUsers (Just userId) model)
                (Bounds.convert (Grid.asciiToCellAndLocalCoord >> Tuple.first) Env.statisticsBounds)
                model.grid

        rows =
            32

        statText : Nonempty (List Ascii)
        statText =
            stats
                |> Nonempty.sortWith
                    (\( asciiA, totalA ) ( asciiB, totalB ) ->
                        case compare totalA totalB of
                            GT ->
                                GT

                            LT ->
                                LT

                            EQ ->
                                compare (Ascii.toChar asciiB) (Ascii.toChar asciiA)
                    )
                |> Nonempty.reverse
                |> Nonempty.greedyGroupsOf rows
                |> Nonempty.map
                    (Nonempty.map
                        (\( ascii, total ) ->
                            let
                                number : List (Maybe Ascii)
                                number =
                                    String.fromInt total
                                        |> String.toList
                                        |> List.map Ascii.fromChar
                            in
                            [ Just ascii, Ascii.fromChar '=' ]
                                ++ number
                                |> List.filterMap identity
                        )
                        >> (\column ->
                                let
                                    maxWidth =
                                        Nonempty.maximumBy List.length column |> List.length |> max 5 |> (+) 2
                                in
                                Nonempty.map
                                    (\chars -> chars ++ List.repeat (maxWidth - List.length chars) Ascii.default)
                                    column
                           )
                    )
                -- We need to make sure our list of lists is rectangular before we do a transpose
                |> Nonempty.map
                    (\list ->
                        let
                            length =
                                Nonempty.length list
                        in
                        case List.repeat (rows - length) [] |> Nonempty.fromList of
                            Just padding ->
                                Nonempty.append list padding

                            Nothing ->
                                list
                    )
                |> Nonempty.transpose
                |> Nonempty.map (Nonempty.toList >> List.concat)

        timestamp_ : Nonempty (List Ascii)
        timestamp_ =
            timestamp currentTime (Duration.addTo currentTime notifyAdminWait)
                |> String.toList
                |> List.filterMap Ascii.fromChar
                |> Nonempty.fromElement

        mapTextPos : Coord AsciiUnit
        mapTextPos =
            Bounds.height Env.statisticsBounds
                |> Quantity.toFloatQuantity
                |> Quantity.divideBy 8
                |> Quantity.round
                |> Quantity.plus (Quantity 1)
                |> Tuple.pair Quantity.zero
                |> Helper.addTuple Env.mapDrawAt
    in
    Grid.textToChange Env.statisticsDrawAt statText
        |> Nonempty.append
            (Grid.textToChange (Helper.addTuple ( Quantity 0, Quantity (rows + 1) ) Env.statisticsDrawAt) timestamp_)
        |> Nonempty.append (Grid.textToChange Env.mapDrawAt map)
        |> Nonempty.append (Grid.textToChange mapTextPos timestamp_)
        |> Nonempty.map Change.LocalGridChange
        -- Remove previous statistics so the undo history doesn't get really long
        |> Nonempty.append (Nonempty Change.LocalUndo [ Change.LocalAddUndo ])
        |> (\a -> broadcastLocalChange ( userId, userData ) a model)


timestamp : Time.Posix -> Time.Posix -> String
timestamp currentTime nextUpdate =
    let
        hourMinute time =
            String.fromInt (Time.toHour Time.utc time)
                ++ ":"
                ++ String.padLeft 2 '0' (String.fromInt (Time.toMinute Time.utc time))
    in
    "Last updated at "
        ++ hourMinute currentTime
        ++ " UTC. Next update at "
        ++ hourMinute nextUpdate
        ++ "."


statistics : EverySet UserId -> Bounds AsciiUnit -> Grid -> Nonempty ( Ascii, Int )
statistics hiddenUsers_ bounds grid =
    let
        cells =
            Grid.allCellsDict grid

        charsPerCell =
            GridCell.cellSize * GridCell.cellSize

        adjustedBounds : Bounds CellUnit
        adjustedBounds =
            Bounds.convert
                (Grid.asciiToCellAndLocalCoord >> Tuple.first)
                bounds

        isOnEdge : Coord CellUnit -> Bool
        isOnEdge coord =
            let
                ( minX, minY ) =
                    Bounds.minimum adjustedBounds |> Helper.toRawCoord

                ( maxX, maxY ) =
                    Bounds.maximum adjustedBounds |> Helper.toRawCoord

                ( x, y ) =
                    Helper.toRawCoord coord
            in
            x == minX || x == maxX || y == minY || y == maxY

        countCell : Coord CellUnit -> GridCell.Cell -> Nonempty ( Ascii, Int ) -> Nonempty ( Ascii, Int )
        countCell coord cell acc =
            GridCell.flatten EverySet.empty hiddenUsers_ cell
                |> Array.foldl
                    (\( _, value ) ( acc_, index ) ->
                        ( if Bounds.contains (Grid.cellAndLocalCoordToAscii ( coord, index )) bounds then
                            Nonempty.updateFirst (Tuple.first >> (==) value) (Tuple.mapSecond ((+) 1)) acc_

                          else
                            acc_
                        , index + 1
                        )
                    )
                    ( acc, 0 )
                |> Tuple.first

        initialCount : Nonempty ( Ascii, Int )
        initialCount =
            Ascii.asciis
                |> Nonempty.toList
                |> List.remove Ascii.default
                -- We make sure the default character comes first because we are going to encounter it a lot
                |> Nonempty Ascii.default
                |> Nonempty.map (\a -> ( a, 0 ))
    in
    Bounds.coordRangeFold
        (\coord acc ->
            case Dict.get (Helper.toRawCoord coord) cells of
                Just cell ->
                    if isOnEdge coord then
                        countCell coord cell acc

                    else
                        GridCell.flatten EverySet.empty hiddenUsers_ cell
                            |> Array.foldl
                                (\( _, value ) acc_ ->
                                    Nonempty.updateFirst (Tuple.first >> (==) value) (Tuple.mapSecond ((+) 1)) acc_
                                )
                                acc

                Nothing ->
                    if isOnEdge coord then
                        countCell coord GridCell.empty acc

                    else
                        Nonempty.updateIf
                            (Tuple.first >> (==) Ascii.default)
                            (Tuple.mapSecond ((+) charsPerCell))
                            acc
        )
        identity
        adjustedBounds
        initialCount


generateMap : EverySet UserId -> Bounds CellUnit -> Grid -> Nonempty (List Ascii)
generateMap hiddenUsers_ bounds grid =
    let
        cells =
            Grid.allCellsDict grid
                |> Dict.toList
                |> List.concatMap
                    (\( ( x, y ), cell ) ->
                        if Bounds.contains (Helper.fromRawCoord ( x, y )) bounds then
                            let
                                flattened =
                                    GridCell.flatten EverySet.empty hiddenUsers_ cell |> Array.toList

                                halfCell =
                                    GridCell.cellSize // 2

                                splitRow list =
                                    list
                                        |> List.splitAt halfCell
                                        |> (\( firstList, rest ) ->
                                                let
                                                    ( secondList, rest2 ) =
                                                        List.splitAt halfCell rest
                                                in
                                                { firstList = firstList, secondList = secondList, rest = rest2 }
                                           )

                                ( topLeft, topRight, remaining ) =
                                    List.repeat halfCell ()
                                        |> List.foldl
                                            (\() ( topLeft_, topRight_, list ) ->
                                                let
                                                    { firstList, secondList, rest } =
                                                        splitRow list
                                                in
                                                ( topLeft_ ++ firstList, topRight_ ++ secondList, rest )
                                            )
                                            ( [], [], flattened )

                                ( bottomLeft, bottomRight, _ ) =
                                    List.repeat 8 ()
                                        |> List.foldl
                                            (\() ( bottomLeft_, bottomRight_, list ) ->
                                                let
                                                    { firstList, secondList, rest } =
                                                        splitRow list
                                                in
                                                ( bottomLeft_ ++ firstList, bottomRight_ ++ secondList, rest )
                                            )
                                            ( [], [], remaining )

                                listToAscii list =
                                    List.foldl
                                        (\( _, ascii ) totalIntensity ->
                                            if ascii == Ascii.default then
                                                totalIntensity

                                            else
                                                1 + totalIntensity
                                        )
                                        0
                                        list
                                        |> toFloat
                                        |> (*) (1 / toFloat charsPerCell)
                            in
                            [ ( ( x * 2, y * 2 ), listToAscii topLeft )
                            , ( ( x * 2 + 1, y * 2 ), listToAscii topRight )
                            , ( ( x * 2, y * 2 + 1 ), listToAscii bottomLeft )
                            , ( ( x * 2 + 1, y * 2 + 1 ), listToAscii bottomRight )
                            ]

                        else
                            []
                    )
                |> Dict.fromList

        charsPerCell =
            GridCell.cellSize * GridCell.cellSize

        chars =
            [ ( 0.01, Ascii.fromChar '░' )
            , ( 0.05, Ascii.fromChar '▒' )
            , ( 0.1, Ascii.fromChar '▓' )
            , ( 0.15, Ascii.fromChar '█' )
            ]
                |> List.map (Tuple.mapSecond (Maybe.withDefault Ascii.default))
    in
    Bounds.coordRangeFold
        (\coord acc ->
            case Dict.get (Helper.toRawCoord coord) cells of
                Just intensity ->
                    Nonempty.replaceHead
                        ((List.takeWhile (\( threshold, _ ) -> intensity >= threshold) chars
                            |> List.last
                            |> Maybe.map Tuple.second
                            |> Maybe.withDefault Ascii.default
                         )
                            :: Nonempty.head acc
                        )
                        acc

                Nothing ->
                    Nonempty.replaceHead (Ascii.default :: Nonempty.head acc) acc
        )
        (Nonempty.cons [])
        (Bounds.multiplyBy 2 bounds)
        (Nonempty.fromElement [])
        |> Nonempty.map List.reverse
        |> Nonempty.reverse


getUserFromSessionId : SessionId -> BackendModel -> Maybe ( UserId, BackendUserData )
getUserFromSessionId sessionId model =
    case Dict.get sessionId model.userSessions of
        Just { userId } ->
            case Dict.get (User.rawId userId) model.users of
                Just user ->
                    Just ( userId, user )

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


broadcastLocalChange :
    ( UserId, BackendUserData )
    -> Nonempty Change.LocalChange
    -> BackendModel
    -> ( BackendModel, List Effect )
broadcastLocalChange userIdAndUser changes model =
    let
        ( newModel, serverChanges ) =
            Nonempty.foldl
                (\change ( model_, serverChanges_ ) ->
                    updateLocalChange userIdAndUser change model_
                        |> Tuple.mapSecond (\serverChange -> serverChange :: serverChanges_)
                )
                ( model, [] )
                changes
                |> Tuple.mapSecond (List.filterMap identity >> List.reverse)
    in
    ( newModel
    , broadcast
        (\sessionId_ _ ->
            case getUserFromSessionId sessionId_ model of
                Just ( userId_, _ ) ->
                    if Tuple.first userIdAndUser == userId_ then
                        Nonempty.map Change.LocalChange changes |> ChangeBroadcast |> Just

                    else
                        List.filterMap
                            (\serverChange ->
                                case serverChange of
                                    Change.ServerToggleUserVisibilityForAll toggleUserId ->
                                        -- Don't let the user who got hidden know that they are hidden.
                                        if toggleUserId == userId_ then
                                            Nothing

                                        else
                                            Change.ServerChange serverChange |> Just

                                    _ ->
                                        Change.ServerChange serverChange |> Just
                            )
                            serverChanges
                            |> Nonempty.fromList
                            |> Maybe.map ChangeBroadcast

                Nothing ->
                    Nothing
        )
        model
    )


updateFromFrontend :
    Time.Posix
    -> SessionId
    -> ClientId
    -> ToBackend
    -> BackendModel
    -> ( BackendModel, List Effect )
updateFromFrontend currentTime sessionId clientId msg model =
    case msg of
        ConnectToBackend requestData maybeEmailEvent ->
            let
                ( newModel, effects ) =
                    requestDataUpdate sessionId clientId requestData model
            in
            (case maybeEmailEvent of
                Just (ConfirmationEmailConfirmed_ key) ->
                    confirmationEmailConfirmed sessionId currentTime key newModel

                Just (UnsubscribeEmail key) ->
                    unsubscribeEmail clientId key newModel

                Nothing ->
                    ( newModel, [] )
            )
                |> Tuple.mapSecond ((++) effects)

        GridChange changes ->
            case getUserFromSessionId sessionId model of
                Just userIdAndUser ->
                    broadcastLocalChange userIdAndUser changes model

                Nothing ->
                    ( model, [] )

        ChangeViewBounds bounds ->
            case
                Dict.get sessionId model.userSessions
                    |> Maybe.andThen (\{ clientIds } -> Dict.get clientId clientIds)
            of
                Just oldBounds ->
                    let
                        newCells =
                            Bounds.coordRangeFold
                                (\coord newCells_ ->
                                    if Bounds.contains coord oldBounds then
                                        newCells_

                                    else
                                        case Grid.getCell coord model.grid of
                                            Just cell ->
                                                ( coord, cell ) :: newCells_

                                            Nothing ->
                                                newCells_
                                )
                                identity
                                bounds
                                []
                    in
                    ( { model
                        | userSessions =
                            Dict.update
                                sessionId
                                (Maybe.map
                                    (\session ->
                                        { session
                                            | clientIds = Dict.update clientId (\_ -> Just bounds) session.clientIds
                                        }
                                    )
                                )
                                model.userSessions
                      }
                    , ViewBoundsChange bounds newCells
                        |> Change.ClientChange
                        |> Nonempty.fromElement
                        |> ChangeBroadcast
                        |> SendToFrontend clientId
                        |> List.singleton
                    )

                Nothing ->
                    ( model, [] )

        NotifyMeSubmitted validated ->
            case Dict.get sessionId model.userSessions of
                Just { userId } ->
                    sendConfirmationEmail validated model sessionId userId currentTime

                Nothing ->
                    ( model, [] )


confirmationEmailConfirmed : SessionId -> Time.Posix -> ConfirmEmailKey -> BackendModel -> ( BackendModel, List Effect )
confirmationEmailConfirmed sessionId currentTime confirmEmailKey model =
    case List.find (.key >> (==) confirmEmailKey) model.pendingEmails of
        Just pending ->
            let
                ( key, model2 ) =
                    generateKey UnsubscribeEmailKey model

                originalSessionId =
                    Dict.toList model.userSessions
                        |> List.find (\( _, { userId } ) -> pending.userId == userId)
                        |> Maybe.map Tuple.first
            in
            ( { model2
                | pendingEmails = List.filter (.key >> (/=) confirmEmailKey) model2.pendingEmails
                , subscribedEmails =
                    model2.subscribedEmails
                        |> List.filter (.email >> (/=) pending.email)
                        |> (::)
                            { email = pending.email
                            , frequency = pending.frequency
                            , confirmTime = currentTime
                            , userId = pending.userId
                            , unsubscribeKey = key
                            }
              }
            , broadcast
                (\sessionId_ _ ->
                    if sessionId_ == sessionId || Just sessionId_ == originalSessionId then
                        Just NotifyMeConfirmed

                    else
                        Nothing
                )
                model
            )

        Nothing ->
            ( model, [] )


unsubscribeEmail : ClientId -> UnsubscribeEmailKey -> BackendModel -> ( BackendModel, List Effect )
unsubscribeEmail clientId unsubscribeEmailKey model =
    case List.find (.unsubscribeKey >> (==) unsubscribeEmailKey) model.subscribedEmails of
        Just _ ->
            ( { model
                | subscribedEmails = List.filter (.unsubscribeKey >> (/=) unsubscribeEmailKey) model.subscribedEmails
              }
            , [ SendToFrontend clientId UnsubscribeEmailConfirmed ]
            )

        Nothing ->
            ( model, [] )


sendConfirmationEmailRateLimit : Duration
sendConfirmationEmailRateLimit =
    Duration.seconds 10


sendConfirmationEmail : NotifyMe.Validated -> BackendModel -> SessionId -> UserId -> Time.Posix -> ( BackendModel, List Effect )
sendConfirmationEmail validated model sessionId userId time =
    let
        tooEarly =
            case List.find (.email >> (==) validated.email) model.pendingEmails of
                Just { creationTime } ->
                    Duration.from creationTime time
                        |> Quantity.lessThanOrEqualTo sendConfirmationEmailRateLimit

                Nothing ->
                    False
    in
    if tooEarly then
        ( model, [] )

    else
        let
            ( key, model2 ) =
                generateKey ConfirmEmailKey model

            content =
                Html.String.div []
                    [ Html.String.a
                        [ Html.String.Attributes.href
                            (Env.domain ++ "/" ++ UrlHelper.encodeUrl (EmailConfirmationRoute key))
                        ]
                        [ Html.String.text "Click this link" ]
                    , Html.String.text
                        " to confirm you want to be notified about changes people make on ascii-collab."
                    , Html.String.br [] []
                    , Html.String.text "If this email was sent to you in error, you can safely ignore it."
                    ]
        in
        ( { model2
            | pendingEmails =
                model2.pendingEmails
                    |> List.filter (.email >> (/=) validated.email)
                    |> (::)
                        { email = validated.email
                        , frequency = validated.frequency
                        , creationTime = time
                        , key = key
                        , userId = userId
                        }
          }
        , [ SendEmail
                (ConfirmationEmailSent sessionId time)
                (NonemptyString 'C' "onfirm ascii-collab notifications")
                content
                validated.email
          ]
        )


generateKey : (String -> keyType) -> { a | secretLinkCounter : Int } -> ( keyType, { a | secretLinkCounter : Int } )
generateKey keyType model =
    ( Env.confirmationEmailKey
        ++ String.fromInt model.secretLinkCounter
        |> Crypto.Hash.sha256
        |> keyType
    , { model | secretLinkCounter = model.secretLinkCounter + 1 }
    )


updateLocalChange :
    ( UserId, BackendUserData )
    -> Change.LocalChange
    -> BackendModel
    -> ( BackendModel, Maybe ServerChange )
updateLocalChange ( userId, _ ) change model =
    case change of
        Change.LocalUndo ->
            case Dict.get (User.rawId userId) model.users of
                Just user ->
                    case Undo.undo user of
                        Just newUser ->
                            let
                                undoMoveAmount : Dict.Dict RawCellCoord Int
                                undoMoveAmount =
                                    Dict.map (\_ a -> -a) user.undoCurrent
                            in
                            ( { model
                                | grid = Grid.moveUndoPoint userId undoMoveAmount model.grid
                                , userChangesRecently =
                                    if userId == backendUserId then
                                        model.userChangesRecently

                                    else
                                        RecentChanges.undoRedoChange undoMoveAmount model.grid model.userChangesRecently
                              }
                                |> updateUser userId (always newUser)
                            , ServerUndoPoint { userId = userId, undoPoints = undoMoveAmount } |> Just
                            )

                        Nothing ->
                            ( model, Nothing )

                Nothing ->
                    ( model, Nothing )

        Change.LocalGridChange localChange ->
            case Dict.get (User.rawId userId) model.users of
                Just user ->
                    ( { model
                        | grid = Grid.addChange (Grid.localChangeToChange userId localChange) model.grid
                        , userChangesRecently =
                            if userId == backendUserId then
                                model.userChangesRecently

                            else
                                RecentChanges.addChange
                                    localChange.cellPosition
                                    (Grid.getCell localChange.cellPosition model.grid |> Maybe.withDefault GridCell.empty)
                                    model.userChangesRecently

                        --if Just userId == Env.adminUserId || userId == backendUserId then
                        --    model.userChangesRecently
                        --
                        --else
                        --    Dict.insert
                        --        ( User.rawId userId, Helper.toRawCoord localChange.cellPosition )
                        --        localChange.localPosition
                        --        model.userChangesRecently
                      }
                        |> updateUser
                            userId
                            (always { user | undoCurrent = LocalGrid.incrementUndoCurrent localChange user.undoCurrent })
                    , ServerGridChange (Grid.localChangeToChange userId localChange) |> Just
                    )

                Nothing ->
                    ( model, Nothing )

        Change.LocalRedo ->
            case Dict.get (User.rawId userId) model.users of
                Just user ->
                    case Undo.redo user of
                        Just newUser ->
                            let
                                undoMoveAmount =
                                    newUser.undoCurrent
                            in
                            ( { model
                                | grid = Grid.moveUndoPoint userId undoMoveAmount model.grid
                                , userChangesRecently =
                                    if userId == backendUserId then
                                        model.userChangesRecently

                                    else
                                        RecentChanges.undoRedoChange undoMoveAmount model.grid model.userChangesRecently
                              }
                                |> updateUser userId (always newUser)
                            , ServerUndoPoint { userId = userId, undoPoints = undoMoveAmount } |> Just
                            )

                        Nothing ->
                            ( model, Nothing )

                Nothing ->
                    ( model, Nothing )

        Change.LocalAddUndo ->
            ( updateUser userId Undo.add model, Nothing )

        Change.LocalHideUser hideUserId hidePoint ->
            ( if userId == hideUserId then
                model

              else if Dict.member (User.rawId hideUserId) model.users then
                updateUser
                    userId
                    (\user -> { user | hiddenUsers = Helper.toggleSet hideUserId user.hiddenUsers })
                    { model
                        | usersHiddenRecently =
                            if Just userId == Env.adminUserId then
                                model.usersHiddenRecently

                            else
                                { reporter = userId, hiddenUser = hideUserId, hidePoint = hidePoint } :: model.usersHiddenRecently
                    }

              else
                model
            , Nothing
            )

        Change.LocalUnhideUser unhideUserId ->
            ( updateUser
                userId
                (\user -> { user | hiddenUsers = Helper.toggleSet unhideUserId user.hiddenUsers })
                { model
                    | usersHiddenRecently =
                        List.filterMap
                            (\value ->
                                if value.reporter == userId && unhideUserId == value.hiddenUser then
                                    Nothing

                                else
                                    Just value
                            )
                            model.usersHiddenRecently
                }
            , Nothing
            )

        Change.LocalToggleUserVisibilityForAll hideUserId ->
            if Just userId == Env.adminUserId && userId /= hideUserId then
                ( updateUser hideUserId (\user -> { user | hiddenForAll = not user.hiddenForAll }) model
                , ServerToggleUserVisibilityForAll hideUserId |> Just
                )

            else
                ( model, Nothing )


updateUser : UserId -> (BackendUserData -> BackendUserData) -> BackendModel -> BackendModel
updateUser userId updateUserFunc model =
    { model | users = Dict.update (User.rawId userId) (Maybe.map updateUserFunc) model.users }


{-| Gets globally hidden users known to a specific user.
-}
hiddenUsers :
    Maybe UserId
    -> { a | users : Dict.Dict Int { b | hiddenForAll : Bool } }
    -> EverySet UserId
hiddenUsers userId model =
    model.users
        |> Dict.toList
        |> List.filterMap
            (\( userId_, { hiddenForAll } ) ->
                if hiddenForAll && userId /= Just (User.userId userId_) then
                    Just (User.userId userId_)

                else
                    Nothing
            )
        |> EverySet.fromList


requestDataUpdate : SessionId -> ClientId -> Bounds CellUnit -> BackendModel -> ( BackendModel, List Effect )
requestDataUpdate sessionId clientId viewBounds model =
    let
        loadingData ( userId, user ) =
            { user = userId
            , grid = Grid.region viewBounds model.grid
            , hiddenUsers = user.hiddenUsers
            , adminHiddenUsers = hiddenUsers (Just userId) model
            , undoHistory = user.undoHistory
            , redoHistory = user.redoHistory
            , undoCurrent = user.undoCurrent
            , viewBounds = viewBounds
            }
    in
    case getUserFromSessionId sessionId model of
        Just ( userId, user ) ->
            ( { model
                | userSessions =
                    Dict.update sessionId
                        (\maybeSession ->
                            case maybeSession of
                                Just session ->
                                    Just { session | clientIds = Dict.insert clientId viewBounds session.clientIds }

                                Nothing ->
                                    Nothing
                        )
                        model.userSessions
              }
            , [ SendToFrontend clientId (LoadingData (loadingData ( userId, user ))) ]
            )

        Nothing ->
            let
                userId =
                    Dict.size model.users |> User.userId

                ( newModel, userData ) =
                    { model
                        | userSessions =
                            Dict.insert
                                sessionId
                                { clientIds = Dict.singleton clientId viewBounds, userId = userId }
                                model.userSessions
                    }
                        |> createUser userId
            in
            ( newModel
            , [ SendToFrontend
                    clientId
                    (LoadingData (loadingData ( userId, userData )))
              ]
            )


createUser : UserId -> BackendModel -> ( BackendModel, BackendUserData )
createUser userId model =
    let
        userBackendData : BackendUserData
        userBackendData =
            { hiddenUsers = EverySet.empty
            , hiddenForAll = False
            , undoHistory = []
            , redoHistory = []
            , undoCurrent = Dict.empty
            }
    in
    ( { model | users = Dict.insert (User.rawId userId) userBackendData model.users }, userBackendData )


broadcast : (SessionId -> ClientId -> Maybe ToFrontend) -> BackendModel -> List Effect
broadcast msgFunc model =
    model.userSessions
        |> Dict.toList
        |> List.concatMap (\( sessionId, { clientIds } ) -> Dict.keys clientIds |> List.map (Tuple.pair sessionId))
        |> List.filterMap (\( sessionId, clientId ) -> msgFunc sessionId clientId |> Maybe.map (SendToFrontend clientId))
