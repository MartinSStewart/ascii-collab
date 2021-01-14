module BackendLogic exposing (Effect(..), init, notifyAdminWait, sendConfirmationEmailRateLimit, statistics, update, updateFromFrontend)

import Array exposing (Array)
import Ascii exposing (Ascii)
import Bounds exposing (Bounds)
import Change exposing (ClientChange(..), ServerChange(..))
import Cluster
import Crypto.Hash
import Dict
import Duration exposing (Duration)
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
import String.Nonempty exposing (NonemptyString(..))
import Time
import Types exposing (..)
import Undo
import Units exposing (AsciiUnit, CellUnit)
import UrlHelper exposing (ConfirmEmailKey(..), InternalRoute(..))
import User exposing (UserId)


type Effect
    = SendToFrontend ClientId ToFrontend
    | SendEmail (Result SendGrid.Error () -> BackendMsg) (SendGrid.Email ())


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
                --( newModel, cmd ) =
                --    notifyAdmin model
                --
                --( newModel3, cmd3 ) =
                --    case Dict.get (User.rawId backendUserId) newModel.users of
                --        Just userData ->
                --            drawStatistics time ( backendUserId, userData ) newModel
                --
                --        Nothing ->
                --            let
                --                ( newModel2, userData ) =
                --                    createUser backendUserId newModel
                --            in
                --            drawStatistics time ( backendUserId, userData ) newModel2
                ( frequencyChanges, recentChangeState ) =
                    RecentChanges.threeHoursElapsed model.userChangesRecently

                getActualChanges : Dict.Dict RawCellCoord GridCell.Cell -> List ( RawCellCoord, Array ( Maybe UserId, Ascii ) )
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

                clusters :
                    List ( RawCellCoord, Array ( Maybe UserId, Ascii ) )
                    -> List ( Bounds CellUnit, Nonempty (Coord CellUnit) )
                clusters actualChanges =
                    List.map Tuple.first actualChanges |> Set.fromList |> Cluster.cluster

                content actualChanges =
                    List.map (\( bounds, _ ) -> clusterToTextImage model actualChanges bounds) (clusters actualChanges)
                        |> Html.String.div []

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
                    let
                        content_ =
                            content (getActualChanges changes) |> SendGrid.htmlContent

                        subject_ =
                            subject frequency
                    in
                    List.filter (.frequency >> (==) frequency)
                        model.subscribedEmails
                        |> List.map
                            (\email ->
                                SendEmail
                                    (ChangeEmailSent time email.email)
                                    (asciiCollabEmail
                                        subject_
                                        content_
                                        email.email
                                    )
                            )
                 --asciiCollabEmail
                 --SendEmail
                 --(ChangeEmailSent time)
                )
                frequencyChanges
            )

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


clusterToTextImage :
    { a | grid : Grid, users : Dict.Dict Int { b | hiddenForAll : Bool } }
    -> List ( RawCellCoord, Array ( Maybe UserId, Ascii ) )
    -> Bounds CellUnit
    -> Html.String.Html msg
clusterToTextImage model actualChanges bounds =
    Bounds.coordRangeFoldReverse
        (\coord ( value, a ) ->
            let
                rawCoord =
                    Helper.toRawCoord coord

                array =
                    case List.find (Tuple.first >> (==) rawCoord) actualChanges of
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
            ( List.map2 (\slice rest -> slice ++ rest) slices value
            , a
            )
        )
        (\( a, b ) -> ( List.repeat GridCell.cellSize [], a :: b ))
        bounds
        ( List.repeat GridCell.cellSize [], [] )
        |> (\( a, b ) -> a :: b)
        |> List.concat
        |> List.map
            (\row ->
                List.foldl
                    (\( maybeUserId, ascii ) ( asciiChanged, text, html ) ->
                        if (maybeUserId == Nothing) == asciiChanged then
                            ( asciiChanged, String.fromChar (Ascii.toChar ascii) ++ text, html )

                        else
                            ( maybeUserId == Nothing
                            , Ascii.toChar ascii |> String.fromChar
                            , (case maybeUserId of
                                Just userId ->
                                    Html.String.span
                                        [ Html.String.Attributes.style "color" "blue" ]
                                        [ Html.String.text text ]

                                Nothing ->
                                    Html.String.text text
                              )
                                :: html
                            )
                    )
                    ( False, "", [] )
                    row
                    |> (\( _, _, a ) -> List.reverse a)
            )
        |> List.intersperse [ Html.String.br [] [] ]
        |> List.concat
        |> Html.String.div []


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

        --changes =
        --    Dict.toList model.userChangesRecently
        --        |> List.gatherEqualsBy (\( ( userId, _ ), _ ) -> userId)
        --        |> List.map
        --            (\( ( ( userId, _ ), _ ) as head, rest ) ->
        --                List.map
        --                    (\( ( _, cellCoord ), localPos ) ->
        --                        Grid.cellAndLocalCoordToAscii ( Helper.fromRawCoord cellCoord, localPos )
        --                            |> fullUrl
        --                            |> (++) "\n    "
        --                    )
        --                    (head :: rest)
        --                    |> String.concat
        --                    |> (++) ("User " ++ String.fromInt userId ++ " made changes at ")
        --            )
        --        |> String.join "\n"
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
    --case
    --    ( List.isEmpty model.usersHiddenRecently && Dict.isEmpty model.userChangesRecently
    --    , String.Nonempty.fromString (changes ++ "\n\n" ++ hidden)
    --    )
    --of
    --    ( False, Just content ) ->
    --        ( { model | usersHiddenRecently = [], userChangesRecently = Dict.empty }
    --        , [ SendEmail
    --                (always NotifyAdminEmailSent)
    --                { subject =
    --                    String.Nonempty.append_
    --                        (String.Nonempty.fromInt (List.length model.usersHiddenRecently))
    --                        (" users hidden & "
    --                            ++ String.fromInt (Dict.size model.userChangesRecently)
    --                            ++ " cells changed"
    --                        )
    --                , content = SendGrid.textContent content
    --                , to = Nonempty.fromElement Env.adminEmail
    --                , cc = []
    --                , bcc = []
    --                , nameOfSender = "ascii-collab"
    --                , emailAddressOfSender = "ascii-collab@lamdera.app"
    --                }
    --          ]
    --        )
    --
    --    _ ->
    ( model, [] )


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
        RequestData requestData ->
            requestDataUpdate sessionId clientId requestData model

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

        ConfirmationEmailConfirmed_ confirmEmailKey ->
            case List.find (.key >> (==) confirmEmailKey) model.pendingEmails of
                Just pending ->
                    ( { model
                        | pendingEmails = List.filter (.key >> (/=) confirmEmailKey) model.pendingEmails
                        , subscribedEmails =
                            model.subscribedEmails
                                |> List.filter (.email >> (/=) pending.email)
                                |> (::)
                                    { email = pending.email
                                    , frequency = pending.frequency
                                    , confirmTime = currentTime
                                    , userId = pending.userId
                                    }
                      }
                    , broadcast
                        (\sessionId_ _ ->
                            if sessionId_ == sessionId then
                                Just NotifyMeConfirmed

                            else
                                Nothing
                        )
                        model
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
            key : ConfirmEmailKey
            key =
                Env.confirmationEmailKey
                    ++ String.fromInt model.secretLinkCounter
                    |> Crypto.Hash.sha256
                    |> ConfirmEmailKey

            content : SendGrid.Content msg
            content =
                SendGrid.htmlContent
                    (Html.String.div []
                        [ Html.String.a
                            [ Html.String.Attributes.href
                                ("https://ascii-collab.lamdera.app"
                                    ++ UrlHelper.encodeUrl (EmailConfirmationRoute key)
                                )
                            ]
                            [ Html.String.text "Click this link" ]
                        , Html.String.text
                            " to confirm you want to be notified about changes people make on ascii-collab."
                        , Html.String.text "If this email was sent to you in error, you can safely ignore it."
                        ]
                    )

            email : SendGrid.Email a
            email =
                asciiCollabEmail
                    (NonemptyString 'C' "onfirm ascii-collab notifications")
                    content
                    validated.email
        in
        ( { model
            | pendingEmails =
                model.pendingEmails
                    |> List.filter (.email >> (/=) validated.email)
                    |> (::)
                        { email = validated.email
                        , frequency = validated.frequency
                        , creationTime = time
                        , key = key
                        , userId = userId
                        }
            , secretLinkCounter = model.secretLinkCounter + 1
          }
        , [ SendEmail (ConfirmationEmailSent sessionId time) email ]
        )


asciiCollabEmail : NonemptyString -> SendGrid.Content a -> Email.Email -> SendGrid.Email a
asciiCollabEmail subject content to =
    { subject = subject
    , content = content
    , to = Nonempty.fromElement (Email.toString to)
    , cc = []
    , bcc = []
    , nameOfSender = "ascii-collab"
    , emailAddressOfSender = "ascii-collab@lamdera.app"
    }


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
