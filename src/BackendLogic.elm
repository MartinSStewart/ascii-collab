module BackendLogic exposing (Effect(..), init, statistics, update, updateFromFrontend)

import Array
import Ascii exposing (Ascii)
import Bounds exposing (Bounds)
import Change exposing (ClientChange(..), ServerChange(..))
import Dict
import Env
import EverySet exposing (EverySet)
import Grid exposing (Grid)
import GridCell
import Helper exposing (Coord)
import List.Extra as List
import List.Nonempty as Nonempty exposing (Nonempty(..))
import LocalGrid
import NonemptyExtra as Nonempty
import Quantity exposing (Quantity(..))
import SendGrid
import String.Nonempty
import Types exposing (..)
import Undo
import Units exposing (AsciiUnit, CellUnit)
import UrlHelper
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
    , userChangesRecently = Dict.empty
    }


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

        NotifyAdminTimeElapsed _ ->
            let
                ( newModel, cmd ) =
                    notifyAdmin model

                ( newModel3, cmd3 ) =
                    case Dict.get (User.rawId backendUserId) newModel.users of
                        Just userData ->
                            drawStatistics ( backendUserId, userData ) newModel

                        Nothing ->
                            let
                                ( newModel2, userData ) =
                                    createUser backendUserId newModel
                            in
                            drawStatistics ( backendUserId, userData ) newModel2
            in
            ( newModel3
            , cmd ++ cmd3
            )

        NotifyAdminEmailSent ->
            ( model, [] )


notifyAdmin : BackendModel -> ( BackendModel, List Effect )
notifyAdmin model =
    let
        idToString =
            User.rawId >> String.fromInt

        fullUrl point =
            Env.domain ++ "/" ++ UrlHelper.encodeUrl point

        changes =
            Dict.toList model.userChangesRecently
                |> List.gatherEqualsBy (\( ( userId, _ ), _ ) -> userId)
                |> List.map
                    (\( ( ( userId, _ ), _ ) as head, rest ) ->
                        List.map
                            (\( ( _, cellCoord ), localPos ) ->
                                Grid.cellAndLocalCoordToAscii ( Helper.fromRawCoord cellCoord, localPos )
                                    |> fullUrl
                                    |> (++) "\n    "
                            )
                            (head :: rest)
                            |> String.concat
                            |> (++) ("User " ++ String.fromInt userId ++ " made changes at ")
                    )
                |> String.join "\n"

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
    case
        ( List.isEmpty model.usersHiddenRecently && Dict.isEmpty model.userChangesRecently
        , String.Nonempty.fromString (changes ++ "\n\n" ++ hidden)
        )
    of
        ( False, Just content ) ->
            ( { model | usersHiddenRecently = [], userChangesRecently = Dict.empty }
            , [ SendEmail
                    (always NotifyAdminEmailSent)
                    { subject =
                        String.Nonempty.append_
                            (String.Nonempty.fromInt (List.length model.usersHiddenRecently))
                            (" users hidden & "
                                ++ String.fromInt (Dict.size model.userChangesRecently)
                                ++ " cells changed"
                            )
                    , content = SendGrid.textContent content
                    , to = Nonempty.fromElement Env.adminEmail
                    , cc = []
                    , bcc = []
                    , nameOfSender = "ascii-collab"
                    , emailAddressOfSender = "ascii-collab@lamdera.app"
                    }
              ]
            )

        _ ->
            ( model, [] )


backendUserId : UserId
backendUserId =
    User.userId -1


drawStatistics : ( UserId, BackendUserData ) -> BackendModel -> ( BackendModel, List Effect )
drawStatistics ( userId, userData ) model =
    let
        stats : Nonempty ( Ascii, Int )
        stats =
            statistics (hiddenUsers userId model) Env.statisticsBounds model.grid

        map : Nonempty (List Ascii)
        map =
            generateMap
                (hiddenUsers userId model)
                (Bounds.convert (Grid.asciiToCellAndLocalCoord >> Tuple.first) Env.statisticsBounds)
                model.grid

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
                |> Nonempty.greedyGroupsOf 32
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
                        case List.repeat (32 - length) [] |> Nonempty.fromList of
                            Just padding ->
                                Nonempty.append list padding

                            Nothing ->
                                list
                    )
                |> Nonempty.transpose
                |> Nonempty.map (Nonempty.toList >> List.concat)
    in
    Grid.textToChange Env.statisticsDrawAt statText
        |> Nonempty.append (Grid.textToChange Env.mapDrawAt map)
        |> Nonempty.map Change.LocalGridChange
        -- Remove previous statistics so the undo history doesn't get really long
        |> Nonempty.append (Nonempty Change.LocalUndo [ Change.LocalAddUndo ])
        |> (\a -> broadcastLocalChange ( userId, userData ) a model)


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
                Just cell ->
                    let
                        intensity : Float
                        intensity =
                            GridCell.flatten EverySet.empty hiddenUsers_ cell
                                |> Array.foldl
                                    (\( _, ascii ) totalIntensity ->
                                        if ascii == Ascii.default then
                                            totalIntensity

                                        else
                                            180 + totalIntensity
                                     --Ascii.intensity ascii + totalIntensity
                                    )
                                    0
                                |> toFloat
                                |> (*) (1 / toFloat (Helper.area Ascii.size * charsPerCell))
                    in
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
        bounds
        (Nonempty.fromElement [])
        |> Nonempty.map List.reverse


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


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, List Effect )
updateFromFrontend sessionId clientId msg model =
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
                                undoMoveAmount =
                                    Dict.map (\_ a -> -a) user.undoCurrent
                            in
                            ( { model | grid = Grid.moveUndoPoint userId undoMoveAmount model.grid }
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
                            if Just userId == Env.adminUserId || userId == backendUserId then
                                model.userChangesRecently

                            else
                                Dict.insert
                                    ( User.rawId userId, Helper.toRawCoord localChange.cellPosition )
                                    localChange.localPosition
                                    model.userChangesRecently
                      }
                        |> updateUser userId (always { user | undoCurrent = LocalGrid.incrementUndoCurrent localChange user.undoCurrent })
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
hiddenUsers : UserId -> BackendModel -> EverySet UserId
hiddenUsers userId model =
    model.users
        |> Dict.toList
        |> List.filterMap
            (\( userId_, { hiddenForAll } ) ->
                if hiddenForAll && userId /= User.userId userId_ then
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
            , adminHiddenUsers = hiddenUsers userId model
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
