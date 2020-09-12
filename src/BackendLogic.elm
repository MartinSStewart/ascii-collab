module BackendLogic exposing (Effect(..), init, update, updateFromFrontend)

import Bounds exposing (Bounds)
import Change exposing (ClientChange(..), ServerChange(..))
import Dict
import Env
import EverySet exposing (EverySet)
import Grid
import Helper
import List.Nonempty exposing (Nonempty)
import LocalGrid
import SendGrid
import String.Nonempty
import Types exposing (..)
import Undo
import Units exposing (CellUnit)
import UrlHelper
import User exposing (UserData, UserId)


type Effect
    = Effect ClientId ToFrontend


init : BackendModel
init =
    { grid = Grid.empty, userSessions = Dict.empty, users = Dict.empty, usersHiddenRecently = [] }


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
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
            , Cmd.none
            )

        NotifyAdminTimeElapsed time ->
            let
                maybeContent =
                    List.map
                        (\{ reporter, hiddenUser, hidePoint } ->
                            "User "
                                ++ String.fromInt (User.rawId reporter)
                                ++ " hid user "
                                ++ String.fromInt (User.rawId hiddenUser)
                                ++ "'s text at "
                                ++ (Env.domain ++ "/" ++ UrlHelper.encodeUrl hidePoint)
                        )
                        model.usersHiddenRecently
                        |> String.join "\n"
                        |> String.Nonempty.fromString
            in
            case ( model.usersHiddenRecently, maybeContent ) of
                ( _ :: _, Just content ) ->
                    ( { model | usersHiddenRecently = [] }
                    , SendGrid.sendEmail
                        (always NotifyAdminEmailSent)
                        Env.sendGridKey
                        { subject =
                            String.Nonempty.append_
                                (String.Nonempty.fromInt (List.length model.usersHiddenRecently))
                                " users hidden"
                        , content = SendGrid.textContent content
                        , to = List.Nonempty.fromElement Env.adminEmail
                        , cc = []
                        , bcc = []
                        , nameOfSender = "ascii-collab"
                        , emailAddressOfSender = "ascii-collab@lamdera.app"
                        }
                    )

                _ ->
                    ( model, Cmd.none )

        NotifyAdminEmailSent ->
            ( model, Cmd.none )


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
            List.Nonempty.foldl
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
                        List.Nonempty.map Change.LocalChange changes |> ChangeBroadcast |> Just

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
                            |> List.Nonempty.fromList
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
                        |> List.Nonempty.fromElement
                        |> ChangeBroadcast
                        |> Effect clientId
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
            { user = ( userId, user.userData )
            , grid = Grid.region viewBounds model.grid
            , hiddenUsers = user.hiddenUsers
            , adminHiddenUsers = hiddenUsers userId model
            , otherUsers =
                Dict.toList model.users
                    |> List.map (Tuple.mapBoth User.userId .userData)
                    |> List.filter (Tuple.first >> (/=) userId)
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
            , [ Effect clientId (LoadingData (loadingData ( userId, user ))) ]
            )

        Nothing ->
            let
                ( userId, user ) =
                    Dict.size model.users |> User.newUser

                userBackendData : BackendUserData
                userBackendData =
                    { userData = user
                    , hiddenUsers = EverySet.empty
                    , hiddenForAll = False
                    , undoHistory = []
                    , redoHistory = []
                    , undoCurrent = Dict.empty
                    }
            in
            ( { model
                | userSessions =
                    Dict.insert
                        sessionId
                        { clientIds = Dict.singleton clientId viewBounds, userId = userId }
                        model.userSessions
                , users = Dict.insert (User.rawId userId) userBackendData model.users
              }
            , Effect
                clientId
                (LoadingData (loadingData ( userId, userBackendData )))
                :: broadcast
                    (\sessionId_ _ ->
                        if sessionId == sessionId_ then
                            Nothing

                        else
                            ServerUserNew ( userId, user )
                                |> Change.ServerChange
                                |> List.Nonempty.fromElement
                                |> ChangeBroadcast
                                |> Just
                    )
                    model
            )


broadcast : (SessionId -> ClientId -> Maybe ToFrontend) -> BackendModel -> List Effect
broadcast msgFunc model =
    model.userSessions
        |> Dict.toList
        |> List.concatMap (\( sessionId, { clientIds } ) -> Dict.keys clientIds |> List.map (Tuple.pair sessionId))
        |> List.filterMap (\( sessionId, clientId ) -> msgFunc sessionId clientId |> Maybe.map (Effect clientId))
