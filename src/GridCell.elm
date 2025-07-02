module GridCell exposing (Cell(..), addLine, cellSize, changeCount, empty, flatten, hasChangesBy, moveUndoPoint, removeUser)

import Array exposing (Array)
import Ascii exposing (Ascii)
import Dict exposing (Dict)
import List.Nonempty exposing (Nonempty)
import SeqSet exposing (SeqSet)
import User exposing (RawUserId, UserId)


type Cell
    = Cell
        { history : List { userId : UserId, position : Int, line : Nonempty Ascii }
        , undoPoint : Dict RawUserId Int
        }


addLine : UserId -> Int -> Nonempty Ascii -> Cell -> Cell
addLine userId position line (Cell cell) =
    let
        userUndoPoint =
            Dict.get (User.rawId userId) cell.undoPoint |> Maybe.withDefault 0
    in
    Cell
        { history =
            List.foldr
                (\change ( newHistory, counter ) ->
                    if change.userId == userId then
                        if counter > 0 then
                            ( change :: newHistory, counter - 1 )

                        else
                            ( newHistory, counter )

                    else
                        ( change :: newHistory, counter )
                )
                ( [], userUndoPoint )
                cell.history
                |> Tuple.first
                |> (::) { userId = userId, position = position, line = line }
        , undoPoint =
            Dict.insert
                (User.rawId userId)
                (userUndoPoint + 1)
                cell.undoPoint
        }


removeUser : UserId -> Cell -> Cell
removeUser userId (Cell cell) =
    Cell
        { history = List.filter (.userId >> (==) userId) cell.history
        , undoPoint = Dict.remove (User.rawId userId) cell.undoPoint
        }


hasChangesBy : UserId -> Cell -> Bool
hasChangesBy userId (Cell cell) =
    Dict.member (User.rawId userId) cell.undoPoint


moveUndoPoint : UserId -> Int -> Cell -> Cell
moveUndoPoint userId moveAmount (Cell cell) =
    Cell
        { history = cell.history
        , undoPoint = Dict.update (User.rawId userId) (Maybe.map ((+) moveAmount)) cell.undoPoint
        }


changeCount : Cell -> Int
changeCount (Cell { history }) =
    List.length history


flatten : SeqSet UserId -> SeqSet UserId -> Cell -> Array ( Maybe UserId, Ascii )
flatten hiddenUsers hiddenUsersForAll (Cell cell) =
    let
        hidden =
            SeqSet.union hiddenUsers hiddenUsersForAll
    in
    List.foldr
        (\{ userId, position, line } state ->
            if SeqSet.member userId hidden then
                state

            else
                case Dict.get (User.rawId userId) state.undoPoint of
                    Just stepsLeft ->
                        if stepsLeft > 0 then
                            { array =
                                List.Nonempty.foldl
                                    (\ascii ( position_, state_ ) ->
                                        ( position_ + 1, Array.set position_ ( Just userId, ascii ) state_ )
                                    )
                                    ( position, state.array )
                                    line
                                    |> Tuple.second
                            , undoPoint = Dict.insert (User.rawId userId) (stepsLeft - 1) state.undoPoint
                            }

                        else
                            state

                    Nothing ->
                        state
        )
        { array = Array.initialize (cellSize * cellSize) (\_ -> ( Nothing, Ascii.default )), undoPoint = cell.undoPoint }
        cell.history
        |> .array


cellSize : Int
cellSize =
    16


empty : Cell
empty =
    Cell { history = [], undoPoint = Dict.empty }
