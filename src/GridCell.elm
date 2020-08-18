module GridCell exposing (Cell, addLine, cellSize, changeCount, empty, flatten)

import Array exposing (Array)
import Ascii exposing (Ascii)
import Dict exposing (Dict)
import List.Extra as List
import List.Nonempty exposing (Nonempty)
import User exposing (UserId)


type alias RawUserId =
    Int


type Cell
    = Cell { history : List { userId : UserId, position : Int, line : Nonempty Ascii }, undoPoint : Dict RawUserId Int }


addLine : UserId -> Int -> Nonempty Ascii -> Cell -> Cell
addLine userId position line (Cell { history, undoPoint }) =
    Cell
        { history = { userId = userId, position = position, line = line } :: history
        , undoPoint =
            Dict.update
                (User.rawId userId)
                (Maybe.map ((+) 1) >> Maybe.withDefault 1 >> Just)
                undoPoint
        }



--removeLinesAfter : UserId -> Int -> Cell -> Cell
--removeLinesAfter userId changeCount_ (Cell history) =
--    List.foldl
--        (\change ( newHistory, counter ) ->
--            if change.userId == userId then
--                if counter > 0 then
--                    ( change :: newHistory, counter - 1 )
--
--                else
--                    ( newHistory, counter )
--
--            else
--                ( change :: newHistory, counter )
--        )
--        ( history, changeCount_ )
--        history
--        |> Tuple.first
--        |> List.reverse
--        |> Cell


changeCount : Cell -> Int
changeCount (Cell { history }) =
    List.length history


flatten : Cell -> Array Ascii
flatten (Cell { history, undoPoint }) =
    List.foldr
        (\{ userId, position, line } state ->
            case Dict.get (User.rawId userId) state.undoPoint of
                Just stepsLeft ->
                    if stepsLeft > 0 then
                        { array =
                            List.Nonempty.foldl
                                (\ascii ( position_, state_ ) ->
                                    ( position_ + 1, Array.set position_ ascii state_ )
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
        { array = Array.initialize (cellSize * cellSize) (\_ -> Ascii.default), undoPoint = undoPoint }
        history
        |> .array


cellSize : Int
cellSize =
    16


empty : Cell
empty =
    Cell { history = [], undoPoint = Dict.empty }
