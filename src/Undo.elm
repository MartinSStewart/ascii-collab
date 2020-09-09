module Undo exposing (add, redo, undo)

import Dict exposing (Dict)


redo :
    { a | redoHistory : List b, undoCurrent : b, undoHistory : List b }
    -> Maybe { a | redoHistory : List b, undoCurrent : b, undoHistory : List b }
redo model =
    case model.redoHistory of
        head :: rest ->
            Just
                { model
                    | undoHistory = model.undoCurrent :: model.undoHistory
                    , redoHistory = rest
                    , undoCurrent = head
                }

        [] ->
            Nothing


undo :
    { a | undoHistory : List b, undoCurrent : b, redoHistory : List b }
    -> Maybe { a | undoHistory : List b, undoCurrent : b, redoHistory : List b }
undo model =
    case model.undoHistory of
        head :: rest ->
            Just
                { model
                    | undoHistory = rest
                    , redoHistory = model.undoCurrent :: model.redoHistory
                    , undoCurrent = head
                }

        [] ->
            Nothing


add :
    { a | undoCurrent : Dict k v, undoHistory : List (Dict k v), redoHistory : List b }
    -> { a | undoCurrent : Dict k v, undoHistory : List (Dict k v), redoHistory : List b }
add model =
    { model
        | redoHistory = []
        , undoHistory = model.undoCurrent :: model.undoHistory
        , undoCurrent = Dict.empty
    }
