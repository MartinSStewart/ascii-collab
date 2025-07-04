module Fusion.SeqSet exposing (build_SeqSet, patch_SeqSet, patcher_SeqSet, toValue_SeqSet)

{-| -}

import Fusion exposing (Value(..))
import Fusion.Patch exposing (Error(..), Patch, Patcher, patch_List)
import Result.Extra
import SeqSet exposing (SeqSet)


{-| -}
patcher_SeqSet : Patcher value -> Patcher (SeqSet value)
patcher_SeqSet patcher =
    { patch = patch_SeqSet patcher
    , build = build_SeqSet patcher
    , toValue = toValue_SeqSet patcher
    }


{-| -}
patch_SeqSet : Patcher value -> { force : Bool } -> Patch -> SeqSet value -> Result Error (SeqSet value)
patch_SeqSet patcher options p value =
    value
        |> SeqSet.toList
        |> patch_List patcher options p
        |> Result.map SeqSet.fromList


{-| -}
build_SeqSet : Patcher value -> Value -> Result Error (SeqSet value)
build_SeqSet patcher p =
    case p of
        VSet lpatch ->
            lpatch.items
                |> Result.Extra.combineMap patcher.build
                |> Result.map SeqSet.fromList

        _ ->
            Err (WrongType "Patch.build_SeqSet")


{-| -}
toValue_SeqSet : Patcher value -> SeqSet value -> Value
toValue_SeqSet patcher value =
    VSet
        { cursor = 0
        , items =
            SeqSet.toList value
                |> List.map (\item -> patcher.toValue item)
        }
