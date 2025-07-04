module Fusion.SeqDict exposing (build_SeqDict, patch_SeqDict, patcher_SeqDict, toValue_SeqDict)

{-| -}

import Fusion exposing (Value(..))
import Fusion.Patch exposing (Error(..), Patch(..), Patcher)
import Fusion.ValueDict as ValueDict exposing (ValueDict)
import SeqDict exposing (SeqDict)


{-| -}
patcher_SeqDict : Patcher key -> Patcher value -> Patcher (SeqDict key value)
patcher_SeqDict keyPatcher valuePatcher =
    { patch = patch_SeqDict keyPatcher valuePatcher
    , build = build_SeqDict keyPatcher valuePatcher
    , toValue = toValue_SeqDict keyPatcher valuePatcher
    }


{-| -}
patch_SeqDict :
    Patcher key
    -> Patcher value
    -> { force : Bool }
    -> Patch
    -> SeqDict key value
    -> Result Error (SeqDict key value)
patch_SeqDict keyPatcher valuePatcher options p value =
    -- TODO: Check when removing that we're removing the expected value
    -- or that force is True
    case p of
        PDict dpatch ->
            let
                foldOn :
                    ValueDict b
                    -> (Value -> b -> c -> Result Error c)
                    -> Result Error c
                    -> Result Error c
                foldOn prop f init =
                    case init of
                        Err e ->
                            Err e

                        Ok _ ->
                            ValueDict.foldl
                                (\key item acc ->
                                    case acc of
                                        Err e ->
                                            Err e

                                        Ok oacc ->
                                            Result.mapError (ErrorAtValueWithKey key) (f key item oacc)
                                )
                                init
                                prop
            in
            Ok value
                |> foldOn dpatch.removed
                    (\removedKey _ acc ->
                        Result.map
                            (\key -> SeqDict.remove key acc)
                            (keyPatcher.build removedKey)
                    )
                |> foldOn dpatch.added
                    (\addedKey addedValue acc ->
                        Result.map2 (\k v -> SeqDict.insert k v acc)
                            (keyPatcher.build addedKey)
                            (valuePatcher.build addedValue)
                    )
                |> foldOn dpatch.edited
                    (\changedKey valuePatch acc ->
                        keyPatcher.build changedKey
                            |> Result.andThen
                                (\key ->
                                    case SeqDict.get key acc of
                                        Nothing ->
                                            -- The value was removed by someone else
                                            Err Conflict

                                        Just item ->
                                            valuePatcher.patch options valuePatch item
                                                |> Result.map (\patched -> SeqDict.insert key patched acc)
                                )
                    )

        _ ->
            Err (WrongType "Patch.")


{-| -}
build_SeqDict :
    Patcher key
    -> Patcher value
    -> Value
    -> Result Error (SeqDict key value)
build_SeqDict keyPatcher valuePatcher p =
    case p of
        VDict fields ->
            List.foldl
                (\( addedKeyPatch, addedValuePatch ) acc ->
                    Result.map3 SeqDict.insert
                        (Result.mapError (ErrorAtKey addedKeyPatch) <| keyPatcher.build addedKeyPatch)
                        (Result.mapError (ErrorAtValueWithKey addedKeyPatch) <| valuePatcher.build addedValuePatch)
                        acc
                )
                (Ok SeqDict.empty)
                fields.items

        _ ->
            Err (WrongType "Patch.")


{-| -}
toValue_SeqDict :
    Patcher key
    -> Patcher value
    -> SeqDict key value
    -> Value
toValue_SeqDict keyPatcher valuePatcher dict =
    VDict
        { cursor = 0
        , items =
            SeqDict.toList dict
                |> List.map
                    (\( key, value ) ->
                        ( keyPatcher.toValue key
                        , valuePatcher.toValue value
                        )
                    )
        }
