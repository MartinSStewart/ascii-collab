module Fusion.Patch exposing
    ( Patch(..), Error(..), Patcher, ListPatch, SetPatch, DictPatch
    , patch, merge, applyQueryResult
    , build_Array, build_Bool, build_Bytes, build_Char, build_Dict, build_Float, build_Int, build_Json, build_List, build_Never, build_Posix, build_Record, build_Set, build_String, build_Triple, build_Tuple, build_Unit, build_Custom
    , patch_Array, patch_Bool, patch_Bytes, patch_Char, patch_Dict, patch_Float, patch_Int, patch_Json, patch_List, patch_Never, patch_Posix, patch_Record, patch_Set, patch_String, patch_Triple, patch_Tuple, patch_Unit, patcher_Array, patcher_Bool, patcher_Bytes, patcher_Char, patcher_Dict, patcher_Float, patcher_Int, patcher_Json, patcher_List, patcher_Never, patcher_Posix, patcher_Set, patcher_String, patcher_Triple, patcher_Tuple, patcher_Unit
    , toValue_Array, toValue_Dict, toValue_List, toValue_Never, toValue_Posix, toValue_Set, toValue_Triple, toValue_Tuple
    , maybeApply, buildFromPatch, simplify, map
    )

{-|

@docs Patch, Error, Patcher, ListPatch, SetPatch, DictPatch

@docs patch, merge, applyQueryResult


# Builders

@docs build_Array, build_Bool, build_Bytes, build_Char, build_Dict, build_Float, build_Int, build_Json, build_List, build_Never, build_Posix, build_Record, build_Set, build_String, build_Triple, build_Tuple, build_Unit, build_Custom


# Patchers

@docs patch_Array, patch_Bool, patch_Bytes, patch_Char, patch_Dict, patch_Float, patch_Int, patch_Json, patch_List, patch_Never, patch_Posix, patch_Record, patch_Set, patch_String, patch_Triple, patch_Tuple, patch_Unit, patcher_Array, patcher_Bool, patcher_Bytes, patcher_Char, patcher_Dict, patcher_Float, patcher_Int, patcher_Json, patcher_List, patcher_Never, patcher_Posix, patcher_Set, patcher_String, patcher_Triple, patcher_Tuple, patcher_Unit


# To Value

@docs toValue_Array, toValue_Dict, toValue_List, toValue_Never, toValue_Posix, toValue_Set, toValue_Triple, toValue_Tuple


# Utilities

@docs maybeApply, buildFromPatch, simplify, map

-}

import Array exposing (Array)
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Fusion exposing (Query(..), Value(..))
import Fusion.Value
import Fusion.ValueDict as ValueDict exposing (ValueDict)
import Json.Decode
import Json.Encode
import List.Extra
import Result.Extra
import Set exposing (Set)
import Time


{-| -}
type Patch
    = PInt Int Int
    | PFloat Float Float
    | PString String String
    | PBool Bool Bool
    | PChar Char Char
    | PBytes Bytes Bytes
    | PUnit -- used when converting a Value into a Patch
    | PTuple (Maybe Patch) (Maybe Patch)
    | PTriple (Maybe Patch) (Maybe Patch) (Maybe Patch)
    | PList ListPatch
    | PSet SetPatch
    | PDict DictPatch
    | PCustomSame String (List (Maybe Patch))
    | PCustomChange String String (List Value) -- from // to // args -- TODO: maybe pass Value as from?
    | PRecord (Dict String Patch)
    | PSetCursor Int


{-| -}
type Error
    = ErrorAtField String Error
    | ErrorAtFirst Error
    | ErrorAtSecond Error
    | ErrorAtThird Error
    | ErrorAtCustomArgument Int Error
    | ErrorAtIndex Int Error
    | ErrorAtKey Value Error
    | ErrorAtValueWithKey Value Error
    | CouldNotBuildValueFromPatch
    | Conflict
    | WrongType String
    | MissingField String
    | MissingValue
    | UnexpectedField String


{-| -}
type alias ListPatch =
    { edited : Dict Int Patch
    , removed : Dict Int Value
    , added : Dict Int Value
    }


{-| -}
type alias SetPatch =
    { removed : ValueDict ()
    , added : ValueDict ()
    }


{-| -}
type alias DictPatch =
    { edited : ValueDict Patch
    , removed : ValueDict Value
    , added : ValueDict Value
    }


{-| -}
type alias Patcher a =
    { patch : { force : Bool } -> Patch -> a -> Result Error a
    , build : Value -> Result Error a
    , toValue : a -> Value
    }


{-| -}
applyQueryResult : Query -> Value -> Value -> Maybe Value
applyQueryResult query result value =
    case ( query, value ) of
        ( QLoad, _ ) ->
            Just result

        ( QRecord queryFieldName child, VRecord fields ) ->
            Dict.get queryFieldName fields
                |> Maybe.andThen
                    (\fieldValue -> applyQueryResult child result fieldValue)
                |> Maybe.map
                    (\patched ->
                        fields
                            |> Dict.insert queryFieldName patched
                            |> VRecord
                    )

        ( QRecord _ _, _ ) ->
            Nothing

        ( QIndexed (VInt index) child, VList l ) ->
            l.items
                |> List.Extra.getAt index
                |> Maybe.andThen (\old -> applyQueryResult child result old)
                |> Maybe.map (\new -> VList { l | items = List.Extra.setAt index new l.items })

        ( QIndexed (VInt index) child, VTuple l r ) ->
            case index of
                0 ->
                    Maybe.map (\new -> VTuple new r) (applyQueryResult child result l)

                1 ->
                    Maybe.map (\new -> VTuple l new) (applyQueryResult child result r)

                _ ->
                    Nothing

        ( QIndexed (VInt index) child, VTriple l m r ) ->
            case index of
                0 ->
                    Maybe.map (\new -> VTriple new m r) (applyQueryResult child result l)

                1 ->
                    Maybe.map (\new -> VTriple l new r) (applyQueryResult child result m)

                2 ->
                    Maybe.map (\new -> VTriple l m new) (applyQueryResult child result r)

                _ ->
                    Nothing

        ( QIndexed (VInt index) child, VSet l ) ->
            l.items
                |> List.Extra.getAt index
                |> Maybe.andThen (\old -> applyQueryResult child result old)
                |> Maybe.map (\new -> VSet { l | items = List.Extra.setAt index new l.items })

        ( QIndexed index child, VDict l ) ->
            l.items
                |> List.Extra.find (\( k, _ ) -> Fusion.Value.compare k index == EQ)
                |> Maybe.andThen (\( key, old ) -> Maybe.map (Tuple.pair key) (applyQueryResult child result old))
                |> Maybe.map
                    (\new ->
                        VDict
                            { l
                                | items =
                                    List.Extra.updateIf
                                        (\( k, _ ) -> Fusion.Value.compare k index == EQ)
                                        (\_ -> new)
                                        l.items
                            }
                    )

        ( QIndexed _ _, _ ) ->
            Nothing


{-| -}
buildFromPatch : (Value -> Result Error a) -> Patch -> Result Error a
buildFromPatch builder p =
    Result.andThen builder (patchToValue p)


{-| -}
patchToValue : Patch -> Result Error Value
patchToValue p =
    case p of
        PInt _ i ->
            Ok (VInt i)

        PFloat _ i ->
            Ok <| VFloat i

        PString _ i ->
            Ok <| VString i

        PChar _ i ->
            Ok <| VChar i

        PBool _ i ->
            Ok <| VBool i

        PUnit ->
            Ok VUnit

        PBytes _ b ->
            Ok (VBytes b)

        PTuple (Just l) (Just r) ->
            mapTuple VTuple
                (patchToValue l)
                (patchToValue r)

        PTuple Nothing _ ->
            Err <| ErrorAtFirst CouldNotBuildValueFromPatch

        PTuple _ Nothing ->
            Err <| ErrorAtSecond CouldNotBuildValueFromPatch

        PTriple (Just l) (Just m) (Just r) ->
            mapTriple VTriple
                (patchToValue l)
                (patchToValue m)
                (patchToValue r)

        PTriple Nothing _ _ ->
            Err <| ErrorAtFirst CouldNotBuildValueFromPatch

        PTriple _ Nothing _ ->
            Err <| ErrorAtSecond CouldNotBuildValueFromPatch

        PTriple _ _ Nothing ->
            Err <| ErrorAtThird CouldNotBuildValueFromPatch

        PList list ->
            let
                len : Int
                len =
                    (Dict.keys list.added ++ Dict.keys list.edited)
                        |> List.maximum
                        |> Maybe.withDefault 0
            in
            List.range 0 (len - 1)
                |> Result.Extra.combineMap
                    (\index ->
                        Result.mapError (ErrorAtIndex index) <|
                            case Dict.get index list.edited of
                                Just value ->
                                    patchToValue value

                                Nothing ->
                                    case Dict.get index list.added of
                                        Just value ->
                                            Ok value

                                        Nothing ->
                                            Err CouldNotBuildValueFromPatch
                    )
                |> Result.map (\items -> VList { cursor = 0, items = items })

        PSet set ->
            Ok <| VSet { cursor = 0, items = ValueDict.keys set.added }

        PDict dict ->
            Ok <| VDict { cursor = 0, items = ValueDict.toList dict.added }

        PCustomSame name maybeArgs ->
            mapCustom (VCustom name)
                (\arg ->
                    case arg of
                        Nothing ->
                            Err CouldNotBuildValueFromPatch

                        Just a ->
                            patchToValue a
                )
                maybeArgs

        PCustomChange _ name args ->
            Ok <| VCustom name args

        PRecord fields ->
            mapRecord VRecord (\_ -> patchToValue) fields

        PSetCursor _ ->
            Err CouldNotBuildValueFromPatch


mapTuple : (a -> b -> c) -> Result Error a -> Result Error b -> Result Error c
mapTuple outer l r =
    Result.map2 outer
        (Result.mapError ErrorAtFirst l)
        (Result.mapError ErrorAtSecond r)


mapTriple : (a -> b -> c -> d) -> Result Error a -> Result Error b -> Result Error c -> Result Error d
mapTriple outer l m r =
    Result.map3 outer
        (Result.mapError ErrorAtFirst l)
        (Result.mapError ErrorAtSecond m)
        (Result.mapError ErrorAtThird r)


mapRecord :
    (Dict String value -> b)
    -> (String -> v -> Result Error value)
    -> Dict String v
    -> Result Error b
mapRecord outer inner fields =
    fields
        |> Dict.toList
        |> foldlResult
            (\( fieldName, fieldValue ) acc ->
                case inner fieldName fieldValue of
                    Ok newValue ->
                        Ok <| Dict.insert fieldName newValue acc

                    Err e ->
                        Err (ErrorAtField fieldName e)
            )
            Dict.empty
        |> Result.map outer


mapCustom :
    (List value -> a)
    -> (arg -> Result Error value)
    -> List arg
    -> Result Error a
mapCustom outer inner ls =
    List.indexedMap Tuple.pair ls
        |> Result.Extra.combineMap
            (\( i, item ) ->
                Result.mapError (ErrorAtCustomArgument i) (inner item)
            )
        |> Result.map outer


mapCustom2 :
    (List value -> a)
    -> (larg -> rarg -> Result Error value)
    -> List larg
    -> List rarg
    -> Result Error a
mapCustom2 outer inner ls rs =
    mapCustom outer (\( l, r ) -> inner l r) (List.map2 Tuple.pair ls rs)


{-| -}
patch : { force : Bool } -> Patch -> Value -> Result Error Value
patch ({ force } as options) p value =
    let
        checkOrConflict : b -> b -> Value -> Result Error Value
        checkOrConflict expected found built =
            if force || expected == found then
                Ok built

            else
                Err Conflict
    in
    -- TODO: check, when removing from dict/list that we are removing what we expect
    case ( p, value ) of
        ( PInt expected i, VInt found ) ->
            checkOrConflict expected found <| VInt i

        ( PInt _ _, _ ) ->
            Err (WrongType "Patch.patch")

        ( PFloat expected f, VFloat found ) ->
            checkOrConflict expected found <| VFloat f

        ( PFloat _ _, _ ) ->
            Err (WrongType "Patch.patch")

        ( PString expected s, VString found ) ->
            checkOrConflict expected found <| VString s

        ( PString _ s, VPartialString _ ) ->
            Ok (VString s)

        ( PString _ _, _ ) ->
            Err (WrongType "Patch.patch")

        ( PChar expected c, VChar found ) ->
            checkOrConflict expected found <| VChar c

        ( PChar _ _, _ ) ->
            Err (WrongType "Patch.patch")

        ( PBool expected b, VBool found ) ->
            checkOrConflict expected found <| VBool b

        ( PBool _ _, _ ) ->
            Err (WrongType "Patch.patch")

        ( PUnit, VUnit ) ->
            Ok VUnit

        ( PUnit, _ ) ->
            Err (WrongType "Patch.patch")

        ( PBytes expected new, VBytes found ) ->
            checkOrConflict expected found <| VBytes new

        ( PBytes _ _, _ ) ->
            Err (WrongType "Patch.patch")

        ( PTuple lpatch rpatch, VTuple lvalue rvalue ) ->
            mapTuple VTuple
                (maybePatchValue options lpatch lvalue)
                (maybePatchValue options rpatch rvalue)

        ( PTuple _ _, _ ) ->
            Err (WrongType "Patch.patch")

        ( PTriple lpatch mpatch rpatch, VTriple lvalue mvalue rvalue ) ->
            mapTriple VTriple
                (maybePatchValue options lpatch lvalue)
                (maybePatchValue options mpatch mvalue)
                (maybePatchValue options rpatch rvalue)

        ( PTriple _ _ _, _ ) ->
            Err (WrongType "Patch.patch")

        ( PList lpatch, VList lvalue ) ->
            Dict.merge
                (\key item acc ->
                    Result.map (Dict.insert key item) acc
                )
                (\key item itemPatch acc ->
                    Result.map2
                        (Dict.insert key)
                        (Result.mapError (ErrorAtIndex key) <| patch options itemPatch item)
                        acc
                )
                (\_ _ acc -> acc)
                (Dict.diff
                    (lvalue.items
                        |> List.indexedMap Tuple.pair
                        |> Dict.fromList
                    )
                    lpatch.removed
                    |> Dict.union lpatch.added
                )
                lpatch.edited
                (Ok Dict.empty)
                |> Result.map
                    (\items ->
                        VList
                            { lvalue
                                | items = Dict.values items
                            }
                    )

        ( PList _, _ ) ->
            Err (WrongType "Patch.patch")

        ( PSet setPatch, VSet set ) ->
            VSet
                { set
                    | items =
                        ValueDict.diff
                            (ValueDict.fromList
                                (List.map (\item -> ( item, () )) set.items)
                            )
                            setPatch.removed
                            |> ValueDict.union setPatch.added
                            |> ValueDict.keys
                }
                |> Ok

        ( PSet _, _ ) ->
            Err (WrongType "Patch.patch")

        ( PDict dpatch, VDict dict ) ->
            ValueDict.merge
                (\key oldValue acc ->
                    Result.map (ValueDict.insert key oldValue) acc
                )
                (\key oldValue valuePatch acc ->
                    Result.map2
                        (ValueDict.insert key)
                        (Result.mapError (ErrorAtValueWithKey key) <| patch options valuePatch oldValue)
                        acc
                )
                (\key _ _ ->
                    -- We can't edit a value that doesn't exist
                    Err <| ErrorAtKey key <| WrongType "Patch.patch"
                )
                (ValueDict.diff (ValueDict.fromList dict.items) dpatch.removed)
                dpatch.edited
                (Ok ValueDict.empty)
                |> Result.map
                    (\e ->
                        VDict
                            { dict
                                | items =
                                    ValueDict.union dpatch.added e
                                        |> ValueDict.toList
                            }
                    )

        ( PDict _, _ ) ->
            Err (WrongType "Patch.patch")

        ( PCustomSame expectedName patchArgs, VCustom oldName args ) ->
            if force || oldName == expectedName then
                mapCustom2 (VCustom oldName)
                    (maybePatchValue options)
                    patchArgs
                    args

            else
                Err Conflict

        ( PCustomSame _ _, _ ) ->
            Err (WrongType "Patch.patch")

        ( PCustomChange expectedName newName patchArgs, VCustom oldName _ ) ->
            checkOrConflict oldName expectedName <| VCustom newName patchArgs

        ( PCustomChange _ _ _, _ ) ->
            Err (WrongType "Patch.patch")

        ( PRecord changed, VRecord fields ) ->
            changed
                |> Dict.toList
                |> foldlResult
                    (\( key, patchValue ) acc ->
                        (case Dict.get key acc of
                            Nothing ->
                                Err (WrongType "Patch.patch")

                            Just fieldValue ->
                                patch options patchValue fieldValue
                                    |> Result.map (\newValue -> Dict.insert key newValue acc)
                        )
                            |> Result.mapError (ErrorAtField key)
                    )
                    fields
                |> Result.map VRecord

        ( PRecord _, _ ) ->
            Err (WrongType "Patch.patch")

        ( PSetCursor cursor, VList list ) ->
            Ok (VList { list | cursor = cursor })

        ( PSetCursor cursor, VSet set ) ->
            Ok (VSet { set | cursor = cursor })

        ( PSetCursor cursor, VDict dict ) ->
            Ok (VDict { dict | cursor = cursor })

        ( PSetCursor _, _ ) ->
            Err (WrongType "Patch.patch")


{-| List.foldl combined with `Result err.map` and early exit.
-}
foldlResult :
    (item -> acc -> Result err acc)
    -> acc
    -> List item
    -> Result err acc
foldlResult func acc list =
    case list of
        [] ->
            Ok acc

        x :: xs ->
            case func x acc of
                Ok y ->
                    foldlResult func y xs

                (Err _) as e ->
                    e


{-| -}
patcher_Int : Patcher Int
patcher_Int =
    { patch = patch_Int
    , build = build_Int
    , toValue = VInt
    }


{-| -}
patch_Int : { force : Bool } -> Patch -> Int -> Result Error Int
patch_Int { force } p found =
    case p of
        PInt expected v ->
            if force || found == expected then
                Ok v

            else
                Err Conflict

        _ ->
            Err (WrongType "Patch.patch_Int")


{-| -}
build_Int : Value -> Result Error Int
build_Int p =
    case p of
        VInt s ->
            Ok s

        _ ->
            Err (WrongType "Patch.")


{-| -}
patcher_Float : Patcher Float
patcher_Float =
    { patch = patch_Float
    , build = build_Float
    , toValue = VFloat
    }


{-| -}
patch_Float : { force : Bool } -> Patch -> Float -> Result Error Float
patch_Float { force } p found =
    case p of
        PFloat expected v ->
            if force || found == expected then
                Ok v

            else
                Err Conflict

        _ ->
            Err (WrongType "Patch.patch_Float")


{-| -}
build_Float : Value -> Result Error Float
build_Float p =
    case p of
        VFloat s ->
            Ok s

        _ ->
            Err (WrongType "Patch.build_Float")


{-| -}
patcher_String : Patcher String
patcher_String =
    { patch = patch_String
    , build = build_String
    , toValue = VString
    }


{-| -}
patch_String : { force : Bool } -> Patch -> String -> Result Error String
patch_String { force } p found =
    case p of
        PString expected v ->
            if force || found == expected then
                Ok v

            else
                Err Conflict

        _ ->
            Err (WrongType "Patch.patch_String")


{-| -}
build_String : Value -> Result Error String
build_String p =
    case p of
        VString s ->
            Ok s

        _ ->
            Err (WrongType "Patch.build_String")


{-| -}
patcher_Json : Patcher Json.Encode.Value
patcher_Json =
    { patch = patch_Json
    , build = build_Json
    , toValue = \v -> VString (Json.Encode.encode 0 v)
    }


{-| -}
patch_Json : { force : Bool } -> Patch -> Json.Encode.Value -> Result Error Json.Encode.Value
patch_Json { force } p found =
    case p of
        PString expected v ->
            if force || Json.Encode.encode 0 found == expected then
                case Json.Decode.decodeString Json.Decode.value v of
                    Ok decoded ->
                        Ok decoded

                    Err _ ->
                        Err (WrongType "Patch.patch_Json Inner")

            else
                Err Conflict

        _ ->
            Err (WrongType "Patch.patch_Json")


{-| -}
build_Json : Value -> Result Error Json.Encode.Value
build_Json p =
    case p of
        VString s ->
            case Json.Decode.decodeString Json.Decode.value s of
                Ok decoded ->
                    Ok decoded

                Err _ ->
                    Err (WrongType "Patch.build_Json Inner")

        _ ->
            Err (WrongType "Patch.build_Json")


{-| -}
patcher_Char : Patcher Char
patcher_Char =
    { patch = patch_Char
    , build = build_Char
    , toValue = VChar
    }


{-| -}
patch_Char : { force : Bool } -> Patch -> Char -> Result Error Char
patch_Char { force } p found =
    case p of
        PChar expected v ->
            if force || found == expected then
                Ok v

            else
                Err Conflict

        _ ->
            Err (WrongType "Patch.patch_Char")


{-| Build a `Char` from a `Value`.
-}
build_Char : Value -> Result Error Char
build_Char p =
    case p of
        VChar s ->
            Ok s

        _ ->
            Err (WrongType "Patch.build_Char")


{-| Patcher for the `Bool` type.
-}
patcher_Bool : Patcher Bool
patcher_Bool =
    { patch = patch_Bool
    , build = build_Bool
    , toValue = VBool
    }


{-| Patch a `Bool` value.
-}
patch_Bool : { force : Bool } -> Patch -> Bool -> Result Error Bool
patch_Bool { force } p found =
    case p of
        PBool expected v ->
            if force || found == expected then
                Ok v

            else
                Err Conflict

        _ ->
            Err (WrongType "Patch.patch_Bool")


{-| Build a `Bool` from a `Value`.
-}
build_Bool : Value -> Result Error Bool
build_Bool p =
    case p of
        VBool s ->
            Ok s

        _ ->
            Err (WrongType "Patch.build_Bool")


{-| Patcher for the `()` type.
-}
patcher_Unit : Patcher ()
patcher_Unit =
    { patch = patch_Unit
    , build = build_Unit
    , toValue = \_ -> VUnit
    }


{-| -}
patch_Unit : { force : Bool } -> Patch -> () -> Result Error ()
patch_Unit _ p _ =
    case p of
        PUnit ->
            Ok ()

        _ ->
            Err (WrongType "Patch.patch_Unit")


{-| -}
build_Unit : Value -> Result Error ()
build_Unit v =
    case v of
        VUnit ->
            Ok ()

        _ ->
            Err (WrongType "Patch.build_Unit")


{-| Patcher for the `Bytes` type.
-}
patcher_Bytes : Patcher Bytes
patcher_Bytes =
    { patch = patch_Bytes
    , build = build_Bytes
    , toValue = VBytes
    }


{-| -}
patch_Bytes : { force : Bool } -> Patch -> Bytes -> Result Error Bytes
patch_Bytes _ p _ =
    case p of
        PBytes _ n ->
            -- TODO: Check old
            Ok n

        _ ->
            Err (WrongType "Patch.patch_Bytes")


{-| -}
build_Bytes : Value -> Result Error Bytes
build_Bytes v =
    case v of
        VBytes b ->
            Ok b

        _ ->
            Err (WrongType "Patch.build_Bytes")


{-| -}
patch_Record :
    (String -> Patch -> a -> Result Error a)
    -> Patch
    -> a
    -> Result Error a
patch_Record patcher p value =
    case p of
        PRecord fields ->
            fields
                |> Dict.toList
                |> foldlResult
                    (\( key, val ) acc ->
                        Result.mapError (ErrorAtField key) <|
                            patcher key val acc
                    )
                    value

        _ ->
            Err (WrongType "Patch.patch_Record")


{-| -}
build_Record : ((String -> Result Error Value) -> Result Error a) -> Value -> Result Error a
build_Record patcher p =
    case p of
        VRecord fields ->
            patcher
                (\fieldName ->
                    case Dict.get fieldName fields of
                        Nothing ->
                            Err (MissingField fieldName)

                        Just fieldValue ->
                            Ok fieldValue
                )

        _ ->
            Err (WrongType "Patch.build_Record")


{-| Patcher for tuples.
-}
patcher_Tuple :
    Patcher a
    -> Patcher b
    -> Patcher ( a, b )
patcher_Tuple lpatcher rpatcher =
    { patch = patch_Tuple lpatcher rpatcher
    , build = build_Tuple lpatcher rpatcher
    , toValue = toValue_Tuple lpatcher rpatcher
    }


{-| -}
patch_Tuple :
    Patcher a
    -> Patcher b
    -> { force : Bool }
    -> Patch
    -> ( a, b )
    -> Result Error ( a, b )
patch_Tuple lpatcher rpatcher options p ( lvalue, rvalue ) =
    case p of
        PTuple lpatch rpatch ->
            mapTuple Tuple.pair
                (maybeApply lpatcher options lpatch lvalue)
                (maybeApply rpatcher options rpatch rvalue)

        _ ->
            Err (WrongType "Patch.patch_Tuple")


{-| -}
build_Tuple : Patcher a -> Patcher b -> Value -> Result Error ( a, b )
build_Tuple lpatcher rpatcher p =
    case p of
        VTuple l r ->
            mapTuple Tuple.pair
                (lpatcher.build l)
                (rpatcher.build r)

        _ ->
            Err (WrongType "Patch.build_Tuple")


{-| -}
toValue_Tuple : Patcher a -> Patcher b -> ( a, b ) -> Value
toValue_Tuple lpatcher rpatcher ( l, r ) =
    VTuple (lpatcher.toValue l) (rpatcher.toValue r)


{-| Patcher for tuples with three elements.
-}
patcher_Triple :
    Patcher a
    -> Patcher b
    -> Patcher c
    -> Patcher ( a, b, c )
patcher_Triple lpatcher mpatcher rpatcher =
    { patch = patch_Triple lpatcher mpatcher rpatcher
    , build = build_Triple lpatcher mpatcher rpatcher
    , toValue = toValue_Triple lpatcher mpatcher rpatcher
    }


{-| -}
patch_Triple :
    Patcher a
    -> Patcher b
    -> Patcher c
    -> { force : Bool }
    -> Patch
    -> ( a, b, c )
    -> Result Error ( a, b, c )
patch_Triple lpatcher mpatcher rpatcher options p ( lvalue, mvalue, rvalue ) =
    case p of
        PTriple lpatch mpatch rpatch ->
            mapTriple (\l m r -> ( l, m, r ))
                (maybeApply lpatcher options lpatch lvalue)
                (maybeApply mpatcher options mpatch mvalue)
                (maybeApply rpatcher options rpatch rvalue)

        _ ->
            Err (WrongType "Patch.patch_Triple")


{-| -}
build_Triple : Patcher a -> Patcher b -> Patcher c -> Value -> Result Error ( a, b, c )
build_Triple lpatcher mpatcher rpatcher p =
    case p of
        VTriple lpatch mpatch rpatch ->
            mapTriple (\l m r -> ( l, m, r ))
                (lpatcher.build lpatch)
                (mpatcher.build mpatch)
                (rpatcher.build rpatch)

        _ ->
            Err (WrongType "Patch.build_Triple")


{-| -}
toValue_Triple : Patcher a -> Patcher b -> Patcher c -> ( a, b, c ) -> Value
toValue_Triple lpatcher mpatcher rpatcher ( l, m, r ) =
    VTriple (lpatcher.toValue l) (mpatcher.toValue m) (rpatcher.toValue r)


{-| Patcher for lists.
-}
patcher_List : Patcher item -> Patcher (List item)
patcher_List patcher =
    { patch = patch_List patcher
    , build = build_List patcher
    , toValue = toValue_List patcher
    }


{-| -}
patch_List : Patcher item -> { force : Bool } -> Patch -> List item -> Result Error (List item)
patch_List patcher options p value =
    case p of
        PList lpatch ->
            let
                init : Dict Int item
                init =
                    value
                        |> List.indexedMap Tuple.pair
                        |> Dict.fromList
            in
            Result.map2
                (\added edited ->
                    Dict.union
                        (Dict.fromList added)
                        (Dict.diff edited lpatch.removed)
                        |> Dict.values
                )
                (lpatch.added
                    |> Dict.toList
                    |> Result.Extra.combineMap
                        (\( k, v ) ->
                            Result.map
                                (Tuple.pair k)
                                (Result.mapError (ErrorAtIndex k) <| patcher.build v)
                        )
                )
                (Dict.foldl
                    (\editKey editPatch ->
                        Result.andThen
                            (\acc ->
                                (case Dict.get editKey acc of
                                    Nothing ->
                                        Err (WrongType "Patch.patch_List")

                                    Just itemValue ->
                                        patcher.patch options editPatch itemValue
                                )
                                    |> Result.mapError (ErrorAtIndex editKey)
                                    |> Result.map
                                        (\newValue ->
                                            Dict.insert editKey newValue acc
                                        )
                            )
                    )
                    (Ok init)
                    lpatch.edited
                )

        _ ->
            Err (WrongType "Patch.patch_List")


{-| -}
build_List : Patcher item -> Value -> Result Error (List item)
build_List patcher p =
    case p of
        VList list ->
            List.range 0 (List.length list.items - 1)
                |> Result.Extra.combineMap
                    (\index ->
                        case List.Extra.getAt index list.items of
                            Nothing ->
                                Err (ErrorAtIndex index MissingValue)

                            Just v ->
                                Result.mapError (ErrorAtIndex index) <| patcher.build v
                    )

        _ ->
            Err (WrongType "Patch.build_List")


{-| -}
toValue_List : Patcher item -> List item -> Value
toValue_List patcher value =
    VList
        { cursor = 0
        , items = List.map (\v -> patcher.toValue v) value
        }


{-| -}
patcher_Set : Patcher comparable -> Patcher (Set comparable)
patcher_Set patcher =
    { patch = patch_Set patcher
    , build = build_Set patcher
    , toValue = toValue_Set patcher
    }


{-| -}
patch_Set : Patcher comparable -> { force : Bool } -> Patch -> Set comparable -> Result Error (Set comparable)
patch_Set patcher options p value =
    value
        |> Set.toList
        |> patch_List patcher options p
        |> Result.map Set.fromList


{-| -}
build_Set : Patcher comparable -> Value -> Result Error (Set comparable)
build_Set patcher p =
    case p of
        VSet lpatch ->
            lpatch.items
                |> Result.Extra.combineMap patcher.build
                |> Result.map Set.fromList

        _ ->
            Err (WrongType "Patch.build_Set")


{-| -}
toValue_Set : Patcher comparable -> Set comparable -> Value
toValue_Set patcher value =
    VSet
        { cursor = 0
        , items =
            Set.toList value
                |> List.map (\item -> patcher.toValue item)
        }


{-| -}
patcher_Array : Patcher item -> Patcher (Array item)
patcher_Array patcher =
    { patch = patch_Array patcher
    , build = build_Array patcher
    , toValue = toValue_Array patcher
    }


{-| -}
patch_Array : Patcher item -> { force : Bool } -> Patch -> Array item -> Result Error (Array item)
patch_Array patcher options p value =
    value
        |> Array.toList
        |> patch_List patcher options p
        |> Result.map Array.fromList


{-| -}
build_Array : Patcher item -> Value -> Result Error (Array item)
build_Array patcher p =
    build_List patcher p
        |> Result.map Array.fromList


{-| -}
toValue_Array : Patcher item -> Array item -> Value
toValue_Array patcher value =
    toValue_List patcher (Array.toList value)


{-| -}
patcher_Dict : Patcher comparable -> Patcher value -> Patcher (Dict comparable value)
patcher_Dict keyPatcher valuePatcher =
    { patch = patch_Dict keyPatcher valuePatcher
    , build = build_Dict keyPatcher valuePatcher
    , toValue = toValue_Dict keyPatcher valuePatcher
    }


{-| -}
patch_Dict :
    Patcher comparable
    -> Patcher value
    -> { force : Bool }
    -> Patch
    -> Dict comparable value
    -> Result Error (Dict comparable value)
patch_Dict keyPatcher valuePatcher options p value =
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
                            (\key -> Dict.remove key acc)
                            (keyPatcher.build removedKey)
                    )
                |> foldOn dpatch.added
                    (\addedKey addedValue acc ->
                        Result.map2 (\k v -> Dict.insert k v acc)
                            (keyPatcher.build addedKey)
                            (valuePatcher.build addedValue)
                    )
                |> foldOn dpatch.edited
                    (\changedKey valuePatch acc ->
                        keyPatcher.build changedKey
                            |> Result.andThen
                                (\key ->
                                    case Dict.get key acc of
                                        Nothing ->
                                            -- The value was removed by someone else
                                            Err Conflict

                                        Just item ->
                                            valuePatcher.patch options valuePatch item
                                                |> Result.map (\patched -> Dict.insert key patched acc)
                                )
                    )

        _ ->
            Err (WrongType "Patch.")


{-| -}
build_Dict :
    Patcher comparable
    -> Patcher value
    -> Value
    -> Result Error (Dict comparable value)
build_Dict keyPatcher valuePatcher p =
    case p of
        VDict fields ->
            List.foldl
                (\( addedKeyPatch, addedValuePatch ) acc ->
                    Result.map3 Dict.insert
                        (Result.mapError (ErrorAtKey addedKeyPatch) <| keyPatcher.build addedKeyPatch)
                        (Result.mapError (ErrorAtValueWithKey addedKeyPatch) <| valuePatcher.build addedValuePatch)
                        acc
                )
                (Ok Dict.empty)
                fields.items

        _ ->
            Err (WrongType "Patch.")


{-| -}
toValue_Dict :
    Patcher comparable
    -> Patcher value
    -> Dict comparable value
    -> Value
toValue_Dict keyPatcher valuePatcher dict =
    VDict
        { cursor = 0
        , items =
            Dict.toList dict
                |> List.map
                    (\( key, value ) ->
                        ( keyPatcher.toValue key
                        , valuePatcher.toValue value
                        )
                    )
        }


{-| -}
build_Custom : (String -> List Value -> Result Error a) -> Value -> Result Error a
build_Custom patcher p =
    case p of
        VCustom name params ->
            patcher name params

        _ ->
            Err (WrongType "Patch.")


{-| -}
patcher_Never : Patcher Never
patcher_Never =
    { patch = patch_Never
    , build = build_Never
    , toValue = toValue_Never
    }


{-| -}
patch_Never : { force : Bool } -> Patch -> Never -> Result Error Never
patch_Never _ _ found =
    Ok found


{-| -}
build_Never : Value -> Result Error Never
build_Never _ =
    Err <| WrongType "Never"


{-| -}
toValue_Never : Never -> Value
toValue_Never =
    never


{-| -}
patcher_Posix : Patcher Time.Posix
patcher_Posix =
    { patch = patch_Posix
    , build = build_Posix
    , toValue = toValue_Posix
    }


{-| -}
patch_Posix : { force : Bool } -> Patch -> Time.Posix -> Result Error Time.Posix
patch_Posix { force } p found =
    case p of
        PInt expected i ->
            if force || expected == Time.posixToMillis found then
                Ok <| Time.millisToPosix i

            else
                Err Conflict

        _ ->
            Err (WrongType "Patch.")


{-| -}
build_Posix : Value -> Result Error Time.Posix
build_Posix p =
    case p of
        VInt i ->
            Ok <| Time.millisToPosix i

        _ ->
            Err (WrongType "Patch.")


{-| -}
toValue_Posix : Time.Posix -> Value
toValue_Posix p =
    VInt <| Time.posixToMillis p


{-| -}
maybeApply : Patcher value -> { force : Bool } -> Maybe Patch -> value -> Result Error value
maybeApply patcher options maybePatch value =
    case maybePatch of
        Nothing ->
            Ok value

        Just p ->
            patcher.patch options p value


maybePatchValue : { force : Bool } -> Maybe Patch -> Value -> Result Error Value
maybePatchValue options maybePatch value =
    case maybePatch of
        Nothing ->
            Ok value

        Just p ->
            patch options p value


{-| Merge two patches. If either is `Nothing`, return the other, otherwise try to combine them, giving priority to the new patch.
-}
merge : Maybe Patch -> Maybe Patch -> Result Error (Maybe Patch)
merge old new =
    case mergeWithoutSimplify old new of
        Ok (Just p) ->
            Ok (simplify p)

        m ->
            m


mergeWithoutSimplify : Maybe Patch -> Maybe Patch -> Result Error (Maybe Patch)
mergeWithoutSimplify old new =
    case ( old, new ) of
        ( Just _, Nothing ) ->
            Ok old

        ( Just (PInt oldExpected oldValue), Just (PInt newExpected newValue) ) ->
            mergeBasic PInt oldExpected oldValue newExpected newValue

        ( Just (PInt _ _), _ ) ->
            Err (WrongType "Patch.")

        ( Just (PFloat oldExpected oldValue), Just (PFloat newExpected newValue) ) ->
            mergeBasic PFloat oldExpected oldValue newExpected newValue

        ( Just (PFloat _ _), _ ) ->
            Err (WrongType "Patch.")

        ( Just (PBool oldExpected oldValue), Just (PBool newExpected newValue) ) ->
            mergeBasic PBool oldExpected oldValue newExpected newValue

        ( Just (PBool _ _), _ ) ->
            Err (WrongType "Patch.")

        ( Just (PString oldExpected oldValue), Just (PString newExpected newValue) ) ->
            mergeBasic PString oldExpected oldValue newExpected newValue

        ( Just (PString _ _), _ ) ->
            Err (WrongType "Patch.")

        ( Just (PChar oldExpected oldValue), Just (PChar newExpected newValue) ) ->
            mergeBasic PChar oldExpected oldValue newExpected newValue

        ( Just (PChar _ _), _ ) ->
            Err (WrongType "Patch.")

        ( Just (PTuple oldl oldr), Just (PTuple newl newr) ) ->
            mapTuple (\lp rp -> Just <| PTuple lp rp)
                (mergeWithoutSimplify oldl newl)
                (mergeWithoutSimplify oldr newr)

        ( Just (PTuple _ _), _ ) ->
            Err (WrongType "Patch.")

        ( Just (PTriple oldl oldm oldr), Just (PTriple newl newm newr) ) ->
            mapTriple (\lp mp rp -> Just <| PTriple lp mp rp)
                (mergeWithoutSimplify oldl newl)
                (mergeWithoutSimplify oldm newm)
                (mergeWithoutSimplify oldr newr)

        ( Just (PTriple _ _ _), _ ) ->
            Err (WrongType "Patch.")

        ( Just (PList oldListPatch), Just (PList newListPatch) ) ->
            let
                maybeAddedFromOld : Result Error (Dict Int Value)
                maybeAddedFromOld =
                    Dict.merge
                        (\key value acc ->
                            Result.map (Dict.insert key value) acc
                        )
                        (\key oldAdded newPatch acc ->
                            Result.map2
                                (Dict.insert key)
                                (Result.mapError (ErrorAtIndex key) <| patch { force = False } newPatch oldAdded)
                                acc
                        )
                        (\_ _ acc -> acc)
                        (Dict.diff oldListPatch.added newListPatch.removed)
                        newListPatch.edited
                        (Ok Dict.empty)

                maybeEdited : Result Error (Dict Int Patch)
                maybeEdited =
                    Dict.merge
                        (\key value acc -> Result.map (Dict.insert key value) acc)
                        (\key oldPatch newPatch acc ->
                            Result.map2
                                (\merged iacc ->
                                    case merged of
                                        Nothing ->
                                            iacc

                                        Just m ->
                                            Dict.insert key m iacc
                                )
                                (Result.mapError (ErrorAtIndex key) <| mergeWithoutSimplify (Just oldPatch) (Just newPatch))
                                acc
                        )
                        (\key value acc -> Result.map (Dict.insert key value) acc)
                        oldListPatch.edited
                        (Dict.diff newListPatch.edited oldListPatch.added)
                        (Ok Dict.empty)
            in
            Result.map2
                (\addedFromOld edited ->
                    let
                        added : Dict Int Value
                        added =
                            Dict.union newListPatch.added addedFromOld

                        removed : Dict Int Value
                        removed =
                            Dict.union
                                newListPatch.removed
                                (Dict.diff oldListPatch.removed newListPatch.added)
                    in
                    { added = added
                    , removed = removed
                    , edited = edited
                    }
                        |> PList
                        |> Just
                )
                maybeAddedFromOld
                maybeEdited

        ( Just (PList _), _ ) ->
            Err (WrongType "Patch.")

        ( Just (PSet oldSet), Just (PSet newSet) ) ->
            let
                added : ValueDict ()
                added =
                    ValueDict.union
                        (ValueDict.diff newSet.added oldSet.removed)
                        (ValueDict.diff oldSet.added newSet.removed)

                removed : ValueDict ()
                removed =
                    ValueDict.union
                        newSet.removed
                        (ValueDict.diff oldSet.removed newSet.added)
            in
            Ok <| Just <| PSet { added = added, removed = removed }

        ( Just (PSet _), _ ) ->
            Err (WrongType "Patch.")

        ( Just (PDict oldDict), Just (PDict newDict) ) ->
            let
                maybeAddedFromOld : Result Error (ValueDict Value)
                maybeAddedFromOld =
                    ValueDict.merge
                        (\key value acc -> Result.map (ValueDict.insert key value) acc)
                        (\k oldAdded newPatch acc ->
                            Result.map2
                                (ValueDict.insert k)
                                (Result.mapError (ErrorAtValueWithKey k) <| patch { force = False } newPatch oldAdded)
                                acc
                        )
                        (\_ _ acc -> acc)
                        (ValueDict.diff oldDict.added newDict.removed)
                        newDict.edited
                        (Ok ValueDict.empty)

                maybeEdited : Result Error (ValueDict Patch)
                maybeEdited =
                    ValueDict.merge
                        (\key value acc -> Result.map (ValueDict.insert key value) acc)
                        (\k oldPatch newPatch acc ->
                            Result.map2
                                (\merged iacc ->
                                    case merged of
                                        Nothing ->
                                            iacc

                                        Just m ->
                                            ValueDict.insert k m iacc
                                )
                                (Result.mapError (ErrorAtValueWithKey k) <| mergeWithoutSimplify (Just oldPatch) (Just newPatch))
                                acc
                        )
                        (\key value acc -> Result.map (ValueDict.insert key value) acc)
                        oldDict.edited
                        (ValueDict.diff newDict.edited oldDict.added)
                        (Ok ValueDict.empty)
            in
            Result.map2
                (\addedFromOld edited ->
                    { added =
                        ValueDict.union newDict.added addedFromOld
                    , removed =
                        ValueDict.union
                            newDict.removed
                            (ValueDict.diff oldDict.removed newDict.added)
                    , edited = edited
                    }
                        |> PDict
                        |> Just
                )
                maybeAddedFromOld
                maybeEdited

        ( Just (PDict _), _ ) ->
            Err (WrongType "Patch.")

        ( Just (PCustomChange oldExpected oldName _), Just (PCustomChange newExpected newName newArgs) ) ->
            if oldName == newExpected then
                Ok <| Just <| PCustomChange oldExpected newName newArgs

            else
                Err Conflict

        ( Just (PCustomChange oldExpected oldName oldArgs), Just (PCustomSame newName newArgs) ) ->
            if oldName == newName then
                mapCustom2
                    (\merged -> Just <| PCustomChange oldExpected newName merged)
                    (maybePatchValue { force = False })
                    newArgs
                    oldArgs

            else
                Err Conflict

        ( Just (PCustomChange _ _ _), _ ) ->
            Err (WrongType "Patch.")

        ( Just (PCustomSame oldName oldArgs), Just (PCustomSame newName newArgs) ) ->
            if oldName == newName then
                mapCustom2
                    (\mergedArgs -> Just <| PCustomSame newName mergedArgs)
                    mergeWithoutSimplify
                    oldArgs
                    newArgs

            else
                Err Conflict

        ( Just (PCustomSame oldName _), Just (PCustomChange newExpected newName newArgs) ) ->
            if oldName == newExpected then
                Ok <| Just <| PCustomChange oldName newName newArgs

            else
                Err Conflict

        ( Just (PCustomSame _ _), _ ) ->
            Err (WrongType "Patch.")

        ( Just (PRecord oldDict), Just (PRecord newDict) ) ->
            Dict.merge
                (\key child acc -> Result.map (Dict.insert key child) acc)
                (\key oldChild newChild acc ->
                    Result.map2
                        (\mergedChild iacc ->
                            case mergedChild of
                                Nothing ->
                                    iacc

                                Just merged ->
                                    Dict.insert key merged iacc
                        )
                        (Result.mapError (ErrorAtField key) <| mergeWithoutSimplify (Just oldChild) (Just newChild))
                        acc
                )
                (\key child acc -> Result.map (Dict.insert key child) acc)
                oldDict
                newDict
                (Ok Dict.empty)
                |> Result.map (\merged -> Just <| PRecord merged)

        ( Just (PRecord _), _ ) ->
            Err (WrongType "Patch.")

        ( Just PUnit, Just PUnit ) ->
            Ok Nothing

        ( Just PUnit, _ ) ->
            Err (WrongType "Patch.")

        ( Just (PBytes oldBytes _), Just (PBytes _ newBytes) ) ->
            Ok (Just (PBytes oldBytes newBytes))

        ( Just (PBytes _ _), _ ) ->
            Err (WrongType "Patch.")

        ( Just (PSetCursor _), Just (PSetCursor _) ) ->
            Ok new

        ( Just (PSetCursor _), _ ) ->
            Err (WrongType "Patch.")

        ( Nothing, Just _ ) ->
            Ok new

        ( Nothing, Nothing ) ->
            Ok old


{-| -}
simplify : Patch -> Maybe Patch
simplify p =
    let
        basic : a -> a -> Maybe Patch
        basic old new =
            if old == new then
                Nothing

            else
                Just p
    in
    case p of
        PBool old new ->
            basic old new

        PInt old new ->
            basic old new

        PFloat old new ->
            basic old new

        PString old new ->
            basic old new

        PChar old new ->
            basic old new

        PUnit ->
            Nothing

        PBytes _ _ ->
            Just p

        PTuple lp rp ->
            case ( Maybe.andThen simplify lp, Maybe.andThen simplify rp ) of
                ( Nothing, Nothing ) ->
                    Nothing

                ( ls, rs ) ->
                    Just <| PTuple ls rs

        PTriple lp mp rp ->
            case
                ( Maybe.andThen simplify lp
                , Maybe.andThen simplify mp
                , Maybe.andThen simplify rp
                )
            of
                ( Nothing, Nothing, Nothing ) ->
                    Nothing

                ( ls, ms, rs ) ->
                    Just <| PTriple ls ms rs

        PList { added, edited, removed } ->
            let
                newEdited : Dict Int Patch
                newEdited =
                    simplifyDict edited
            in
            if Dict.isEmpty added && Dict.isEmpty removed && Dict.isEmpty newEdited then
                Nothing

            else
                Just <|
                    PList
                        { added = added
                        , edited = newEdited
                        , removed = removed
                        }

        PSet { added, removed } ->
            if ValueDict.isEmpty added && ValueDict.isEmpty removed then
                Nothing

            else
                Just p

        PDict { added, edited, removed } ->
            let
                newEdited : ValueDict Patch
                newEdited =
                    simplifyValueDict edited
            in
            if ValueDict.isEmpty added && ValueDict.isEmpty removed && ValueDict.isEmpty newEdited then
                Nothing

            else
                Just <|
                    PDict
                        { added = added
                        , edited = newEdited
                        , removed = removed
                        }

        PCustomSame name patches ->
            let
                newPatches : List (Maybe Patch)
                newPatches =
                    List.map (Maybe.andThen simplify) patches
            in
            if List.all ((==) Nothing) newPatches then
                Nothing

            else
                Just <| PCustomSame name newPatches

        PRecord fields ->
            let
                newFields : Dict String Patch
                newFields =
                    simplifyDict fields
            in
            if Dict.isEmpty newFields then
                Nothing

            else
                Just <| PRecord newFields

        PCustomChange _ _ _ ->
            Just p

        PSetCursor _ ->
            Just p


simplifyValueDict : ValueDict Patch -> ValueDict Patch
simplifyValueDict fields =
    fields
        |> ValueDict.toList
        |> List.filterMap
            (\( fieldName, fieldPatch ) ->
                Maybe.map (Tuple.pair fieldName) (simplify fieldPatch)
            )
        |> ValueDict.fromList


simplifyDict : Dict comparable Patch -> Dict comparable Patch
simplifyDict fields =
    fields
        |> Dict.toList
        |> List.filterMap
            (\( fieldName, fieldPatch ) ->
                Maybe.map (Tuple.pair fieldName) (simplify fieldPatch)
            )
        |> Dict.fromList


mergeBasic :
    (value -> value -> Patch)
    -> value
    -> value
    -> value
    -> value
    -> Result Error (Maybe Patch)
mergeBasic ctor oldExpected oldValue newExpected newValue =
    if newExpected /= oldValue then
        Err Conflict

    else if newValue == oldExpected then
        Ok Nothing

    else
        Ok (Just <| ctor oldExpected newValue)


{-| Map an existing patcher. This is especially useful for containers.
-}
map : (a -> b) -> (b -> a) -> Patcher a -> Patcher b
map to from patcher =
    { patch =
        \config p value ->
            value
                |> from
                |> patcher.patch config p
                |> Result.map to
    , build =
        \value ->
            patcher.build value
                |> Result.map to
    , toValue =
        \value ->
            value
                |> from
                |> patcher.toValue
    }
