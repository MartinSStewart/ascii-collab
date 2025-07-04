module PatchTests exposing (suite)

import Dict
import Expect
import Fusion exposing (SpecialType(..), Type(..), Value(..))
import Fusion.Patch as Patch exposing (Patch(..))
import Fusion.ValueDict as ValueDict exposing (ValueDict)
import Fuzz exposing (Fuzzer)
import Result.Extra
import Test exposing (Test, describe, fuzz)
import Utils


suite : Test
suite =
    describe "Testing the `Patch` module"
        [ mergeCorrectness, idempotency ]


idempotency : Test
idempotency =
    let
        onePatchFuzzer : Fuzzer ( Value, Patch, Value )
        onePatchFuzzer =
            Utils.typeAndValueFuzzer 2
                |> Fuzz.andThen
                    (\( type_, value ) ->
                        patchedFuzzer value type_
                            |> Fuzz.map (\( patch, result ) -> ( value, patch, result ))
                    )
    in
    describe "Idempotency"
        [ fuzz onePatchFuzzer
            "Applying a patch with or without force should produce the same result"
          <|
            \( value, patch, result ) ->
                case Patch.patch { force = True } patch value of
                    Err _ ->
                        Expect.fail "Could not patch"

                    Ok actual ->
                        actual
                            |> expectEqualValue result
        , fuzz onePatchFuzzer
            "Applying a patch twice (forcing the second time) should be idempotent"
          <|
            \( _, patch, result ) ->
                case Patch.patch { force = True } patch result of
                    Err _ ->
                        Expect.fail "Could not patch"

                    Ok actual ->
                        actual
                            |> expectEqualValue result
        , fuzz onePatchFuzzer "Patch.simplify is idempotent" <|
            \( _, patch, _ ) ->
                patch
                    |> Patch.simplify
                    |> Maybe.andThen Patch.simplify
                    |> Expect.equal (Patch.simplify patch)
        , fuzz onePatchFuzzer "Patch.simplify should not change the result of applying the patch" <|
            \( value, patch, result ) ->
                case Patch.simplify patch of
                    Nothing ->
                        result |> Expect.equal value

                    Just simplified ->
                        Patch.patch { force = False } simplified value
                            |> Expect.equal (Ok result)
        ]


mergeCorrectness : Test
mergeCorrectness =
    describe "Testing the `merge` function"
        [ fuzz mergeFuzzer "[fuzz] Applying the result of `merge` is the same as applying both patches sequentially" <|
            \({ value, firstPatch, secondPatch, fullyPatched } as mergeRecord) ->
                let
                    maybeMerged : Result Patch.Error (Maybe Patch)
                    maybeMerged =
                        Patch.merge (Just firstPatch) (Just secondPatch)
                in
                case maybeMerged of
                    Err e ->
                        Expect.fail <| "Cannot merge: " ++ Debug.toString e ++ "\n" ++ toMessage mergeRecord Nothing

                    Ok Nothing ->
                        value
                            |> expectEqualValue fullyPatched

                    Ok (Just merged) ->
                        case Patch.patch { force = False } merged value of
                            Err e ->
                                Expect.fail <| "Cannot patch: " ++ Debug.toString e ++ "\n" ++ toMessage mergeRecord (Just merged)

                            Ok patched ->
                                patched |> expectEqualValue fullyPatched
        , fuzz mergeFuzzer "[fuzz] If the fully patched value is the same as the original then `merge` should be Nothing " <|
            \({ type_, value, firstPatch, secondPatch, fullyPatched } as mergeRecord) ->
                case ( type_, isEqualValue value fullyPatched ) of
                    ( TNamed _ _ _ (Just (TSet _)), Ok True ) ->
                        Expect.pass

                    ( TNamed _ _ _ (Just (TDict _ _)), Ok True ) ->
                        Expect.pass

                    ( _, Ok True ) ->
                        let
                            maybeMerged : Result Patch.Error (Maybe Patch)
                            maybeMerged =
                                Patch.merge (Just firstPatch) (Just secondPatch)

                            message : String
                            message =
                                toMessage mergeRecord (Just maybeMerged)
                        in
                        maybeMerged
                            |> Expect.equal (Ok Nothing)
                            |> Expect.onFail message

                    ( _, Ok False ) ->
                        Expect.pass

                    ( _, Err e ) ->
                        Expect.fail e
        ]


toMessage : { a | value : Value, firstPatch : Patch, secondPatch : Patch, fullyPatched : Value, halfPatched : Value } -> Maybe b -> String
toMessage { value, firstPatch, halfPatched, secondPatch, fullyPatched } maybeMerged =
    [ ( "value", Just <| Debug.toString value )
    , ( "firstPatch", Just <| Debug.toString firstPatch )
    , ( "halfPatched", Just <| Debug.toString halfPatched )
    , ( "secondPatch", Just <| Debug.toString secondPatch )
    , ( "fullyPatched", Just <| Debug.toString fullyPatched )
    , ( "maybeMerged", Maybe.map Debug.toString maybeMerged )
    ]
        |> List.filterMap
            (\( key, s ) ->
                Maybe.map
                    (\debugString ->
                        String.padRight 14 ' ' (key ++ ":")
                            ++ debugString
                    )
                    s
            )
        |> String.join "\n"


expectEqualValue : Value -> Value -> Expect.Expectation
expectEqualValue l r =
    case isEqualValue l r of
        Ok True ->
            Expect.pass

        _ ->
            Expect.equal l r


isEqualValue : Value -> Value -> Result String Bool
isEqualValue l r =
    if l == r then
        Ok True

    else
        case ( l, r ) of
            ( VString _, _ ) ->
                Ok False

            ( VBool _, _ ) ->
                Ok False

            ( VInt _, _ ) ->
                Ok False

            ( VChar _, _ ) ->
                Ok False

            ( VFloat fl, VFloat fr ) ->
                Ok <|
                    if isNaN fl then
                        isNaN fr

                    else
                        False

            ( VTuple ll lr, VTuple rl rr ) ->
                equalsValueList [ ll, lr ] [ rl, rr ]

            ( VTriple ll lm lr, VTriple rl rm rr ) ->
                equalsValueList [ ll, lm, lr ] [ rl, rm, rr ]

            ( VSet ls, VSet rs ) ->
                equalsValueList ls.items rs.items

            ( VRecord lf, VRecord rf ) ->
                equalsValueList (Dict.values lf) (Dict.values rf)

            ( VDict ld, VDict rd ) ->
                Result.map2 (&&)
                    (equalsValueList
                        (List.map Tuple.first ld.items)
                        (List.map Tuple.first rd.items)
                    )
                    (equalsValueList
                        (List.map Tuple.second ld.items)
                        (List.map Tuple.second rd.items)
                    )

            _ ->
                Err <| "Tried to compare incompatible types: " ++ Debug.toString l ++ " and " ++ Debug.toString r


equalsValueList : List Value -> List Value -> Result String Bool
equalsValueList lk rk =
    if List.isEmpty lk then
        Ok <| List.isEmpty rk

    else if List.length lk == List.length rk then
        List.map2 isEqualValue lk rk
            |> Result.Extra.combine
            |> Result.map (List.all identity)

    else
        Ok False


mergeFuzzer :
    Fuzzer
        { type_ : Type
        , value : Value
        , firstPatch : Patch
        , halfPatched : Value
        , secondPatch : Patch
        , fullyPatched : Value
        }
mergeFuzzer =
    Utils.typeAndValueFuzzer 2
        |> Fuzz.andThen
            (\( type_, value ) ->
                patchedFuzzer value type_
                    |> Fuzz.andThen
                        (\( firstPatch, halfPatched ) ->
                            patchedFuzzer halfPatched type_
                                |> Fuzz.map
                                    (\( secondPatch, fullyPatched ) ->
                                        { type_ = type_
                                        , value = value
                                        , firstPatch = firstPatch
                                        , halfPatched = halfPatched
                                        , secondPatch = secondPatch
                                        , fullyPatched = fullyPatched
                                        }
                                    )
                        )
            )


patchFuzzer : Value -> Type -> Fuzzer Patch
patchFuzzer value type_ =
    case ( value, type_ ) of
        ( VInt expected, _ ) ->
            Fuzz.oneOf
                [ Fuzz.int
                , Fuzz.oneOfValues [ 0, 1 ]
                ]
                |> Fuzz.map (PInt expected)

        ( VFloat expected, _ ) ->
            Fuzz.oneOf
                [ Fuzz.niceFloat
                , Fuzz.oneOfValues [ 0, 1 ]
                ]
                |> Fuzz.map (PFloat expected)

        ( VString expected, _ ) ->
            Fuzz.map (PString expected) Fuzz.string

        ( VBool expected, _ ) ->
            Fuzz.map (PBool expected) Fuzz.bool

        ( VChar expected, _ ) ->
            Fuzz.map (PChar expected) Fuzz.char

        ( VUnit, _ ) ->
            Fuzz.constant PUnit

        ( VBytes v, _ ) ->
            Fuzz.constant (PBytes v v)

        ( VTuple l r, TTuple lt rt ) ->
            Fuzz.map2 PTuple
                (Fuzz.maybe <| patchFuzzer l lt)
                (Fuzz.maybe <| patchFuzzer r rt)

        ( VTriple l m r, TTriple lt mt rt ) ->
            Fuzz.map3 PTriple
                (Fuzz.maybe <| patchFuzzer l lt)
                (Fuzz.maybe <| patchFuzzer m mt)
                (Fuzz.maybe <| patchFuzzer r rt)

        ( VList _, _ ) ->
            Fuzz.invalid "[patchFuzzer] branch 'VList _' not implemented"

        ( VSet { items }, TNamed _ _ _ (Just (TSet child)) ) ->
            Fuzz.map2
                (\added removed ->
                    let
                        removedDict : ValueDict ()
                        removedDict =
                            List.map2
                                (\item remove ->
                                    if remove then
                                        Nothing

                                    else
                                        Just ( item, () )
                                )
                                items
                                removed
                                |> List.filterMap identity
                                |> ValueDict.fromList

                        addedDict : ValueDict ()
                        addedDict =
                            ValueDict.diff
                                (ValueDict.fromList added)
                                removedDict
                    in
                    PSet
                        { added = addedDict
                        , removed = removedDict
                        }
                )
                (Fuzz.list
                    (Fuzz.map (\add -> ( add, () )) <|
                        if List.isEmpty items then
                            Utils.valueFuzzer child

                        else
                            Fuzz.oneOf
                                [ Utils.valueFuzzer child
                                , Fuzz.oneOfValues items
                                ]
                    )
                )
                (Fuzz.listOfLength
                    (List.length items)
                    Fuzz.bool
                )

        ( VDict { items }, TNamed _ _ _ (Just (TDict keyType valueType)) ) ->
            Fuzz.map2
                (\added removed ->
                    let
                        removedDict : ValueDict Value
                        removedDict =
                            List.map2
                                (\pair remove ->
                                    if remove then
                                        Nothing

                                    else
                                        Just pair
                                )
                                items
                                removed
                                |> List.filterMap identity
                                |> ValueDict.fromList

                        addedDict : ValueDict Value
                        addedDict =
                            ValueDict.diff
                                (ValueDict.fromList added)
                                removedDict
                    in
                    PDict
                        { added = addedDict
                        , edited =
                            let
                                _ =
                                    Debug.todo
                            in
                            ValueDict.empty
                        , removed = removedDict
                        }
                )
                (Fuzz.list
                    (Fuzz.pair
                        (if List.isEmpty items then
                            Utils.valueFuzzer keyType

                         else
                            Fuzz.oneOf
                                [ Utils.valueFuzzer keyType
                                , Fuzz.oneOfValues (List.map Tuple.first items)
                                ]
                        )
                        (if List.isEmpty items then
                            Utils.valueFuzzer valueType

                         else
                            Fuzz.oneOf
                                [ Utils.valueFuzzer valueType
                                , Fuzz.oneOfValues (List.map Tuple.second items)
                                ]
                        )
                    )
                )
                (Fuzz.listOfLength
                    (List.length items)
                    Fuzz.bool
                )

        ( VCustom _ _, TCustom _ _ _ ) ->
            Fuzz.invalid "[patchFuzzer] branch 'VCustom _ _' not implemented"

        ( VRecord fields, TRecord fieldTypes ) ->
            fieldTypes
                |> List.foldl
                    (\( fieldName, fieldType ) acc ->
                        case Dict.get fieldName fields of
                            Just fieldValue ->
                                Fuzz.andThen
                                    (\facc ->
                                        Fuzz.oneOf
                                            [ Fuzz.constant facc
                                            , Fuzz.map
                                                (\p -> Dict.insert fieldName p facc)
                                                (patchFuzzer fieldValue fieldType)
                                            ]
                                    )
                                    acc

                            Nothing ->
                                Fuzz.invalid <| "Could not find field " ++ fieldName ++ " in value VRecord " ++ Debug.toString (Dict.toList fields) ++ " of type " ++ Debug.toString type_
                    )
                    (Fuzz.constant Dict.empty)
                |> Fuzz.map PRecord

        ( VUnloaded, _ ) ->
            Fuzz.invalid "It doesn't make sense to patch an Unloaded value"

        _ ->
            Fuzz.invalid <| "Invalid type " ++ Debug.toString type_ ++ " for value " ++ Debug.toString value


patchedFuzzer : Value -> Type -> Fuzzer ( Patch, Value )
patchedFuzzer value type_ =
    patchFuzzer value type_
        |> Fuzz.andThen
            (\patch ->
                let
                    maybePatched : Result Patch.Error Value
                    maybePatched =
                        Patch.patch { force = False } patch value
                in
                case maybePatched of
                    Ok patched ->
                        Fuzz.constant ( patch, patched )

                    Err e ->
                        Fuzz.invalid <| "Failed to apply patch " ++ Debug.toString patch ++ " to value" ++ Debug.toString value ++ ": " ++ Debug.toString e
            )
