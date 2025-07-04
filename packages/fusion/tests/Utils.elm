module Utils exposing (tBool, tBytes, tChar, tDict, tFloat, tInt, tJsonDecode, tJsonEncode, tList, tMaybe, tNever, tOrder, tResult, tSet, tString, typeAndValueFuzzer, valueFuzzer)

import Bytes.Encode
import Dict
import Fusion exposing (SpecialType(..), Type(..), Value(..))
import Fusion.ValueDict as ValueDict
import Fuzz exposing (Fuzzer)


typeFuzzer : Int -> Fuzzer Type
typeFuzzer budget =
    let
        leaves : List Type
        leaves =
            [ tInt
            , tFloat
            , tString
            , tBool
            , tChar
            , TUnit
            , tJsonEncode
            , tJsonDecode
            ]
    in
    if budget <= 0 then
        Fuzz.oneOfValues leaves

    else
        let
            next : Fuzzer Type
            next =
                typeFuzzer <| budget - 1

            -- variantsFuzzer : Fuzzer (List ( String, List Type ))
            -- variantsFuzzer =
            --     Fuzz.list <| Fuzz.pair Fuzz.string (Fuzz.list next)
            nodes : List (Fuzzer Type)
            nodes =
                [ Fuzz.map2 TTuple next next
                , Fuzz.map3 TTriple next next next

                -- , Fuzz.map TList next
                , Fuzz.map tSet next
                , Fuzz.map2 tDict next next

                -- , Fuzz.map3 TCustom Fuzz.string (Fuzz.constant []) variantsFuzzer
                , Fuzz.map
                    (TRecord << Dict.toList << Dict.fromList)
                    (Fuzz.list <| Fuzz.pair Fuzz.asciiString next)
                ]
        in
        Fuzz.oneOf <| List.map Fuzz.constant leaves ++ nodes


valueFuzzer : Type -> Fuzzer Value
valueFuzzer type_ =
    case type_ of
        TNamed _ _ _ (Just TInt) ->
            Fuzz.map VInt Fuzz.int

        TNamed _ _ _ (Just TFloat) ->
            Fuzz.map VFloat Fuzz.niceFloat

        TNamed _ _ _ (Just TString) ->
            Fuzz.map VString Fuzz.string

        TNamed _ _ _ (Just TBool) ->
            Fuzz.map VBool Fuzz.bool

        TNamed _ _ _ (Just TChar) ->
            Fuzz.map VChar Fuzz.char

        TNamed _ _ _ (Just TNever) ->
            Fuzz.invalid "Cannot build never"

        TNamed _ _ _ (Just TBytes) ->
            Fuzz.list (Fuzz.intRange 0 255)
                |> Fuzz.map
                    (\seq ->
                        seq
                            |> List.map Bytes.Encode.unsignedInt8
                            |> Bytes.Encode.sequence
                            |> Bytes.Encode.encode
                            |> VBytes
                    )

        TNamed _ _ _ (Just TJson) ->
            Fuzz.oneOfValues
                [ VString "{}"
                , VString "null"
                , VString "[]"
                , VString "0"
                , VString "[0, null, {}, \"a\"]"
                ]

        TNamed _ _ _ (Just (TList child)) ->
            Fuzz.map
                (\list ->
                    VList
                        { cursor = 0
                        , items = list
                        }
                )
                (Fuzz.list (valueFuzzer child))

        TNamed _ _ _ (Just (TSet child)) ->
            Fuzz.map
                (\list ->
                    VSet
                        { cursor = 0
                        , items =
                            list
                                |> List.map (\v -> ( v, () ))
                                |> ValueDict.fromList
                                |> ValueDict.keys
                        }
                )
                (Fuzz.list (valueFuzzer child))

        TNamed _ _ _ (Just (TDict key value)) ->
            Fuzz.map
                (\list ->
                    VDict
                        { cursor = 0
                        , items = list
                        }
                )
                (Fuzz.list (Fuzz.pair (valueFuzzer key) (valueFuzzer value)))

        TNamed _ _ _ _ ->
            Debug.todo "[valueFuzzer] branch 'TNamed _ _ _' not implemented"

        TUnit ->
            Fuzz.constant VUnit

        TTuple lt rt ->
            Fuzz.map2 VTuple (valueFuzzer lt) (valueFuzzer rt)

        TTriple lt mt rt ->
            Fuzz.map3 VTriple (valueFuzzer lt) (valueFuzzer mt) (valueFuzzer rt)

        TCustom _ _ _ ->
            Debug.todo "[valueFuzzer] branch 'TCustom _ _ _' not implemented"

        TRecord fields ->
            fields
                |> Fuzz.traverse
                    (\( fieldName, fieldType ) ->
                        Fuzz.map
                            (\fieldValue -> ( fieldName, fieldValue ))
                            (valueFuzzer fieldType)
                    )
                |> Fuzz.map (Dict.fromList >> VRecord)

        TVar _ ->
            Debug.todo "[valueFuzzer] branch 'TVar _' not implemented"

        TGenericRecord _ _ ->
            Debug.todo "[valueFuzzer] branch 'TGenericRecord _ _' not implemented"


typeAndValueFuzzer : Int -> Fuzzer ( Type, Value )
typeAndValueFuzzer budget =
    typeFuzzer budget
        |> Fuzz.andThen (\type_ -> Fuzz.map (Tuple.pair type_) (valueFuzzer type_))


tInt : Type
tInt =
    TNamed [ "Basics" ] "Int" [] (Just TInt)


tFloat : Type
tFloat =
    TNamed [ "Basics" ] "Float" [] (Just TFloat)


tOrder : Type
tOrder =
    TNamed [ "Basics" ] "Order" [] (Just TOrder)


tBool : Type
tBool =
    TNamed [ "Basics" ] "Bool" [] (Just TBool)


tNever : Type
tNever =
    TNamed [ "Basics" ] "Never" [] (Just TNever)


tList : Type -> Type
tList child =
    TNamed [ "List" ] "List" [ child ] (Just (TList child))


tMaybe : Type -> Type
tMaybe child =
    TNamed [ "Maybe" ] "Maybe" [ child ] (Just (TMaybe child))


tResult : Type -> Type -> Type
tResult e ok =
    TNamed [ "Result" ] "Result" [ e, ok ] (Just (TResult e ok))


tString : Type
tString =
    TNamed [ "String" ] "String" [] (Just TString)


tChar : Type
tChar =
    TNamed [ "Char" ] "Char" [] (Just TChar)


tSet : Type -> Type
tSet child =
    TNamed [ "Set" ] "Set" [ child ] (Just (TSet child))


tDict : Type -> Type -> Type
tDict key value =
    TNamed [ "Dict" ] "Dict" [ key, value ] (Just (TDict key value))


tBytes : Type
tBytes =
    TNamed [ "Bytes" ] "Bytes" [] (Just TBytes)


tJsonDecode : Type
tJsonDecode =
    TNamed [ "Json", "Decode" ] "Value" [] (Just TJson)


tJsonEncode : Type
tJsonEncode =
    TNamed [ "Json", "Encode" ] "Value" [] (Just TJson)
