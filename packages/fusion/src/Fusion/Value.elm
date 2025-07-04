module Fusion.Value exposing (typeToDefault, compare, getType)

{-|

@docs typeToDefault, compare, getType

-}

import Bytes
import Bytes.Decode
import Bytes.Encode
import Dict
import Fusion exposing (SpecialType(..), Type(..), Value(..))
import List.Extra
import Maybe.Extra
import Result.Extra
import Set exposing (Set)


{-| Build a default/empty value of the given type.

This will build empty lists and strings, zero, False, space, and similarly for other types.

-}
typeToDefault :
    Dict.Dict (List String) (Dict.Dict String ( Type, List String ))
    -> Type
    -> Result String Value
typeToDefault typeDict t =
    let
        ok : a -> Result error (Maybe a)
        ok v =
            Ok (Just v)

        go : Set ( List String, String ) -> Type -> Result String (Maybe Value)
        go seen type_ =
            case type_ of
                TNamed _ _ _ (Just TInt) ->
                    ok <| VInt 0

                TNamed _ _ _ (Just TFloat) ->
                    ok <| VFloat 0

                TNamed _ _ _ (Just TString) ->
                    ok <| VString ""

                TNamed _ _ _ (Just TBool) ->
                    ok <| VBool False

                TNamed _ _ _ (Just TChar) ->
                    ok <| VChar ' '

                TNamed _ _ _ (Just TNever) ->
                    Err "Cannot build default for Never"

                TNamed _ _ _ (Just TBytes) ->
                    ok (VBytes (Bytes.Encode.encode (Bytes.Encode.sequence [])))

                TNamed _ _ _ (Just TJson) ->
                    ok (VString "{}")

                TNamed _ _ _ (Just (TList _)) ->
                    ok <| VList { cursor = 0, items = [] }

                TNamed _ _ _ (Just (TSet _)) ->
                    ok <| VSet { cursor = 0, items = [] }

                TNamed _ _ _ (Just (TDict _ _)) ->
                    ok <| VDict { cursor = 0, items = [] }

                -- TNamed _ _ _ (Just (TSeqDict _ _)) ->
                --     ok <| VDict { cursor = 0, items = [] }
                TNamed moduleName typeName args _ ->
                    if Set.member ( moduleName, typeName ) seen then
                        Ok Nothing

                    else
                        Result.andThen (go (Set.insert ( moduleName, typeName ) seen)) <|
                            getType typeDict moduleName typeName args

                TVar _ ->
                    Err "Cannot build default for a type variable"

                TUnit ->
                    ok VUnit

                TTuple l r ->
                    Result.map2 (Maybe.map2 VTuple)
                        (go seen l)
                        (go seen r)

                TTriple l m r ->
                    Result.map3 (Maybe.map3 VTriple)
                        (go seen l)
                        (go seen m)
                        (go seen r)

                TCustom name _ variants ->
                    case
                        variants
                            -- Heuristic: variants with less args are easier to build
                            |> List.sortBy (\( _, variantArgs ) -> List.length variantArgs)
                            |> List.Extra.findMap
                                (\( variantName, variantArgs ) ->
                                    case Result.Extra.combineMap (go seen) variantArgs of
                                        Err _ ->
                                            Nothing

                                        Ok argValues ->
                                            Maybe.map (VCustom variantName) (Maybe.Extra.combine argValues)
                                )
                    of
                        Nothing ->
                            Err <| "None of the variants of type " ++ name ++ " are safe"

                        Just r ->
                            ok r

                TRecord fields ->
                    fields
                        |> List.foldl
                            (\( fieldName, fieldType ) acc ->
                                case acc of
                                    Ok (Just iacc) ->
                                        case go seen fieldType of
                                            Err e ->
                                                Err e

                                            Ok Nothing ->
                                                Ok Nothing

                                            Ok (Just fieldValue) ->
                                                ok (Dict.insert fieldName fieldValue iacc)

                                    _ ->
                                        acc
                            )
                            (ok Dict.empty)
                        |> Result.map (Maybe.map VRecord)

                TGenericRecord _ _ ->
                    Err "Cannot build default for a generic record"
    in
    case go Set.empty t of
        Ok Nothing ->
            Err "Could not build - unexpected error"

        Ok (Just r) ->
            Ok r

        Err e ->
            Err e


{-| -}
getType :
    Dict.Dict (List String) (Dict.Dict String ( Type, List String ))
    -> List String
    -> String
    -> List Type
    -> Result String Type
getType typeDict moduleName typeName args =
    case Dict.get moduleName typeDict of
        Nothing ->
            Err <| "Module " ++ String.join "." moduleName ++ " not found"

        Just mod ->
            case Dict.get typeName mod of
                Nothing ->
                    Err <| "Type " ++ String.join "." (moduleName ++ [ typeName ]) ++ " not found"

                Just ( rawType, argNames ) ->
                    case replace (Dict.fromList (List.map2 Tuple.pair argNames args)) rawType of
                        Nothing ->
                            Err <|
                                "Wrong number of args passed to type "
                                    ++ String.join "." (moduleName ++ [ typeName ])

                        Just built ->
                            Ok built


replace : Dict.Dict String Type -> Type -> Maybe Type
replace dict base =
    case base of
        TVar v ->
            Dict.get v dict

        TNamed moduleName name args special ->
            Maybe.map2
                (TNamed moduleName name)
                (Maybe.Extra.combineMap (replace dict) args)
                (case special of
                    Nothing ->
                        Just Nothing

                    Just s ->
                        Maybe.map Just (replaceSpecial dict s)
                )

        TUnit ->
            Just TUnit

        TTuple l r ->
            Maybe.map2 TTuple
                (replace dict l)
                (replace dict r)

        TTriple l m r ->
            Maybe.map3 TTriple
                (replace dict l)
                (replace dict m)
                (replace dict r)

        TCustom name args variants ->
            variants
                |> Maybe.Extra.combineMap
                    (\( variantName, variantArgs ) ->
                        variantArgs
                            |> Maybe.Extra.combineMap (replace dict)
                            |> Maybe.map (Tuple.pair variantName)
                    )
                -- TODO: Check this
                |> Maybe.map (TCustom name args)

        TRecord fields ->
            fields
                |> Maybe.Extra.combineMap
                    (\( fieldName, fieldType ) ->
                        replace dict fieldType
                            |> Maybe.map (Tuple.pair fieldName)
                    )
                |> Maybe.map TRecord

        TGenericRecord var fields ->
            fields
                |> Maybe.Extra.combineMap
                    (\( fieldName, fieldType ) ->
                        replace dict fieldType
                            |> Maybe.map (Tuple.pair fieldName)
                    )
                |> Maybe.map (TGenericRecord var)


replaceSpecial : Dict.Dict String Type -> SpecialType -> Maybe SpecialType
replaceSpecial dict special =
    case special of
        TList c ->
            Maybe.map TList (replace dict c)

        TMaybe c ->
            Maybe.map TMaybe (replace dict c)

        TSet c ->
            Maybe.map TSet (replace dict c)

        TResult e o ->
            Maybe.map2 TResult (replace dict e) (replace dict o)

        TDict k v ->
            Maybe.map2 TDict (replace dict k) (replace dict v)

        TInt ->
            Just TInt

        TFloat ->
            Just TFloat

        TOrder ->
            Just TOrder

        TBool ->
            Just TBool

        TNever ->
            Just TNever

        TString ->
            Just TString

        TChar ->
            Just TChar

        TBytes ->
            Just TBytes

        TJson ->
            Just TJson


{-| Compare two values.
-}
compare : Value -> Value -> Order
compare l r =
    case ( l, r ) of
        ( VInt lv, VInt rv ) ->
            Basics.compare lv rv

        ( VInt _, _ ) ->
            LT

        ( _, VInt _ ) ->
            GT

        ( VFloat lv, VFloat rv ) ->
            Basics.compare lv rv

        ( VFloat _, _ ) ->
            LT

        ( _, VFloat _ ) ->
            GT

        ( VString lv, VString rv ) ->
            Basics.compare lv rv

        ( VString _, _ ) ->
            LT

        ( _, VString _ ) ->
            GT

        ( VBool lv, VBool rv ) ->
            if lv == rv then
                EQ

            else if lv then
                LT

            else
                GT

        ( VBool _, _ ) ->
            LT

        ( _, VBool _ ) ->
            GT

        ( VChar lv, VChar rv ) ->
            Basics.compare lv rv

        ( VChar _, _ ) ->
            LT

        ( _, VChar _ ) ->
            GT

        ( VUnit, VUnit ) ->
            EQ

        ( VUnit, _ ) ->
            LT

        ( _, VUnit ) ->
            GT

        ( VBytes lb, VBytes rb ) ->
            let
                lWidth : Int
                lWidth =
                    Bytes.width lb

                widthCompare : Order
                widthCompare =
                    Basics.compare lWidth (Bytes.width rb)

                toList : Bytes.Bytes -> List Int
                toList input =
                    Bytes.Decode.decode toListDecoder input
                        |> Maybe.withDefault []

                toListDecoder : Bytes.Decode.Decoder (List Int)
                toListDecoder =
                    Bytes.Decode.loop
                        ( lWidth, [] )
                        (\( left, acc ) ->
                            if left <= 0 then
                                -- No need to reverse, as we're not exposing the list
                                Bytes.Decode.succeed (Bytes.Decode.Done acc)

                            else if left >= 4 then
                                -- We can read 4 bytes for a slight efficiency boost
                                Bytes.Decode.map
                                    (\i -> Bytes.Decode.Loop ( left - 4, i :: acc ))
                                    (Bytes.Decode.unsignedInt32 Bytes.LE)

                            else
                                Bytes.Decode.map
                                    (\i -> Bytes.Decode.Loop ( left - 1, i :: acc ))
                                    Bytes.Decode.unsignedInt8
                        )
            in
            if widthCompare == EQ then
                Basics.compare (toList lb) (toList rb)

            else
                widthCompare

        ( VBytes _, _ ) ->
            LT

        ( _, VBytes _ ) ->
            GT

        ( VTuple ll lr, VTuple rl rr ) ->
            let
                lcomp : Order
                lcomp =
                    compare ll rl
            in
            if lcomp == EQ then
                compare lr rr

            else
                lcomp

        ( VTuple _ _, _ ) ->
            LT

        ( _, VTuple _ _ ) ->
            GT

        ( VTriple ll lm lr, VTriple rl rm rr ) ->
            let
                lcomp : Order
                lcomp =
                    compare ll rl
            in
            if lcomp == EQ then
                let
                    mcomp : Order
                    mcomp =
                        compare lm rm
                in
                if mcomp == EQ then
                    compare lr rr

                else
                    mcomp

            else
                lcomp

        ( VTriple _ _ _, _ ) ->
            LT

        ( _, VTriple _ _ _ ) ->
            GT

        ( VList ll, VList rl ) ->
            compareValueLists ll.items rl.items

        ( VList _, _ ) ->
            LT

        ( _, VList _ ) ->
            GT

        ( VSet ls, VSet rs ) ->
            compareValueLists ls.items rs.items

        ( VSet _, _ ) ->
            LT

        ( _, VSet _ ) ->
            GT

        ( VDict ls, VDict rs ) ->
            let
                inner : ( Value, Value ) -> ( Value, Value ) -> Order
                inner ( ll, lr ) ( rl, rr ) =
                    case compare ll rl of
                        EQ ->
                            compare lr rr

                        res ->
                            res
            in
            compareLists inner ls.items rs.items

        ( VDict _, _ ) ->
            LT

        ( _, VDict _ ) ->
            GT

        ( VCustom lname largs, VCustom rname rargs ) ->
            case Basics.compare lname rname of
                EQ ->
                    compareValueLists largs rargs

                res ->
                    res

        ( VCustom _ _, _ ) ->
            LT

        ( _, VCustom _ _ ) ->
            GT

        ( VRecord lrec, VRecord rrec ) ->
            compareValueLists (Dict.values lrec) (Dict.values rrec)

        ( VRecord _, _ ) ->
            LT

        ( _, VRecord _ ) ->
            GT

        ( VPartialString ls, VPartialString rs ) ->
            case Basics.compare ls.length rs.length of
                EQ ->
                    Basics.compare ls.partial rs.partial

                res ->
                    res

        ( VPartialString _, _ ) ->
            LT

        ( _, VPartialString _ ) ->
            GT

        ( VUnloaded, VUnloaded ) ->
            EQ


compareValueLists : List Value -> List Value -> Order
compareValueLists ll rl =
    compareLists compare ll rl


compareLists : (a -> a -> Order) -> List a -> List a -> Order
compareLists inner lq rq =
    case lq of
        [] ->
            case rq of
                [] ->
                    EQ

                _ ->
                    LT

        lh :: lt ->
            case rq of
                [] ->
                    GT

                rh :: rt ->
                    case inner lh rh of
                        EQ ->
                            compareLists inner lt rt

                        comp ->
                            comp
