module Fusion.View exposing (type_, value, viewPatch, viewPatchError)

{-|

@docs type_, value, viewPatch, viewPatchError

-}

import Dict exposing (Dict)
import Fusion exposing (SpecialType(..), Type(..), Value(..))
import Fusion.Colors
import Fusion.Internal
import Fusion.Patch as Patch exposing (Patch(..))
import Fusion.ValueDict as ValueDict
import Html exposing (Html)
import Html.Attributes
import HtmlExtra
import List.Extra


{-| View a value.
-}
value : Value -> Html msg
value input =
    case input of
        VInt i ->
            Fusion.Colors.valueText <| String.fromInt i

        VFloat i ->
            Fusion.Colors.valueText <| String.fromFloat i

        VString i ->
            Fusion.Colors.quoteText <| "\"" ++ i ++ "\""

        VPartialString i ->
            Fusion.Colors.quoteText <| "\"" ++ i.partial ++ "\" [" ++ String.fromInt i.length ++ "]"

        VChar i ->
            Fusion.Colors.quoteText <| "'" ++ String.fromChar i ++ "'"

        VUnit ->
            Fusion.Colors.orangeText "()"

        VBytes _ ->
            Fusion.Colors.orangeText "<bytes>"

        VBool b ->
            Html.text <| Fusion.Internal.boolToString b

        VList list ->
            let
                values : List (Html msg)
                values =
                    list.items
                        |> List.map value
                        |> List.intersperse (Fusion.Colors.purpleText ", ")
            in
            (Fusion.Colors.purpleText "[" :: values ++ [ Fusion.Colors.purpleText "]" ])
                |> HtmlExtra.row

        VSet set ->
            let
                values : List (Html msg)
                values =
                    set.items
                        |> List.map (\k -> ( k, () ))
                        |> ValueDict.fromList
                        |> ValueDict.keys
                        |> List.map value
                        |> List.intersperse (Fusion.Colors.purpleText ", ")
            in
            HtmlExtra.wrappedRow <|
                Html.text "Set.fromList "
                    :: Fusion.Colors.purpleText "["
                    :: values
                    ++ [ Fusion.Colors.purpleText "]" ]

        VDict values ->
            dict values

        VCustom cname params ->
            custom cname params

        VRecord fields ->
            record fields

        VUnloaded ->
            Fusion.Colors.errorText "Unloaded"

        VTuple l r ->
            tuple [ l, r ]

        VTriple l m r ->
            tuple [ l, m, r ]


dict :
    { a | items : List ( Value, Value ) }
    -> Html msg
dict { items } =
    if List.isEmpty items then
        HtmlExtra.row [ Html.text "Dict.fromList ", Fusion.Colors.purpleText "[]" ]

    else
        Html.table
            []
            (List.indexedMap
                (\index ( key, val ) ->
                    Html.tr
                        []
                        [ if index == 0 then
                            HtmlExtra.td [ Fusion.Colors.purpleText "  [ (" ]

                          else
                            HtmlExtra.td [ Fusion.Colors.purpleText "  , (" ]
                        , HtmlExtra.td [ value key ]
                        , HtmlExtra.td [ Fusion.Colors.purpleText ", " ]
                        , HtmlExtra.td [ value val ]
                        , HtmlExtra.td [ Fusion.Colors.purpleText ")" ]
                        ]
                )
                items
            )


custom :
    String
    -> List Value
    -> Html msg
custom valueName valueParams =
    let
        variantPicker : Html msg
        variantPicker =
            Html.text valueName
    in
    if List.isEmpty valueParams then
        variantPicker

    else
        HtmlExtra.columnWithBorder
            [ variantPicker
            , HtmlExtra.row (List.map value valueParams)
            ]


record :
    Dict String Value
    -> Html msg
record fields =
    Html.table
        []
        (List.map
            (\( name, field ) ->
                Html.tr
                    []
                    [ HtmlExtra.td [ Html.text name ]
                    , HtmlExtra.td [ Fusion.Colors.purpleText " = " ]
                    , HtmlExtra.td [ value field ]
                    ]
            )
            (Dict.toList fields)
        )


tuple :
    List Value
    -> Html msg
tuple children =
    let
        values : List (Html msg)
        values =
            children
                |> List.map value
                |> List.intersperse (Fusion.Colors.purpleText ", ")
    in
    (Fusion.Colors.purpleText "(" :: values ++ [ Fusion.Colors.purpleText ")" ])
        |> HtmlExtra.row


{-| View a type.
-}
type_ : Type -> Html msg
type_ stub =
    let
        specialTypeStyle : List (Html.Attribute msg)
        specialTypeStyle =
            [ Html.Attributes.style "color" Fusion.Colors.orange
            , Html.Attributes.style "padding" "2px"
            , Html.Attributes.style "border" "1px white solid"
            ]
    in
    case stub of
        TNamed _ _ _ (Just TString) ->
            Html.div specialTypeStyle [ Html.text "String" ]

        TNamed _ _ _ (Just TInt) ->
            Html.div specialTypeStyle [ Html.text "Int" ]

        TNamed _ _ _ (Just TFloat) ->
            Html.div specialTypeStyle [ Html.text "Float" ]

        TNamed _ _ _ (Just TBool) ->
            Html.div specialTypeStyle [ Html.text "Bool" ]

        TNamed _ _ _ (Just TChar) ->
            Html.div specialTypeStyle [ Html.text "Char" ]

        TNamed _ _ _ (Just TNever) ->
            Html.div specialTypeStyle [ Html.text "Never" ]

        TNamed _ _ _ (Just TOrder) ->
            Html.div specialTypeStyle [ Html.text "Order" ]

        TNamed moduleName name params _ ->
            HtmlExtra.rowWithBorder
                (Fusion.Colors.orangeText
                    (String.join "." (moduleName ++ [ name ]))
                    :: List.map type_ params
                )

        TVar name ->
            Html.div [ Html.Attributes.style "border" "1px white solid" ] [ Fusion.Colors.purpleText name ]

        TUnit ->
            Html.div specialTypeStyle [ Html.text "()" ]

        TTuple firstType secondType ->
            HtmlExtra.rowWithBorder
                [ Fusion.Colors.purpleText "("
                , type_ firstType
                , Fusion.Colors.purpleText ","
                , type_ secondType
                , Fusion.Colors.purpleText ")"
                ]

        TTriple firstType secondType thirdType ->
            HtmlExtra.rowWithBorder
                [ Fusion.Colors.purpleText "("
                , type_ firstType
                , Fusion.Colors.purpleText ","
                , type_ secondType
                , Fusion.Colors.purpleText ","
                , type_ thirdType
                , Fusion.Colors.purpleText ")"
                ]

        TCustom name params constructors ->
            let
                viewConstructor : ( String, List Type ) -> Html msg
                viewConstructor ( cname, cparams ) =
                    HtmlExtra.row
                        ([ Html.text " |", Fusion.Colors.orangeText cname ]
                            ++ List.map type_ cparams
                        )
            in
            HtmlExtra.column
                (HtmlExtra.row
                    (Fusion.Colors.purpleText "type"
                        :: Fusion.Colors.orangeText name
                        :: List.map Fusion.Colors.purpleText params
                    )
                    :: List.map viewConstructor constructors
                )

        TRecord fields ->
            let
                viewField : ( String, Type ) -> Html msg
                viewField ( fieldName, fieldType ) =
                    HtmlExtra.row
                        [ Html.text fieldName
                        , Fusion.Colors.purpleText " : "
                        , type_ fieldType
                        ]
            in
            HtmlExtra.column (List.map viewField fields)

        TGenericRecord var fields ->
            let
                viewField : ( String, Type ) -> Html msg
                viewField ( fieldName, fieldType ) =
                    HtmlExtra.row
                        [ Html.text fieldName
                        , Fusion.Colors.purpleText " : "
                        , type_ fieldType
                        ]
            in
            HtmlExtra.row
                [ Html.text <| var ++ " | "
                , HtmlExtra.column (List.map viewField fields)
                ]


viewError : Value -> Value -> Value -> Patch.Error -> Html msg
viewError old new wanted error =
    let
        withChild : String -> Patch.Error -> String -> (Value -> Maybe Value) -> Html msg
        withChild message child what extractor =
            HtmlExtra.wrappedRow
                [ Html.text (capitalize message ++ ", ")
                , case ( extractor old, extractor new, extractor wanted ) of
                    ( Just o, Just n, Just w ) ->
                        viewError o n w child

                    _ ->
                        viewUndisplayable message what
                ]

        capitalize : String -> String
        capitalize str =
            case String.uncons str of
                Nothing ->
                    ""

                Just ( h, t ) ->
                    String.cons (Char.toUpper h) t

        leaf : String -> Html msg
        leaf message =
            Html.text <| capitalize message

        viewUndisplayable : String -> String -> Html msg
        viewUndisplayable where_ what =
            HtmlExtra.row
                [ Html.text <| "Could not show error " ++ where_ ++ ", expected " ++ what ++ ", got: old "
                , value old
                , Html.text " and new "
                , value new
                , Html.text " and wanted "
                , value wanted
                ]
    in
    case ( ( old, new, wanted ), error ) of
        ( _, Patch.WrongType location ) ->
            Html.text <| "Wrong type in " ++ location ++ ". The type on the backend probably changed."

        ( _, Patch.Conflict ) ->
            Html.table
                [ Html.Attributes.style "color" "white", Html.Attributes.style "padding" "10px" ]
                [ Html.tr
                    []
                    [ Html.th [] [ Html.text "Old" ]
                    , Html.th [] [ Html.text "New" ]
                    , Html.th [] [ Html.text "Wanted" ]
                    , Html.th [] [ Html.text "Issue" ]
                    ]
                , Html.tr
                    []
                    [ HtmlExtra.td [ value old ]
                    , HtmlExtra.td [ value new ]
                    , HtmlExtra.td [ value wanted ]
                    , HtmlExtra.td [ Html.text "Conflict" ]
                    ]
                ]

        ( _, Patch.CouldNotBuildValueFromPatch ) ->
            leaf "internal error: the patch was insufficient to build the new value."

        ( _, Patch.MissingField field ) ->
            leaf <| "missing field: " ++ field

        ( _, Patch.UnexpectedField field ) ->
            leaf <| "unexpected field: " ++ field

        ( _, Patch.MissingValue ) ->
            leaf "missing value"

        ( ( VRecord rold, VRecord rnew, VRecord rwanted ), Patch.ErrorAtField field child ) ->
            viewRecordError rold rnew rwanted field child

        ( _, Patch.ErrorAtField field _ ) ->
            viewUndisplayable ("at field " ++ field) "records"

        ( ( VTuple toldl _, VTuple tnewl _, VTuple twantedl twantedr ), Patch.ErrorAtFirst child ) ->
            viewTupleError
                [ TupleConflict toldl tnewl twantedl child
                , TupleNonconflict twantedr
                ]

        ( ( VTriple toldl _ _, VTriple tnewl _ _, VTriple twantedl twantedm twantedr ), Patch.ErrorAtFirst child ) ->
            viewTupleError
                [ TupleConflict toldl tnewl twantedl child
                , TupleNonconflict twantedm
                , TupleNonconflict twantedr
                ]

        ( _, Patch.ErrorAtFirst _ ) ->
            viewUndisplayable "in the first item of the tuple" "tuples"

        ( ( VTuple _ oldSecond, VTuple _ newSecond, VTuple wantedFirst wantedSecond ), Patch.ErrorAtSecond child ) ->
            viewTupleError
                [ TupleNonconflict wantedFirst
                , TupleConflict oldSecond newSecond wantedSecond child
                ]

        ( ( VTriple _ oldSecond _, VTriple _ newSecond _, VTriple wantedFirst wantedSecond wantedThird ), Patch.ErrorAtSecond child ) ->
            viewTupleError
                [ TupleNonconflict wantedFirst
                , TupleConflict oldSecond newSecond wantedSecond child
                , TupleNonconflict wantedThird
                ]

        ( _, Patch.ErrorAtSecond _ ) ->
            viewUndisplayable "in the second item of the tuple" "tuples"

        ( ( VTriple _ _ oldThird, VTriple _ _ newThird, VTriple wantedFirst wantedSecond wantedThird ), Patch.ErrorAtThird child ) ->
            viewTupleError
                [ TupleNonconflict wantedFirst
                , TupleNonconflict wantedSecond
                , TupleConflict oldThird newThird wantedThird child
                ]

        ( _, Patch.ErrorAtThird _ ) ->
            viewUndisplayable "in the third item of the tuple" "tuples"

        ( _, Patch.ErrorAtCustomArgument index child ) ->
            withChild ("at the custom argument number " ++ String.fromInt index) child "a custom type" <|
                \v ->
                    case v of
                        VCustom _ args ->
                            List.Extra.getAt index args

                        _ ->
                            Nothing

        ( _, Patch.ErrorAtIndex index child ) ->
            withChild ("at index " ++ String.fromInt index) child "a list or set" <|
                \v ->
                    case v of
                        VList { items } ->
                            List.Extra.getAt index items

                        VSet { items } ->
                            List.Extra.getAt index items

                        _ ->
                            Nothing

        ( _, Patch.ErrorAtKey key child ) ->
            HtmlExtra.wrappedRow [ Html.text "at key ", value key, Html.text ", ", viewError old new wanted child ]

        ( _, Patch.ErrorAtValueWithKey key child ) ->
            HtmlExtra.wrappedRow [ Html.text "at value with key ", value key, Html.text ", ", viewError old new wanted child ]


{-| -}
viewPatchError : Value -> Value -> Value -> Patch.Error -> Html msg
viewPatchError old new wanted error =
    HtmlExtra.column
        [ Html.b
            [ Html.Attributes.style "color" Fusion.Colors.error ]
            [ Html.text "Error patching backend: " ]
        , viewError old new wanted error
        ]


type TupleConflict
    = TupleConflict Value Value Value Patch.Error
    | TupleNonconflict Value


viewTupleError : List TupleConflict -> Html msg
viewTupleError children =
    let
        viewConflict : TupleConflict -> Html msg
        viewConflict conflict =
            case conflict of
                TupleConflict old new wanted error ->
                    viewError old new wanted error

                TupleNonconflict inner ->
                    value inner

        values : List (Html msg)
        values =
            children
                |> List.map viewConflict
                |> List.intersperse (Fusion.Colors.purpleText ", ")
    in
    HtmlExtra.row <| Fusion.Colors.purpleText "(" :: values ++ [ Fusion.Colors.purpleText ")" ]


viewRecordError : Dict String Value -> Dict String Value -> Dict String Value -> String -> Patch.Error -> Html msg
viewRecordError old new wanted field error =
    case Dict.get field old of
        Nothing ->
            Html.text <| "Could not find field \"" ++ field ++ "\" in the old value"

        Just oldValue ->
            case Dict.get field wanted of
                Nothing ->
                    Html.text <| "Could not find field \"" ++ field ++ "\" in the wanted value"

                Just wantedValue ->
                    Html.table
                        []
                        (List.map
                            (\( key, val ) ->
                                Html.tr
                                    []
                                    [ Html.td
                                        []
                                        [ if key == field then
                                            Html.b [ Html.Attributes.style "color" "white" ] [ Html.text key ]

                                          else
                                            Html.text key
                                        ]
                                    , Html.td
                                        []
                                        [ if key == field then
                                            viewError oldValue val wantedValue error

                                          else
                                            value val
                                        ]
                                    ]
                            )
                            (Dict.toList new)
                        )


{-| -}
viewPatch : Patch -> Html msg
viewPatch patch =
    let
        patchToStringMaybe : Maybe Patch -> Html msg
        patchToStringMaybe maybePatch =
            maybePatch
                |> Maybe.map viewPatch
                |> Maybe.withDefault (Html.text "Nothing")

        viewList : String -> (a -> Html msg) -> List a -> Html msg
        viewList header viewer args =
            HtmlExtra.rowWithBorder
                [ Html.div
                    [ Html.Attributes.style "align-self" "center"
                    , Html.Attributes.style "padding" "2px"
                    ]
                    [ Html.text header ]
                , HtmlExtra.column
                    (List.map
                        (\arg ->
                            Html.div
                                [ Html.Attributes.style "border" "1px white solid"
                                , Html.Attributes.style "padding" "2px"
                                ]
                                [ viewer arg ]
                        )
                        args
                    )
                ]

        basic : String -> (a -> String) -> a -> a -> Html msg
        basic name toString old v =
            Html.text <| name ++ " " ++ toString old ++ " " ++ toString v
    in
    case patch of
        PInt old v ->
            basic "PInt " String.fromInt old v

        PFloat old v ->
            basic "PFloat " String.fromFloat old v

        PString old v ->
            basic "PString " Fusion.Internal.escapeString old v

        PBool old v ->
            basic "PBool" Fusion.Internal.boolToString old v

        PChar old v ->
            basic "PChar" (\c -> "'" ++ String.fromChar c ++ "'") old v

        PUnit ->
            Html.text "PUnit"

        PBytes _ _ ->
            Html.text "PBytes <bytes> <bytes>"

        PTuple l r ->
            viewList "PTuple" patchToStringMaybe [ l, r ]

        PTriple l m r ->
            viewList "PTriple" patchToStringMaybe [ l, m, r ]

        PList list ->
            viewList "PList"
                identity
                [ viewList "added"
                    (\( k, v ) -> HtmlExtra.row [ Html.text <| String.fromInt k, Fusion.Colors.purpleText ": ", value v ])
                    (Dict.toList list.added)
                , viewList "edited"
                    (\( k, v ) -> HtmlExtra.row [ Html.text <| String.fromInt k, Fusion.Colors.purpleText ": ", viewPatch v ])
                    (Dict.toList list.edited)
                , viewList "removed" (Html.text << String.fromInt) (Dict.keys list.removed)
                ]

        PSet set ->
            viewList "PSet"
                identity
                [ viewList "added" value (ValueDict.keys set.added)
                , viewList "removed" value (ValueDict.keys set.removed)
                ]

        PDict pdict ->
            viewList "PDict"
                identity
                [ viewList "added"
                    (\( k, v ) -> HtmlExtra.row [ value k, Fusion.Colors.purpleText ": ", value v ])
                    (ValueDict.toList pdict.added)
                , viewList "edited"
                    (\( k, v ) -> HtmlExtra.row [ value k, Fusion.Colors.purpleText ": ", viewPatch v ])
                    (ValueDict.toList pdict.edited)
                , viewList "removed" value (ValueDict.keys pdict.removed)
                ]

        PCustomSame name args ->
            viewList ("PCustomSame " ++ name) patchToStringMaybe args

        PCustomChange old name args ->
            viewList ("PCustomChange " ++ old ++ " " ++ name) value args

        PRecord fields ->
            viewList "PRecord"
                (\( fieldName, fieldValue ) ->
                    HtmlExtra.row
                        [ Html.text fieldName
                        , Fusion.Colors.purpleText ": "
                        , viewPatch fieldValue
                        ]
                )
                (Dict.toList fields)

        PSetCursor cursor ->
            Html.text ("PSetCursor " ++ String.fromInt cursor)
