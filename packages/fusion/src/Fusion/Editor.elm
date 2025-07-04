module Fusion.Editor exposing (Config, value, view)

{-|

@docs Config, value, view

-}

import Dict exposing (Dict)
import Fusion exposing (Query(..), SpecialType(..), Type(..), Value(..))
import Fusion.Colors
import Fusion.Internal
import Fusion.Patch as Patch exposing (DictPatch, Patch(..))
import Fusion.Value as Value
import Fusion.ValueDict as ValueDict
import Fusion.View exposing (type_)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import HtmlExtra
import List.Extra
import Result.Extra


{-| When showing a dict/list/set, how many elements to show per page.
-}
paginationSize : Bool -> number
paginationSize simple =
    if simple then
        20

    else
        5


{-| The main Fusion view
-}
view :
    { typeDict : Dict (List String) (Dict String ( Type, List String ))
    , type_ : Maybe Type
    , patch : Maybe Patch
    , patchError : Maybe ( Value, Patch.Error )
    , editMsg : Patch -> msg
    , queryMsg : Query -> msg
    , applyMsg : Bool -> Patch -> msg
    }
    -> Value
    -> Html msg
view { typeDict, type_, patch, patchError, editMsg, queryMsg, applyMsg } input =
    let
        wanted : Value
        wanted =
            case patch of
                Just p ->
                    Patch.patch { force = False } p input
                        -- TODO: Handle this better somehow?
                        -- If it happens it means that the backend type has changed
                        |> Result.withDefault input

                Nothing ->
                    input
    in
    HtmlExtra.column
        [ Html.div
            [ Html.Attributes.style "border" ("1px " ++ Fusion.Colors.error ++ " solid")
            ]
            [ value
                { typeDict = typeDict
                , type_ = type_
                , editMsg = editMsg
                , queryMsg = queryMsg
                }
                wanted
            ]
        , case patch of
            Nothing ->
                Html.text "No changes yet."

            Just changes ->
                HtmlExtra.column
                    [ Fusion.View.viewPatch changes
                    , HtmlExtra.row
                        [ button 1 2 (applyMsg False changes) "Apply"
                        , Html.div [ Html.Attributes.style "width" "10px" ] []
                        , case patchError of
                            Nothing ->
                                HtmlExtra.none

                            Just _ ->
                                button 1 2 (applyMsg True changes) "Apply - force"
                        ]
                    , case patchError of
                        Nothing ->
                            HtmlExtra.none

                        Just ( new, error ) ->
                            Fusion.View.viewPatchError input new wanted error
                    ]
        ]


{-| Data the editor needs to display values.
-}
type alias Config msg =
    { typeDict : Dict (List String) (Dict String ( Type, List String ))
    , editMsg : Patch -> msg
    , queryMsg : Query -> msg
    , type_ : Maybe Type
    }


htmlTextInput : (String -> msg) -> String -> Html msg
htmlTextInput onChange text =
    Html.input
        [ Html.Attributes.type_ "text"
        , Html.Attributes.value text
        , Html.Events.onInput onChange
        , Html.Attributes.style "border" "0"
        , Html.Attributes.style "background" Fusion.Colors.textInputBackground
        , Html.Attributes.style "color" Fusion.Colors.value
        ]
        []


htmlCheckbox : Bool -> String -> (Bool -> msg) -> Html msg
htmlCheckbox isChecked label msg =
    Html.label
        []
        [ Html.input
            [ Html.Attributes.type_ "checkbox", Html.Attributes.checked isChecked, Html.Events.onCheck msg ]
            []
        , Html.text label
        ]


htmlMultiline : (String -> msg) -> String -> Html msg
htmlMultiline onChange text =
    Html.textarea
        [ Html.Attributes.value text
        , Html.Events.onInput onChange
        , Html.Attributes.style "border" "0"
        , Html.Attributes.style "background" Fusion.Colors.textInputBackground
        , Html.Attributes.style "color" Fusion.Colors.value
        , Html.Attributes.style "display" "inline-block"
        ]
        []


{-| View the editor for a value.
-}
value : Config msg -> Value -> Html msg
value ({ typeDict, type_, editMsg, queryMsg } as config) input =
    case ( type_, input ) of
        ( _, VInt i ) ->
            htmlTextInput
                (\newValue ->
                    newValue
                        |> String.toInt
                        |> Maybe.withDefault i
                        |> PInt i
                        |> editMsg
                )
                (String.fromInt i)

        ( _, VFloat i ) ->
            htmlTextInput
                (\newValue ->
                    newValue
                        |> String.toFloat
                        |> Maybe.withDefault i
                        |> PFloat i
                        |> editMsg
                )
                (String.fromFloat i)

        ( _, VString i ) ->
            HtmlExtra.row
                [ Html.div
                    [ Html.Attributes.style "display" "inline-block"
                    , Html.Attributes.style "color" Fusion.Colors.quote
                    ]
                    [ Html.text "\"" ]
                , htmlMultiline
                    (\newValue ->
                        newValue
                            |> PString i
                            |> editMsg
                    )
                    i
                , Html.div
                    [ Html.Attributes.style "display" "inline-block"
                    , Html.Attributes.style "color" Fusion.Colors.quote
                    , Html.Attributes.style "align-self" "flex-end"
                    ]
                    [ Html.text "\"" ]
                ]

        ( _, VChar i ) ->
            HtmlExtra.row
                [ Html.div
                    [ Html.Attributes.style "display" "inline-block"
                    , Html.Attributes.style "color" Fusion.Colors.quote
                    ]
                    [ Html.text "'" ]
                , htmlTextInput
                    (\newValue ->
                        case String.toList newValue of
                            [ newChar ] ->
                                newChar
                                    |> PChar i
                                    |> editMsg

                            _ ->
                                editMsg <| PChar i i
                    )
                    (String.fromChar i)
                , Html.div
                    [ Html.Attributes.style "display" "inline-block"
                    , Html.Attributes.style "color" Fusion.Colors.quote
                    , Html.Attributes.style "align-self" "flex-end"
                    ]
                    [ Html.text "'" ]
                ]

        ( _, VBytes _ ) ->
            Html.div
                [ Html.Attributes.style "color" Fusion.Colors.orange
                ]
                [ Html.text "<bytes>" ]

        ( _, VBool b ) ->
            htmlCheckbox b (Fusion.Internal.boolToString b) (editMsg << PBool b)

        ( Just (TNamed _ _ _ (Just (TList child))), VList values ) ->
            viewListEditor { config | type_ = Just child } values

        ( Nothing, VList values ) ->
            viewListEditor { config | type_ = Nothing } values

        ( Just (TNamed _ _ _ (Just (TSet child))), VSet values ) ->
            viewSetEditor { config | type_ = Just child } values

        ( Nothing, VSet values ) ->
            viewSetEditor { config | type_ = Nothing } values

        ( Just (TNamed _ _ _ (Just (TDict keyType valueType))), VDict values ) ->
            viewDictEditor
                { typeDict = typeDict
                , editMsg = editMsg
                , queryMsg = queryMsg
                , keyType = Just keyType
                , valueType = Just valueType
                }
                values

        ( Nothing, VDict values ) ->
            viewDictEditor
                { typeDict = typeDict
                , editMsg = editMsg
                , queryMsg = queryMsg
                , keyType = Nothing
                , valueType = Nothing
                }
                values

        ( Just (TNamed moduleName name args _), _ ) ->
            let
                child : Result String Type
                child =
                    Value.getType typeDict moduleName name args
            in
            (if isSimple input then
                HtmlExtra.row

             else
                HtmlExtra.column
            )
                [ Fusion.Colors.orangeText (String.join "." (moduleName ++ [ name ]))
                , value { config | type_ = child |> Result.toMaybe } input
                ]

        ( _, VUnit ) ->
            Html.div
                [ Html.Attributes.style "color" Fusion.Colors.orange
                ]
                [ Html.text "()" ]

        ( Just (TTuple lt rt), VTuple l r ) ->
            let
                children : List ( Value, Maybe Type, Patch -> Patch )
                children =
                    [ ( l, Just lt, \nl -> PTuple (Just nl) Nothing )
                    , ( r, Just rt, \nr -> PTuple Nothing (Just nr) )
                    ]
            in
            viewTupleEditor
                { typeDict = typeDict
                , editMsg = editMsg
                , queryMsg = queryMsg
                }
                children

        ( Nothing, VTuple l r ) ->
            let
                children : List ( Value, Maybe Type, Patch -> Patch )
                children =
                    [ ( l, Nothing, \nl -> PTuple (Just nl) Nothing )
                    , ( r, Nothing, \nr -> PTuple Nothing (Just nr) )
                    ]
            in
            viewTupleEditor
                { typeDict = typeDict
                , editMsg = editMsg
                , queryMsg = queryMsg
                }
                children

        ( Just (TTriple lt mt rt), VTriple l m r ) ->
            let
                children : List ( Value, Maybe Type, Patch -> Patch )
                children =
                    [ ( l, Just lt, \nl -> PTriple (Just nl) Nothing Nothing )
                    , ( m, Just mt, \nm -> PTriple Nothing (Just nm) Nothing )
                    , ( r, Just rt, \nr -> PTriple Nothing Nothing (Just nr) )
                    ]
            in
            viewTupleEditor
                { typeDict = typeDict
                , editMsg = editMsg
                , queryMsg = queryMsg
                }
                children

        ( Nothing, VTriple l m r ) ->
            let
                children : List ( Value, Maybe Type, Patch -> Patch )
                children =
                    [ ( l, Nothing, \nl -> PTriple (Just nl) Nothing Nothing )
                    , ( m, Nothing, \nm -> PTriple Nothing (Just nm) Nothing )
                    , ( r, Nothing, \nr -> PTriple Nothing Nothing (Just nr) )
                    ]
            in
            viewTupleEditor
                { typeDict = typeDict
                , editMsg = editMsg
                , queryMsg = queryMsg
                }
                children

        ( Just (TCustom _ _ variants), VCustom valueName valueParams ) ->
            viewCustomEditor
                { typeDict = typeDict
                , editMsg = editMsg
                , queryMsg = queryMsg
                , variants = Just variants
                }
                valueName
                valueParams

        ( Nothing, VCustom valueName valueParams ) ->
            viewCustomEditor
                { typeDict = typeDict
                , editMsg = editMsg
                , queryMsg = queryMsg
                , variants = Nothing
                }
                valueName
                valueParams

        ( Just (TRecord fieldTypes), VRecord fieldValues ) ->
            viewRecordEditor
                { typeDict = typeDict
                , editMsg = editMsg
                , queryMsg = queryMsg
                , fieldTypes = Just fieldTypes
                }
                fieldValues

        ( Nothing, VRecord fieldValues ) ->
            viewRecordEditor
                { typeDict = typeDict
                , editMsg = editMsg
                , queryMsg = queryMsg
                , fieldTypes = Nothing
                }
                fieldValues

        ( _, VUnloaded ) ->
            Html.button
                [ Html.Attributes.style "color" Fusion.Colors.error
                , Html.Events.onClick (queryMsg QLoad)
                ]
                [ Html.text "Unloaded" ]

        _ ->
            typeError type_ [ Html.text "value ", Fusion.View.value input ]


viewListEditor :
    { typeDict : Dict (List String) (Dict String ( Type, List String ))
    , editMsg : Patch -> msg
    , queryMsg : Query -> msg
    , type_ : Maybe Type
    }
    ->
        { cursor : Int
        , items : List Value
        }
    -> Html msg
viewListEditor { typeDict, editMsg, queryMsg, type_ } { cursor, items } =
    let
        simple : Bool
        simple =
            List.all isSimple items

        slice : List Value
        slice =
            items
                |> List.drop cursor
                |> List.take (paginationSize simple)

        viewItem : Int -> Value -> Html msg
        viewItem rawIndex val =
            let
                index : Int
                index =
                    rawIndex + cursor
            in
            value
                { typeDict = typeDict
                , type_ = type_
                , editMsg =
                    \patch ->
                        { added = Dict.empty
                        , removed = Dict.empty
                        , edited = Dict.singleton index patch
                        }
                            |> PList
                            |> editMsg
                , queryMsg =
                    \query -> queryMsg <| QIndexed (VInt index) query
                }
                val

        values : () -> List (Html msg)
        values () =
            slice
                |> List.indexedMap viewItem
                |> List.intersperse (Fusion.Colors.purpleText ", ")
    in
    if cursor == 0 && List.length items <= paginationSize simple && simple then
        HtmlExtra.wrappedRow <| Fusion.Colors.purpleText "[" :: values () ++ [ Fusion.Colors.purpleText "]" ]

    else
        let
            maybeKeys : Maybe (List String)
            maybeKeys =
                case type_ of
                    Just (TRecord fields) ->
                        Just (List.map Tuple.first fields)

                    Just _ ->
                        Nothing

                    Nothing ->
                        List.Extra.findMap
                            (\e ->
                                case e of
                                    VRecord fields ->
                                        Just (Dict.keys fields)

                                    _ ->
                                        Nothing
                            )
                            slice
        in
        case maybeKeys of
            Nothing ->
                HtmlExtra.column
                    [ paginationControls simple editMsg cursor items
                    , HtmlExtra.column (values ())
                    ]

            Just keys ->
                let
                    rows : List (Html msg)
                    rows =
                        List.indexedMap
                            (\rawIndex item ->
                                Html.tr
                                    []
                                    (List.map
                                        (\key ->
                                            case item of
                                                VRecord itemFields ->
                                                    Html.td
                                                        []
                                                        [ Dict.get key itemFields
                                                            |> Maybe.withDefault VUnloaded
                                                            |> (\val ->
                                                                    let
                                                                        index : Int
                                                                        index =
                                                                            rawIndex + cursor

                                                                        fieldType : Maybe Type
                                                                        fieldType =
                                                                            case type_ of
                                                                                Just (TRecord typeFields) ->
                                                                                    List.Extra.findMap
                                                                                        (\( k, v ) ->
                                                                                            if k == key then
                                                                                                Just v

                                                                                            else
                                                                                                Nothing
                                                                                        )
                                                                                        typeFields

                                                                                _ ->
                                                                                    Nothing
                                                                    in
                                                                    value
                                                                        { typeDict = typeDict
                                                                        , type_ = fieldType
                                                                        , editMsg =
                                                                            \patch ->
                                                                                { added = Dict.empty
                                                                                , removed = Dict.empty
                                                                                , edited =
                                                                                    Dict.singleton key patch
                                                                                        |> PRecord
                                                                                        |> Dict.singleton index
                                                                                }
                                                                                    |> PList
                                                                                    |> editMsg
                                                                        , queryMsg =
                                                                            \query ->
                                                                                QRecord key query
                                                                                    |> QIndexed (VInt index)
                                                                                    |> queryMsg
                                                                        }
                                                                        val
                                                               )
                                                        ]

                                                _ ->
                                                    HtmlExtra.td [ viewItem rawIndex item ]
                                        )
                                        keys
                                    )
                            )
                            slice
                in
                Html.table
                    []
                    (Html.tr []
                        (List.map
                            (\key ->
                                Html.th
                                    [ Html.Attributes.style "text-align" "left" ]
                                    [ Html.text key ]
                            )
                            keys
                        )
                        :: rows
                    )


isSimple : Value -> Bool
isSimple v =
    case v of
        VTuple l r ->
            isSimple l && isSimple r

        VTriple l m r ->
            isSimple l && isSimple m && isSimple r

        VList { items } ->
            List.all isSimple items && List.length items < paginationSize True

        VSet _ ->
            False

        VDict _ ->
            False

        VCustom _ args ->
            List.all isSimple args

        VRecord _ ->
            False

        _ ->
            True


viewSetEditor :
    Config msg
    ->
        { cursor : Int
        , items : List Value
        }
    -> Html msg
viewSetEditor ({ editMsg, queryMsg } as config) { cursor, items } =
    let
        simple : Bool
        simple =
            List.all isSimple items

        values : List (Html msg)
        values =
            items
                |> List.drop cursor
                |> List.take (paginationSize simple)
                |> List.indexedMap
                    (\rawIndex val ->
                        let
                            index : Int
                            index =
                                rawIndex + cursor
                        in
                        value
                            { config
                                | editMsg =
                                    \patch ->
                                        let
                                            newValue : Value
                                            newValue =
                                                Result.withDefault val <| Patch.patch { force = False } patch val
                                        in
                                        { added = ValueDict.singleton newValue ()
                                        , removed = ValueDict.singleton val ()
                                        }
                                            |> PSet
                                            |> editMsg
                                , queryMsg =
                                    \query -> queryMsg <| QIndexed (VInt index) query
                            }
                            val
                    )
                |> List.intersperse (Fusion.Colors.purpleText ", ")
    in
    if cursor == 0 && List.length items <= paginationSize simple && simple then
        HtmlExtra.wrappedRow <| Fusion.Colors.purpleText "[" :: values ++ [ Fusion.Colors.purpleText "]" ]

    else
        HtmlExtra.column
            [ paginationControls simple editMsg cursor items
            , HtmlExtra.column values
            ]


viewDictEditor :
    { typeDict : Dict (List String) (Dict String ( Type, List String ))
    , editMsg : Patch -> msg
    , queryMsg : Query -> msg
    , keyType : Maybe Type
    , valueType : Maybe Type
    }
    ->
        { cursor : Int
        , items : List ( Value, Value )
        }
    -> Html msg
viewDictEditor { typeDict, editMsg, queryMsg, keyType, valueType } { cursor, items } =
    if List.isEmpty items then
        HtmlExtra.row [ Html.text "Dict.fromList ", Fusion.Colors.purpleText "[]" ]

    else
        let
            simple : Bool
            simple =
                List.all (\( k, v ) -> isSimple k && isSimple v) items

            viewBeforeKey : Int -> Html msg
            viewBeforeKey index =
                if index == 0 then
                    Fusion.Colors.purpleText "  [ ("

                else
                    Fusion.Colors.purpleText "  , ("

            viewPairKey : Value -> Value -> Html msg
            viewPairKey key val =
                value
                    { typeDict = typeDict
                    , type_ = keyType
                    , editMsg =
                        \patch ->
                            let
                                newKey : Value
                                newKey =
                                    Result.withDefault key <| Patch.patch { force = False } patch key

                                dictPatch : DictPatch
                                dictPatch =
                                    { added = ValueDict.singleton newKey val
                                    , edited = ValueDict.empty
                                    , removed = ValueDict.singleton key val
                                    }
                            in
                            dictPatch
                                |> PDict
                                |> editMsg
                    , queryMsg = \query -> queryMsg <| QIndexed key query
                    }
                    key

            viewPairValue : Value -> Value -> Html msg
            viewPairValue key val =
                value
                    { typeDict = typeDict
                    , type_ = valueType
                    , editMsg =
                        \patch ->
                            { added = ValueDict.empty
                            , edited = ValueDict.singleton key patch
                            , removed = ValueDict.empty
                            }
                                |> PDict
                                |> editMsg
                    , queryMsg = \query -> queryMsg <| QIndexed key query
                    }
                    val

            values : List ( Value, Value )
            values =
                items
                    |> List.drop cursor
                    |> List.take (paginationSize simple)
        in
        if cursor == 0 && List.length items <= paginationSize simple && simple then
            let
                views : List (Html msg)
                views =
                    values
                        |> List.indexedMap
                            (\rawIndex ( key, val ) ->
                                let
                                    index : Int
                                    index =
                                        rawIndex + cursor
                                in
                                HtmlExtra.row
                                    [ viewBeforeKey index
                                    , viewPairKey key val
                                    , Fusion.Colors.purpleText ", "
                                    , viewPairValue key val
                                    , Fusion.Colors.purpleText ")"
                                    ]
                            )
            in
            HtmlExtra.wrappedRow <| Html.text "Dict.fromList " :: views ++ [ Fusion.Colors.purpleText "]" ]

        else
            HtmlExtra.column
                [ paginationControls simple editMsg cursor items
                , Html.table
                    []
                    (List.indexedMap
                        (\rawIndex ( key, val ) ->
                            Html.tr
                                []
                                [ HtmlExtra.td [ viewBeforeKey (rawIndex + cursor) ]
                                , HtmlExtra.td [ viewPairKey key val ]
                                , HtmlExtra.td [ Fusion.Colors.purpleText ", " ]
                                , HtmlExtra.td [ viewPairValue key val ]
                                , HtmlExtra.td [ Fusion.Colors.purpleText ")" ]
                                ]
                        )
                        values
                    )
                ]


viewTupleEditor :
    { typeDict : Dict (List String) (Dict String ( Type, List String ))
    , editMsg : Patch -> msg
    , queryMsg : Query -> msg
    }
    -> List ( Value, Maybe Type, Patch -> Patch )
    -> Html msg
viewTupleEditor { typeDict, editMsg, queryMsg } children =
    let
        values : List (Html msg)
        values =
            children
                |> List.indexedMap
                    (\index ( val, child, updater ) ->
                        value
                            { typeDict = typeDict
                            , type_ =
                                child
                            , editMsg =
                                \patch ->
                                    patch
                                        |> updater
                                        |> editMsg
                            , queryMsg = \query -> queryMsg <| QIndexed (VInt index) query
                            }
                            val
                    )
                |> List.intersperse (Fusion.Colors.purpleText ", ")
    in
    HtmlExtra.row <| Fusion.Colors.purpleText "(" :: values ++ [ Fusion.Colors.purpleText ")" ]


viewRecordEditor :
    { typeDict : Dict (List String) (Dict String ( Type, List String ))
    , editMsg : Patch -> msg
    , queryMsg : Query -> msg
    , fieldTypes : Maybe (List ( String, Type ))
    }
    -> Dict String Value
    -> Html msg
viewRecordEditor { typeDict, editMsg, queryMsg, fieldTypes } fieldValues =
    let
        rows : List ( ( String, Maybe Type ), ( String, Value ) )
        rows =
            List.map2 Tuple.pair
                (case fieldTypes of
                    Just tf ->
                        tf
                            |> List.sortBy Tuple.first
                            |> List.map (\( fieldName, fieldType ) -> ( fieldName, Just fieldType ))

                    Nothing ->
                        Dict.toList fieldValues
                            |> List.map (\( name, _ ) -> ( name, Nothing ))
                )
                (Dict.toList fieldValues)
    in
    HtmlExtra.columnWithBorder
        (List.concatMap
            (\( ( fieldNameFromType, fieldType ), ( fieldNameFromValue, fieldValue ) ) ->
                [ Html.div []
                    [ Html.text (fieldNameFromValue ++ " ")
                    , Fusion.Colors.purpleText "="
                    ]
                , Html.div
                    [ Html.Attributes.style "padding-left" "16px" ]
                    [ value
                        { typeDict = typeDict
                        , type_ =
                            if fieldNameFromType == fieldNameFromValue then
                                fieldType

                            else
                                Nothing
                        , editMsg = \patch -> editMsg <| PRecord <| Dict.singleton fieldNameFromValue patch
                        , queryMsg = \query -> queryMsg <| QRecord fieldNameFromValue query
                        }
                        fieldValue
                    ]
                ]
            )
            rows
        )


viewCustomEditor :
    { typeDict : Dict (List String) (Dict String ( Type, List String ))
    , editMsg : Patch -> msg
    , queryMsg : Query -> msg
    , variants : Maybe (List ( String, List Type ))
    }
    -> String
    -> List Value
    -> Html msg
viewCustomEditor ({ typeDict, editMsg, queryMsg } as config) valueName valueParams =
    let
        length : Int
        length =
            List.length valueParams

        nothings : List (Maybe a)
        nothings =
            List.repeat length Nothing

        variantPicker : Html msg
        variantPicker =
            case config.variants of
                Nothing ->
                    Fusion.Colors.orangeText valueName

                Just variants ->
                    Html.select
                        [ Html.Events.onInput
                            (\variant ->
                                variants
                                    |> List.Extra.find (\( variantName, _ ) -> variantName == variant)
                                    |> Maybe.map Tuple.second
                                    |> Maybe.andThen
                                        (\args ->
                                            args
                                                |> Result.Extra.combineMap (Value.typeToDefault typeDict)
                                                |> Result.toMaybe
                                        )
                                    |> Maybe.map (PCustomChange valueName variant)
                                    |> Maybe.withDefault (PCustomSame valueName nothings)
                                    |> editMsg
                            )
                        , Html.Attributes.style "background" "#333"
                        , Html.Attributes.style "border" "none"
                        , Html.Attributes.style "padding" "5px"
                        , Html.Attributes.style "color" Fusion.Colors.orange
                        ]
                        (List.map
                            (\( variantName, _ ) ->
                                Html.option
                                    [ Html.Attributes.value variantName
                                    , Html.Attributes.selected (variantName == valueName)
                                    ]
                                    [ Html.text variantName
                                    ]
                            )
                            (List.sortBy Tuple.first variants)
                        )

        valueParamsViews : List (Html msg)
        valueParamsViews =
            let
                inner : Maybe Type -> Int -> Value -> Html msg
                inner paramType i paramValue =
                    value
                        { typeDict = typeDict
                        , type_ = paramType
                        , editMsg =
                            \patch ->
                                nothings
                                    |> List.Extra.setAt i (Just patch)
                                    |> PCustomSame valueName
                                    |> editMsg
                        , queryMsg =
                            --TODO: this is wrong
                            queryMsg
                        }
                        paramValue

                typeParams : List (Maybe Type)
                typeParams =
                    config.variants
                        |> Maybe.andThen
                            (List.Extra.findMap
                                (\( variantName, variantParams ) ->
                                    if variantName == valueName then
                                        Just (List.map Just variantParams)

                                    else
                                        Nothing
                                )
                            )
                        |> Maybe.withDefault (List.repeat length Nothing)
            in
            List.map3 inner
                typeParams
                (List.range 0 (length - 1))
                valueParams
    in
    if List.isEmpty valueParamsViews then
        variantPicker

    else
        (if List.all isSimple valueParams then
            HtmlExtra.rowWithBorder

         else
            HtmlExtra.columnWithBorder
        )
            [ variantPicker
            , HtmlExtra.row valueParamsViews
            ]


typeError : Maybe Type -> List (Html msg) -> Html msg
typeError tipe message =
    Html.b
        [ Html.Attributes.style "color" Fusion.Colors.error ]
        (Html.text "Wrong type information: got type "
            :: Maybe.withDefault (Html.text "<no type info>") (Maybe.map type_ tipe)
            :: Html.text " but "
            :: message
        )


paginationControls : Bool -> (Patch -> msg) -> Int -> List a -> Html msg
paginationControls simple editMsg cursor items =
    if cursor == 0 && List.length items <= paginationSize simple then
        HtmlExtra.none

    else
        let
            len : Int
            len =
                List.length items

            pageNumber : Int
            pageNumber =
                1 + cursor // paginationSize simple

            pageCount : Int
            pageCount =
                (len + paginationSize simple - 1) // paginationSize simple
        in
        HtmlExtra.row
            [ button 1 8 (editMsg (PSetCursor (max 0 (cursor - paginationSize simple)))) "<"
            , Html.div
                [ Html.Attributes.style "align-self" "center", Html.Attributes.style "padding" "4px" ]
                [ " Page "
                    ++ String.fromInt pageNumber
                    ++ " of "
                    ++ String.fromInt pageCount
                    ++ " "
                    |> Html.text
                ]
            , button 1 8 (editMsg (PSetCursor (min (len - 1) (cursor + paginationSize simple)))) ">"
            ]


button : Int -> Int -> msg -> String -> Html msg
button borderWidth padding msg text =
    Html.button
        [ Html.Events.onClick msg
        , Html.Attributes.style "border" (String.fromInt borderWidth ++ "px black solid")
        , Html.Attributes.style "padding" (String.fromInt padding ++ "px")
        ]
        [ Html.text text ]
