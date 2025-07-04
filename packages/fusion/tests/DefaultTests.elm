module DefaultTests exposing (suite)

import Dict exposing (Dict)
import Expect
import Fusion exposing (Type(..), Value(..))
import Fusion.Value
import Test exposing (Test, describe, test)
import Utils exposing (tInt)


suite : Test
suite =
    cases
        |> List.map tryBuild
        |> describe "Value.typeToDefault"


cases : List ( Type, List Type, Maybe Value )
cases =
    [ ( TCustom "MyList"
            []
            [ ( "MyCons"
              , [ tInt
                , TNamed [] "MyList" [] Nothing
                ]
              )
            , ( "MyEmpty", [] )
            ]
      , []
      , Just (VCustom "MyEmpty" [])
      )
    , ( TCustom "MF"
            []
            [ ( "MFCons"
              , [ tInt, TNamed [] "MS" [] Nothing ]
              )
            , ( "MFEmpty", [] )
            ]
      , [ TCustom "MS"
            []
            [ ( "MSCons"
              , [ tInt, TNamed [] "MF" [] Nothing ]
              )
            , ( "MSEmpty", [] )
            ]
        ]
      , Just (VCustom "MFEmpty" [])
      )
    , ( TCustom "Safe"
            []
            [ ( "Safe"
              , [ tInt, TNamed [] "Maybe" [ TNamed [] "Safe" [] Nothing ] Nothing ]
              )
            ]
      , [ TCustom "Maybe" [ "a" ] [ ( "Just", [ TVar "a" ] ), ( "Nothing", [] ) ] ]
      , Just (VCustom "Safe" [ VInt 0, VCustom "Nothing" [] ])
      )
    , ( TCustom "Unsafe"
            []
            [ ( "Unsafe"
              , [ tInt, TNamed [] "Identity" [ TNamed [] "Unsafe" [] Nothing ] Nothing ]
              )
            ]
      , [ TCustom "Identity" [ "a" ] [ ( "Identity", [ TVar "a" ] ) ] ]
      , Nothing
      )
    , ( TCustom "Unsafe"
            [ "a" ]
            [ ( "Unsafe"
              , [ tInt, TNamed [] "Unsafe" [ TTuple tInt (TNamed [] "Unsafe" [] Nothing) ] Nothing ]
              )
            ]
      , []
      , Nothing
      )
    ]


tryBuild : ( Type, List Type, Maybe Value ) -> Test
tryBuild ( type_, others, expected ) =
    test (Debug.toString type_) <|
        \_ ->
            case Fusion.Value.typeToDefault (toTypeDict (type_ :: others)) type_ of
                Err e ->
                    case expected of
                        Just _ ->
                            Expect.fail <| "Could not build: " ++ e

                        Nothing ->
                            Expect.pass

                Ok actual ->
                    Just actual
                        |> Expect.equal expected


toTypeDict : List Type -> Dict (List String) (Dict String ( Type, List String ))
toTypeDict types =
    types
        |> List.foldl
            (\type_ acc ->
                case type_ of
                    TCustom name vars _ ->
                        Dict.insert name ( type_, vars ) acc

                    _ ->
                        acc
            )
            Dict.empty
        |> Dict.singleton []
