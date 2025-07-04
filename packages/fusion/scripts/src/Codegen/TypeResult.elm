module Codegen.TypeResult exposing (TypeResult(..), combineMap, map, map2, map3, toResult)

import Codegen.ErrorWithLocation exposing (ErrorWithLocation)


type TypeResult x
    = TypeOk x
    | TypeFunction
    | TypeErr ErrorWithLocation


toResult : TypeResult a -> Result ErrorWithLocation (Maybe a)
toResult x =
    case x of
        TypeOk j ->
            Ok (Just j)

        TypeFunction ->
            Ok Nothing

        TypeErr e ->
            Err e


{-| Implementation is lightly adapted from elmcraft/core-extra
-}
combineMap : (a -> TypeResult b) -> List a -> TypeResult (List b)
combineMap f ls =
    combineMapHelp f ls []


{-| Implementation is lightly adapted from elmcraft/core-extra
-}
combineMapHelp : (a -> TypeResult b) -> List a -> List b -> TypeResult (List b)
combineMapHelp f list acc =
    case list of
        head :: tail ->
            case f head of
                TypeOk a ->
                    combineMapHelp f tail (a :: acc)

                TypeErr x ->
                    TypeErr x

                TypeFunction ->
                    TypeFunction

        [] ->
            TypeOk (List.reverse acc)


map : (a -> b) -> TypeResult a -> TypeResult b
map f x =
    case x of
        TypeOk j ->
            TypeOk (f j)

        TypeFunction ->
            TypeFunction

        TypeErr e ->
            TypeErr e


map2 : (a -> b -> r) -> TypeResult a -> TypeResult b -> TypeResult r
map2 f x y =
    case ( x, y ) of
        ( TypeOk xv, TypeOk yv ) ->
            TypeOk <| f xv yv

        ( TypeErr e, _ ) ->
            TypeErr e

        ( _, TypeErr e ) ->
            TypeErr e

        ( TypeFunction, _ ) ->
            TypeFunction

        ( _, TypeFunction ) ->
            TypeFunction


map3 : (a -> b -> c -> r) -> TypeResult a -> TypeResult b -> TypeResult c -> TypeResult r
map3 f x y z =
    case ( x, y, z ) of
        ( TypeOk xv, TypeOk yv, TypeOk zv ) ->
            TypeOk <| f xv yv zv

        ( TypeErr e, _, _ ) ->
            TypeErr e

        ( _, TypeErr e, _ ) ->
            TypeErr e

        ( _, _, TypeErr e ) ->
            TypeErr e

        ( TypeFunction, _, _ ) ->
            TypeFunction

        ( _, TypeFunction, _ ) ->
            TypeFunction

        ( _, _, TypeFunction ) ->
            TypeFunction
