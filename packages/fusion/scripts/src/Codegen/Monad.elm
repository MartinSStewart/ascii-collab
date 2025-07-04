module Codegen.Monad exposing (Context, Monad, andThen, andThen2, combine, combineMap, errString, map, map2, map3, ok)

import Codegen.CodegenResult as CodegenResult exposing (CodegenResult(..))
import Codegen.Parser
import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)


type alias Context =
    { debug : Bool
    , modules : Dict ModuleName Codegen.Parser.ParsedFile
    , currentModule : ModuleName
    }


type alias Monad a =
    Context -> CodegenResult a


map : (a -> b) -> Monad a -> Monad b
map f a context =
    CodegenResult.map f (a context)


map2 : (a -> b -> r) -> Monad a -> Monad b -> Monad r
map2 f a b context =
    CodegenResult.map2 f (a context) (b context)


map3 : (a -> b -> c -> r) -> Monad a -> Monad b -> Monad c -> Monad r
map3 f a b c context =
    CodegenResult.map3 f (a context) (b context) (c context)


andThen : (a -> Monad b) -> Monad a -> Monad b
andThen f g context =
    CodegenResult.andThen (\v -> f v context) (g context)


andThen2 : (a -> b -> Monad r) -> Monad a -> Monad b -> Monad r
andThen2 f a b context =
    CodegenResult.andThen2 (\v w -> f v w context) (a context) (b context)


ok : a -> Monad a
ok v _ =
    CodegenOk v


combine : List (Monad a) -> Monad (List a)
combine ls context =
    CodegenResult.combineMap (\x -> x context) ls


combineMap : (a -> Monad b) -> List a -> Monad (List b)
combineMap f ls context =
    CodegenResult.combineMap (\x -> f x context) ls


errString : String -> Monad a
errString body _ =
    CodegenErr <| CodegenResult.errorFromString body
