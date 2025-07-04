module Fusion exposing (Value(..), Type(..), SpecialType(..), ModuleName, Query(..))

{-|

@docs Value, Type, SpecialType, ModuleName, Query

-}

import Bytes exposing (Bytes)
import Dict exposing (Dict)


{-| A generic Elm value.
-}
type Value
    = VInt Int
    | VFloat Float
    | VString String
    | VBool Bool
    | VChar Char
    | VUnit
    | VBytes Bytes
    | VTuple Value Value
    | VTriple Value Value Value
    | VList { cursor : Int, items : List Value }
    | VSet { cursor : Int, items : List Value }
    | VDict { cursor : Int, items : List ( Value, Value ) }
    | VCustom String (List Value)
    | VRecord (Dict String Value)
      -- Helpers
    | VUnloaded
    | VPartialString { length : Int, partial : String }


{-| A module's name.
-}
type alias ModuleName =
    List String


{-| Type level type.
-}
type Type
    = TNamed ModuleName String (List Type) (Maybe SpecialType)
    | TVar String
    | TUnit
    | TTuple Type Type
    | TTriple Type Type Type
    | TCustom String (List String) (List ( String, List Type ))
    | TRecord (List ( String, Type ))
    | TGenericRecord String (List ( String, Type ))


{-| Specially-handled types.
-}
type SpecialType
    = -- ## Default includes
      -- elm/core Basics
      TInt
    | TFloat
    | TOrder
    | TBool
    | TNever
      -- elm/core List
    | TList Type
      -- elm/core Maybe
    | TMaybe Type
      -- elm/core Result
    | TResult Type Type
      -- elm/core String
    | TString
      -- elm/core Char
    | TChar
      -- ## Other special types
      -- elm/core Set
    | TSet Type
      -- elm/core Dict
    | TDict Type Type
      -- elm/bytes Bytes
    | TBytes
      -- elm/json Json.Encode/Json.Decode
    | TJson



--   -- lamdera/containers SeqDict
-- | TSeqDict Type Type


{-| Queries to load more data.
-}
type Query
    = QLoad
    | QRecord String Query
      -- List or Set or Dict
    | QIndexed Value Query
