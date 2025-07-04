module Fusion.Random exposing (build_Seed, patch_Seed, patcher_Seed, toValue_Seed)

{-| -}

import Fusion exposing (Value(..))
import Fusion.Patch exposing (Error(..), Patch, Patcher)
import Random exposing (Seed)


patcher_Seed : Patcher Seed
patcher_Seed =
    { patch = patch_Seed
    , build = build_Seed
    , toValue = toValue_Seed
    }


{-| -}
patch_Seed : { force : Bool } -> Patch -> Seed -> Result Error Seed
patch_Seed _ _ found =
    Ok found


{-| -}
build_Seed : Value -> Result Error Seed
build_Seed _ =
    Err <| WrongType "Seed"


{-| -}
toValue_Seed : Seed -> Value
toValue_Seed _ =
    VUnloaded
