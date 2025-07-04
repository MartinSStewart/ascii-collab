module Main exposing (main)

import Browser
import Fusion exposing (Value)
import Fusion.Editor
import Fusion.Generated.Result
import Fusion.Generated.TypeDict
import Fusion.Generated.TypeDict.Types
import Fusion.Patch
import Html exposing (Html)
import Types exposing (FrontendMsg(..))


main : Program () Value FrontendMsg
main =
    Browser.sandbox
        { view = view
        , update = update
        , init = init
        }


update : FrontendMsg -> Value -> Value
update msg model =
    case msg of
        Nop ->
            model

        Patch p ->
            model
                |> Fusion.Patch.patch { force = False } p
                |> Result.withDefault model


init : Value
init =
    Ok "Hello"
        |> Fusion.Generated.Result.toValue_Result
            Fusion.Patch.patcher_Never
            Fusion.Patch.patcher_String


view : Value -> Html FrontendMsg
view model =
    Fusion.Editor.value
        { typeDict = Fusion.Generated.TypeDict.typeDict
        , type_ = Just Fusion.Generated.TypeDict.Types.type_FrontendModel
        , editMsg = Patch
        , queryMsg = \_ -> Nop
        }
        model
