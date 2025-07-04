module Types exposing (BackendModel, BackendMsg, FrontendModel, FrontendMsg(..), ToBackend, ToFrontend)

import Fusion.Patch


type FrontendMsg
    = Nop
    | Patch Fusion.Patch.Patch


type alias FrontendModel =
    Result Never String


type alias BackendModel =
    ()


type alias ToBackend =
    ()


type alias BackendMsg =
    ()


type alias ToFrontend =
    ()
