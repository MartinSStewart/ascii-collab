module Evergreen.V83.LocalModel exposing (..)

import Time


type LocalModel msg model
    = LocalModel
        { localMsgs : List ( Time.Posix, msg )
        , localModel : model
        , model : model
        }
