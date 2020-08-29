module Evergreen.V4.LocalModel exposing (..)

type LocalModel msg model
    = LocalModel 
    { localMsgs : (List msg)
    , localModel : model
    , model : model
    }