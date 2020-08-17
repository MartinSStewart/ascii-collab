module LocalState exposing (Config, LocalModel, init, update, updateFromBackend)


type LocalModel msg model
    = LocalModel { localMsgs : List msg, localModel : model, model : model }


type alias Config msg model =
    { msgEqual : msg -> msg -> Bool
    , update : msg -> model -> model
    }


init : model -> LocalModel msg model
init model =
    LocalModel { localMsgs = [], localModel = model, model = model }


update : Config msg model -> msg -> LocalModel msg model -> LocalModel msg model
update config msg (LocalModel localModel_) =
    LocalModel
        { localMsgs = localModel_.localMsgs ++ [ msg ]
        , localModel = config.update msg localModel_.localModel
        , model = localModel_.model
        }


localModel : LocalModel msg model -> model
localModel (LocalModel localModel_) =
    localModel_.localModel


updateFromBackend : Config msg model -> msg -> LocalModel msg model -> LocalModel msg model
updateFromBackend config msg (LocalModel localModel_) =
    let
        newModel =
            config.update msg localModel_.model

        newLocalMsgs =
            List.foldl
                (\localMsg ( newList, isDone ) ->
                    if isDone then
                        ( localMsg :: newList, True )

                    else if config.msgEqual localMsg msg then
                        ( newList, True )

                    else
                        ( localMsg :: newList, False )
                )
                ( [], False )
                localModel_.localMsgs
                |> Tuple.first
                |> List.reverse
    in
    LocalModel
        { localMsgs = newLocalMsgs
        , localModel = List.foldl config.update newModel newLocalMsgs
        , model = newModel
        }
