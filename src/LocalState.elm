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
update config msg (LocalModel localModel) =
    LocalModel
        { localMsgs = localModel.localMsgs ++ [ msg ]
        , localModel = config.update msg localModel.localModel
        , model = localModel.model
        }


updateFromBackend : Config msg model -> msg -> LocalModel msg model -> LocalModel msg model
updateFromBackend config msg (LocalModel localModel) =
    let
        newModel =
            config.update msg localModel.model

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
                localModel.localMsgs
                |> Tuple.first
                |> List.reverse
    in
    LocalModel
        { localMsgs = newLocalMsgs
        , localModel = List.foldl config.update newModel newLocalMsgs
        , model = newModel
        }
