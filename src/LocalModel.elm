module LocalModel exposing (Config, LocalModel, init, localModel, update, updateFromBackend)

import List.Nonempty exposing (Nonempty)


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


updateFromBackend : Config msg model -> Nonempty msg -> LocalModel msg model -> LocalModel msg model
updateFromBackend config msgs (LocalModel localModel_) =
    let
        newModel =
            List.Nonempty.foldl config.update localModel_.model msgs

        newLocalMsgs =
            List.Nonempty.foldl
                (\serverMsg localMsgs ->
                    List.foldl
                        (\localMsg ( newList, isDone ) ->
                            if isDone then
                                ( localMsg :: newList, True )

                            else if config.msgEqual localMsg serverMsg then
                                ( newList, True )

                            else
                                ( localMsg :: newList, False )
                        )
                        ( [], False )
                        localMsgs
                        |> Tuple.first
                        |> List.reverse
                )
                localModel_.localMsgs
                msgs
    in
    LocalModel
        { localMsgs = newLocalMsgs
        , localModel = List.foldl config.update newModel newLocalMsgs
        , model = newModel
        }
