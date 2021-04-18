module Utils.Update exposing (..)

{-| To chain multiple update functions.
More information: <https://sporto.github.io/elm-patterns/architecture/update-return-pipeline.html>
-}


andThen : (model -> ( model, Cmd msg )) -> ( model, Cmd msg ) -> ( model, Cmd msg )
andThen fn ( model, cmd ) =
    fn model |> Tuple.mapSecond (\v -> Cmd.batch [ v, cmd ])


multiple : List msg -> (msg -> model -> ( model, Cmd msg )) -> model -> ( model, Cmd msg )
multiple messages fn initialModal =
    let
        fold : msg -> ( model, Cmd msg ) -> ( model, Cmd msg )
        fold msg ( model, cmd ) =
            let
                ( nextModel, nextCmd ) =
                    fn msg model
            in
            ( nextModel
            , Cmd.batch [ nextCmd, cmd ]
            )
    in
    messages |> List.foldl fold ( initialModal, Cmd.none )
