module Utils.Update exposing (..)

{-| To chain multiple update functions.
More information: <https://sporto.github.io/elm-patterns/architecture/update-return-pipeline.html>
-}


andThen : (model -> ( model, Cmd msg )) -> ( model, Cmd msg ) -> ( model, Cmd msg )
andThen fn ( model, cmd ) =
    fn model |> Tuple.mapSecond (\v -> Cmd.batch [ v, cmd ])
