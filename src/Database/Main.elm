port module Database.Main exposing (..)

import Database.Modules as Modules
import Database.Package as Package
import Database.Package.Encode
import Database.Package.Readme as Readme
import Database.Package.Readme.Encode
import Elm.Docs
import Elm.Package
import Elm.Project
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Task exposing (Task)
import Utils.Resolver as Resolver


main : Program Decode.Value Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }


port output : Output -> Cmd msg


port exit : String -> Cmd msg



--


type alias Model =
    ()


type alias Output =
    List ( String, Decode.Value )


type Error
    = GetNamesError Http.Error
    | GetProjectError Elm.Package.Name Http.Error
    | GetReadmeError Elm.Package.Name Http.Error
    | GetModulesError Elm.Package.Name Http.Error


init : Decode.Value -> ( Model, Cmd Msg )
init _ =
    let
        task : Task Error Output
        task =
            getNames
                |> Task.mapError GetNamesError
                |> Task.andThen (List.map packageTask >> Task.sequence)
                |> Task.map (List.filterMap identity >> List.unzip)
                |> Task.map
                    (\( v, vv ) ->
                        ( "packages"
                        , v
                            |> List.sortBy Package.sorter
                            |> Encode.list Database.Package.Encode.package
                        )
                            :: ( "modules"
                               , v
                                    |> Modules.fromPackages
                                    |> Modules.encode
                               )
                            :: (vv |> List.map (Tuple.mapBoth Elm.Package.toString Database.Package.Readme.Encode.readme))
                    )

        packageTask : Elm.Package.Name -> Task Error (Maybe ( Package.Package, ( Elm.Package.Name, Readme.Readme ) ))
        packageTask name =
            getProject name
                |> Task.mapError (GetProjectError name)
                |> Task.andThen
                    (\project ->
                        let
                            _ =
                                name |> Debug.log "log"
                        in
                        case project |> Package.fromProject of
                            Just package ->
                                Task.map2
                                    (\v vv ->
                                        Just
                                            ( package
                                            , ( name, Readme.fromReadmeAndDocs v vv )
                                            )
                                    )
                                    (name |> getReadme |> Task.mapError (GetReadmeError name))
                                    (name |> getModules |> Task.mapError (GetModulesError name))

                            Nothing ->
                                Task.succeed Nothing
                    )
    in
    ( ()
    , task |> Task.attempt identity
    )



--


type alias Msg =
    Result Error Output


update : Msg -> Model -> ( Model, Cmd msg )
update msg _ =
    (case msg of
        Ok a ->
            output a

        Err a ->
            case a of
                GetNamesError _ ->
                    exit "Get names failed."

                GetProjectError _ _ ->
                    exit "Get project failed."

                GetReadmeError _ _ ->
                    exit "Get readme failed."

                GetModulesError _ _ ->
                    exit "Get modules failed."
    )
        |> Tuple.pair ()



--


getNames : Task Http.Error (List Elm.Package.Name)
getNames =
    let
        decoder : Decoder (List Elm.Package.Name)
        decoder =
            Decode.keyValuePairs Decode.value
                |> Decode.map (List.filterMap (Tuple.first >> Elm.Package.fromString))
    in
    Http.task
        { method = "GET"
        , headers = []
        , url = "https://package.elm-lang.org/all-packages"
        , body = Http.emptyBody
        , resolver = Resolver.json decoder
        , timeout = Nothing
        }


getProject : Elm.Package.Name -> Task Http.Error Elm.Project.Project
getProject a =
    Http.task
        { method = "GET"
        , headers = []
        , url = "https://package.elm-lang.org/packages/" ++ Elm.Package.toString a ++ "/latest/elm.json"
        , body = Http.emptyBody
        , resolver = Resolver.json Elm.Project.decoder
        , timeout = Nothing
        }


getReadme : Elm.Package.Name -> Task Http.Error String
getReadme a =
    Http.task
        { method = "GET"
        , headers = []
        , url = "https://package.elm-lang.org/packages/" ++ Elm.Package.toString a ++ "/latest/README.md"
        , body = Http.emptyBody
        , resolver = Resolver.string
        , timeout = Nothing
        }


getModules : Elm.Package.Name -> Task Http.Error (List Elm.Docs.Module)
getModules a =
    Http.task
        { method = "GET"
        , headers = []
        , url = "https://package.elm-lang.org/packages/" ++ Elm.Package.toString a ++ "/latest/docs.json"
        , body = Http.emptyBody
        , resolver = Resolver.json (Decode.list Elm.Docs.decoder)
        , timeout = Nothing
        }
