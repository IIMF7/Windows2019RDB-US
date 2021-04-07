module Database.Modules exposing (..)

import Database.Package as Package
import Elm.Module as Module
import Elm.Module.Decode
import Elm.Module.Encode
import Elm.Module.NameDict as NameDict
import Elm.Package as Package
import Elm.Package.Decode
import Elm.Package.Encode
import Json.Decode as Decode
import Json.Encode as Encode
import Utils.Json.Decode_ as Decode_
import Utils.Json.Encode_ as Encode_


type alias Modules =
    List ( Module.Name, List ( Package.Name, Module.Name ) )


type alias ModuleDict =
    NameDict.NameDict (List ( Package.Name, Module.Name ))


fromPackages : List Package.Package -> Modules
fromPackages a =
    let
        fold : ( Package.Name, Module.Name ) -> ModuleDict -> ModuleDict
        fold ( package, module_ ) acc =
            let
                key : Module.Name
                key =
                    if package |> Package.toString |> String.contains "/elm-review" then
                        Module.fromString ("Review.Rule." ++ Module.toString module_)
                            |> Maybe.withDefault module_

                    else
                        module_
            in
            acc
                |> NameDict.update
                    key
                    (\v ->
                        v |> Maybe.withDefault [] |> (::) ( package, module_ ) |> Just
                    )
    in
    a
        |> List.concatMap
            (\v ->
                v.exposed |> Package.exposedToList |> List.map (Tuple.pair v.name)
            )
        |> List.foldl fold NameDict.empty
        |> NameDict.toList
        |> List.map (Tuple.mapSecond List.reverse)
        |> List.sortBy (Tuple.first >> Module.toString >> String.toLower)


encode : Encode_.Encoder Modules
encode =
    Encode.list
        (Encode_.tuple Elm.Module.Encode.name
            (Encode.list (Encode_.tuple Elm.Package.Encode.name Elm.Module.Encode.name))
        )


decoder : Decode.Decoder Modules
decoder =
    Decode.list
        (Decode_.tuple Elm.Module.Decode.name
            (Decode.list (Decode_.tuple Elm.Package.Decode.name Elm.Module.Decode.name))
        )
