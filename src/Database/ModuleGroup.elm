module Database.ModuleGroup exposing (..)

import Database.Package
import Elm.Module as Module
import Elm.Module.NameDict as NameDict
import Elm.Package as Package
import Regex


type alias ModuleGroup =
    { name : Module.Name
    , modules : List ( Package.Name, Module.Name )
    }


type alias ModuleGroupDict =
    NameDict.NameDict (List ( Package.Name, Module.Name ))


fromPackages : List Database.Package.Package -> List ModuleGroup
fromPackages a =
    let
        fold : ( Package.Name, Module.Name ) -> ModuleGroupDict -> ModuleGroupDict
        fold ( package, module_ ) acc =
            acc
                |> NameDict.update
                    (parentModule module_)
                    (\v ->
                        v |> Maybe.withDefault [] |> (::) ( package, module_ ) |> Just
                    )
    in
    a
        |> List.concatMap
            (\v ->
                v.exposed |> Database.Package.exposedToList |> List.map (Tuple.pair v.name)
            )
        |> List.foldl fold NameDict.empty
        |> NameDict.toList
        |> List.map
            (\( v, vv ) ->
                { name = v
                , modules = vv |> List.sortBy (Tuple.second >> Module.toString >> String.toLower)
                }
            )
        |> List.sortBy (.name >> Module.toString >> String.toLower)


parentModule : Module.Name -> Module.Name
parentModule a =
    let
        elmReview : Regex.Regex
        elmReview =
            Regex.fromString "^No[A-Z]" |> Maybe.withDefault Regex.never
    in
    if a |> Module.toString |> Regex.contains elmReview then
        -- https://github.com/jfmengels/elm-review/issues/98
        Module.fromString "Review"
            |> Maybe.withDefault a

    else if a |> Module.toString |> String.toLower |> (==) "graphql" then
        -- https://github.com/dillonkearns/elm-graphql/issues/494
        Module.fromString "GraphQl"
            |> Maybe.withDefault a

    else if a |> Module.toString |> (==) "BinaryBase64" then
        -- https://github.com/newlandsvalley/elm-binary-base64/issues/10
        Module.fromString "Base64"
            |> Maybe.withDefault a

    else if a |> Module.toString |> String.startsWith "Vector" then
        Module.fromString "Vector"
            |> Maybe.withDefault a

    else
        a
            |> Module.toString
            |> String.split "."
            |> List.head
            |> Maybe.andThen Module.fromString
            |> Maybe.withDefault a
