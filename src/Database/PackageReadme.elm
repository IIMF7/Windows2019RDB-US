module Database.PackageReadme exposing (..)

import Dict exposing (Dict)
import Elm.Docs
import Elm.Module
import Elm.Module.NameDict as PackageNameDict


type alias PackageReadme =
    { readme : String
    , modules : PackageNameDict.NameDict ModuleReadme
    }


type alias ModuleReadme =
    { readme : String
    , unions : Dict String Elm.Docs.Union
    , aliases : Dict String Elm.Docs.Alias
    , values : Dict String Elm.Docs.Value
    , binops : Dict String Elm.Docs.Binop
    }


fromReadmeAndDocs : String -> List Elm.Docs.Module -> PackageReadme
fromReadmeAndDocs a b =
    let
        toDict : List { a | name : String } -> Dict String { a | name : String }
        toDict c =
            c |> List.map (\v -> ( v.name, v )) |> Dict.fromList

        modules : PackageNameDict.NameDict ModuleReadme
        modules =
            b
                |> List.filterMap
                    (\v ->
                        v.name
                            |> Elm.Module.fromString
                            |> Maybe.map
                                (\vv ->
                                    ( vv
                                    , { readme = v.comment
                                      , unions = v.unions |> toDict
                                      , aliases = v.aliases |> toDict
                                      , values = v.values |> toDict
                                      , binops = v.binops |> toDict
                                      }
                                    )
                                )
                    )
                |> PackageNameDict.fromList
    in
    { readme = a
    , modules = modules
    }
