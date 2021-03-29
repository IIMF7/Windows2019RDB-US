module Database.Package.Readme exposing (..)

import Elm.Docs
import Elm.Module
import Elm.Module.NameDict as PackageNameDict


type alias Readme =
    { readme : String
    , modules : PackageNameDict.NameDict (List Elm.Docs.Block)
    }


fromReadmeAndDocs : String -> List Elm.Docs.Module -> Readme
fromReadmeAndDocs a b =
    { readme = a
    , modules =
        b
            |> List.filterMap
                (\v ->
                    v.name
                        |> Elm.Module.fromString
                        |> Maybe.map (\vv -> ( vv, Elm.Docs.toBlocks v ))
                )
            |> PackageNameDict.fromList
    }
