module Database.Package.Readme exposing (..)

import Dict exposing (Dict)
import Elm.Docs


type alias Readme =
    { readme : String
    , modules : Dict String (List Elm.Docs.Block)
    }


fromReadmeAndDocs : String -> List Elm.Docs.Module -> Readme
fromReadmeAndDocs a b =
    { readme = a
    , modules =
        b
            |> List.map (\v -> ( v.name, Elm.Docs.toBlocks v ))
            |> Dict.fromList
    }
