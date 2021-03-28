module Elm.Package.NameDict exposing (..)

import Dict
import Elm.Package


type NameDict v
    = NameDict (Dict.Dict String v)


fromList : List ( Elm.Package.Name, v ) -> NameDict v
fromList a =
    a
        |> List.map (Tuple.mapFirst Elm.Package.toString)
        |> Dict.fromList
        |> NameDict


toList : NameDict v -> List ( Elm.Package.Name, v )
toList (NameDict a) =
    a
        |> Dict.toList
        |> List.filterMap (\( k, v ) -> Elm.Package.fromString k |> Maybe.map (\vv -> ( vv, v )))


member : Elm.Package.Name -> NameDict v -> Bool
member k (NameDict a) =
    a |> Dict.member (Elm.Package.toString k)


insert : Elm.Package.Name -> v -> NameDict v -> NameDict v
insert k v (NameDict a) =
    a |> Dict.insert (Elm.Package.toString k) v |> NameDict
