module Elm.Module.NameDict exposing (..)

import Dict
import Elm.Module


type NameDict v
    = NameDict (Dict.Dict String v)


fromList : List ( Elm.Module.Name, v ) -> NameDict v
fromList a =
    a
        |> List.map (Tuple.mapFirst Elm.Module.toString)
        |> Dict.fromList
        |> NameDict


empty : NameDict v
empty =
    fromList []


toList : NameDict v -> List ( Elm.Module.Name, v )
toList (NameDict a) =
    a
        |> Dict.toList
        |> List.filterMap (\( k, v ) -> Elm.Module.fromString k |> Maybe.map (\vv -> ( vv, v )))


member : Elm.Module.Name -> NameDict v -> Bool
member k (NameDict a) =
    a |> Dict.member (Elm.Module.toString k)


get : Elm.Module.Name -> NameDict v -> Maybe v
get k (NameDict a) =
    a |> Dict.get (Elm.Module.toString k)


insert : Elm.Module.Name -> v -> NameDict v -> NameDict v
insert k v (NameDict a) =
    a |> Dict.insert (Elm.Module.toString k) v |> NameDict


update : Elm.Module.Name -> (Maybe v -> Maybe v) -> NameDict v -> NameDict v
update k fn (NameDict a) =
    a |> Dict.update (Elm.Module.toString k) fn |> NameDict
