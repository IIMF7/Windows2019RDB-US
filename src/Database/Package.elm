module Database.Package exposing (..)

import Elm.Constraint as Constraint
import Elm.Module as Module
import Elm.Package as Package
import Elm.Project as Project
import Elm.Version as Version


type alias Package =
    { name : Package.Name
    , summary : String
    , exposed : Project.Exposed
    }


fromProject : Project.Project -> Maybe Package
fromProject a =
    case a of
        Project.Application _ ->
            Nothing

        Project.Package b ->
            if b.elm |> isCompatible then
                Just
                    { name = b.name
                    , summary = b.summary
                    , exposed = b.exposed
                    }

            else
                Nothing


isCompatible : Constraint.Constraint -> Bool
isCompatible a =
    Version.fromTuple ( 0, 19, 1 )
        |> Maybe.map (\v -> Constraint.check v a)
        |> Maybe.withDefault False


exposedToList : Project.Exposed -> List Module.Name
exposedToList a =
    case a of
        Project.ExposedList b ->
            b

        Project.ExposedDict b ->
            b |> List.concatMap Tuple.second


sorter : Package -> String
sorter a =
    case a.exposed of
        Project.ExposedList b ->
            b
                |> List.head
                |> Maybe.map Module.toString
                |> Maybe.withDefault ""

        Project.ExposedDict b ->
            b
                |> List.head
                |> Maybe.withDefault ( "", [] )
                |> Tuple.second
                |> List.head
                |> Maybe.map Module.toString
                |> Maybe.withDefault ""
