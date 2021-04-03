module PackageBrowser.Element.Readme.Section exposing (..)

import Markdown.Block
import Regex


replaceDocs : String -> String
replaceDocs a =
    let
        regex : Regex.Regex
        regex =
            "\n@docs (.*)"
                |> Regex.fromString
                |> Maybe.withDefault Regex.never

        escape : String -> String
        escape b =
            b
                |> String.replace "&" "&amp;"
                |> String.replace "'" "&apos;"
                |> String.replace "\"" "&quot;"
                |> String.replace "<" "&lt;"
                |> String.replace ">" "&gt;"
    in
    a
        |> Regex.replace regex
            (\v ->
                v.submatches
                    |> List.head
                    |> Maybe.andThen identity
                    |> Maybe.withDefault ""
                    |> (\vv -> "\n<docs value=\"" ++ escape vv ++ "\"></docs>")
            )


blocksToSections : String -> List Markdown.Block.Block -> List ( String, List Markdown.Block.Block )
blocksToSections defaultTitle a =
    let
        fold : Markdown.Block.Block -> List ( String, List Markdown.Block.Block ) -> List ( String, List Markdown.Block.Block )
        fold b acc =
            case b of
                Markdown.Block.Heading _ c ->
                    ( Markdown.Block.extractInlineText c, [] ) :: acc

                _ ->
                    case acc of
                        [] ->
                            ( defaultTitle, [ b ] ) :: acc

                        ( title, c ) :: rest ->
                            ( title, b :: c ) :: rest
    in
    a
        |> List.foldl fold []
        |> List.map (Tuple.mapSecond List.reverse)
        |> List.reverse
