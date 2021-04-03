module PackageBrowser.Element.Readme.Section exposing (..)

import Markdown.Block
import Markdown.Parser
import Regex


type alias Section =
    { name : String
    , items : List Item
    }


type Item
    = Markdown (List Markdown.Block.Block)
    | Member String



--


fromMarkdown : String -> String -> Result String (List Section)
fromMarkdown defaultTitle a =
    let
        mapItems : (List Item -> List Item) -> List Section -> List Section
        mapItems fn acc =
            case acc of
                [] ->
                    { name = defaultTitle, items = fn [] } :: acc

                b :: rest ->
                    { b | items = fn b.items } :: rest

        fold : Markdown.Block.Block -> List Section -> List Section
        fold b acc =
            case b of
                Markdown.Block.Heading _ c ->
                    { name = Markdown.Block.extractInlineText c
                    , items = []
                    }
                        :: acc

                Markdown.Block.HtmlBlock (Markdown.Block.HtmlElement "docs" (attr :: _) _) ->
                    case attr.name of
                        "value" ->
                            acc |> mapItems (\v -> Member attr.value :: v)

                        _ ->
                            acc

                _ ->
                    acc
                        |> mapItems
                            (\v ->
                                case v of
                                    (Markdown vv) :: items_ ->
                                        Markdown (b :: vv) :: items_

                                    _ ->
                                        Markdown [ b ] :: v
                            )

        reverse : List Section -> List Section
        reverse b =
            b
                |> List.map
                    (\v ->
                        { v
                            | items =
                                v.items
                                    |> List.map
                                        (\vv ->
                                            case vv of
                                                Markdown vvv ->
                                                    Markdown (List.reverse vvv)

                                                Member _ ->
                                                    vv
                                        )
                                    |> List.reverse
                        }
                    )
                |> List.reverse
    in
    a
        |> replaceDocs
        |> Markdown.Parser.parse
        |> Result.mapError (List.map Markdown.Parser.deadEndToString >> String.join "\n")
        |> Result.map (List.foldl fold [] >> reverse)


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
                    |> String.split ","
                    |> List.map String.trim
                    |> List.filter (String.isEmpty >> not)
                    |> List.map (\vv -> "\n<docs value=\"" ++ escape vv ++ "\"></docs>")
                    |> String.join ""
            )
