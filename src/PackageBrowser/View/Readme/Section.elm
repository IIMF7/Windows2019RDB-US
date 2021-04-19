module PackageBrowser.View.Readme.Section exposing (..)

import Database.PackageReadme
import Dict
import Elm.Docs
import Elm.Type
import Markdown.Block
import Markdown.Parser
import Regex


type alias Section =
    { name : String
    , items : List Item
    }


type Item
    = MarkdownItem (List Markdown.Block.Block)
    | MemberItem Member


type alias Member =
    { name : String
    , type_ : String
    , comment : String
    }



--


fromMarkdown : Database.PackageReadme.ModuleReadme -> String -> String -> Result String (List Section)
fromMarkdown module_ defaultTitle a =
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
                            case attr.value |> toMember module_ of
                                Just c ->
                                    acc |> mapItems (\v -> MemberItem c :: v)

                                Nothing ->
                                    acc

                        _ ->
                            acc

                _ ->
                    acc
                        |> mapItems
                            (\v ->
                                case v of
                                    (MarkdownItem vv) :: items_ ->
                                        MarkdownItem (b :: vv) :: items_

                                    _ ->
                                        MarkdownItem [ b ] :: v
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
                                                MarkdownItem vvv ->
                                                    MarkdownItem (List.reverse vvv)

                                                MemberItem _ ->
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



--


toMember : Database.PackageReadme.ModuleReadme -> String -> Maybe Member
toMember module_ a =
    Nothing
        |> onNothing (\_ -> module_.unions |> Dict.get a |> Maybe.map unionToMember)
        |> onNothing (\_ -> module_.aliases |> Dict.get a |> Maybe.map aliasToMember)
        |> onNothing (\_ -> module_.values |> Dict.get a |> Maybe.map valueToMember)
        |> onNothing (\_ -> module_.binops |> Dict.get (a |> String.dropLeft 1 |> String.dropRight 1) |> Maybe.map binopToMember)


unionToMember : Elm.Docs.Union -> Member
unionToMember a =
    let
        type_ : String
        type_ =
            (if a.args == [] then
                []

             else
                "" :: a.args
            )
                ++ (if a.tags == [] then
                        []

                    else
                        ""
                            :: "="
                            :: [ a.tags
                                    |> List.map
                                        (\( vv, vvv ) ->
                                            vv
                                                :: (vvv
                                                        |> List.map (typeToString >> maybeWrapInParents)
                                                   )
                                                |> String.join " "
                                        )
                                    |> String.join " | "
                               ]
                   )
                |> String.join " "
    in
    { name = a.name
    , type_ = type_
    , comment = a.comment
    }


aliasToMember : Elm.Docs.Alias -> Member
aliasToMember a =
    { name = a.name
    , type_ = "" :: a.args ++ [ "=" ] ++ typeToString a.tipe |> String.join " "
    , comment = a.comment
    }


valueToMember : Elm.Docs.Value -> Member
valueToMember a =
    { name = a.name
    , type_ = "" :: ":" :: typeToString a.tipe |> String.join " "
    , comment = a.comment
    }


binopToMember : Elm.Docs.Binop -> Member
binopToMember a =
    { name = "(" ++ a.name ++ ")"
    , type_ = "" :: ":" :: typeToString a.tipe |> String.join " "
    , comment = a.comment
    }



--


typeToString : Elm.Type.Type -> List String
typeToString a =
    case a of
        Elm.Type.Var b ->
            [ b
            ]

        Elm.Type.Lambda b c ->
            [ b
                |> typeToString
                |> (\v ->
                        if v |> List.any ((==) "->") then
                            "(" ++ String.join " " v ++ ")"

                        else
                            v |> String.join " "
                   )
            , "->"
            , c |> typeToString |> String.join " "
            ]

        Elm.Type.Tuple b ->
            if b == [] then
                [ "()"
                ]

            else
                [ "("
                , b |> List.map (typeToString >> String.join " ") |> String.join ", "
                , ")"
                ]
                    |> String.join " "
                    |> List.singleton

        Elm.Type.Type b c ->
            let
                name : String
                name =
                    b |> String.split "." |> List.reverse |> List.head |> Maybe.withDefault ""
            in
            if c == [] then
                [ name
                ]

            else
                [ name
                , c
                    |> List.map (typeToString >> maybeWrapInParents)
                    |> String.join " "
                ]

        Elm.Type.Record b c ->
            let
                open : String
                open =
                    case c of
                        Just d ->
                            "{ " ++ d ++ " |"

                        Nothing ->
                            "{"
            in
            [ open
            , b
                |> List.map (\( v, vv ) -> v :: ":" :: typeToString vv |> String.join " ")
                |> String.join ", "
            , "}"
            ]
                |> String.join " "
                |> List.singleton


maybeWrapInParents : List String -> String
maybeWrapInParents a =
    if List.length a > 1 then
        "(" ++ String.join " " a ++ ")"

    else
        a |> String.join " "



--


onNothing : (() -> Maybe a) -> Maybe a -> Maybe a
onNothing fn b =
    case b of
        Just c ->
            Just c

        Nothing ->
            fn ()
