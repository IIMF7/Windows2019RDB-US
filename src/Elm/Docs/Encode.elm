module Elm.Docs.Encode exposing (..)

{-| Generated by elm-json-interop.
-}

import Elm.Docs as A
import Elm.Type.Encode
import Json.Encode as E
import Utils.Json.Encode_ as E_ exposing (Encoder)


module_ : Encoder A.Module
module_ =
    \v1 ->
        E.object
            [ ( "name"
              , E.string v1.name
              )
            , ( "comment"
              , E.string v1.comment
              )
            , ( "unions"
              , E.list union v1.unions
              )
            , ( "aliases"
              , E.list alias v1.aliases
              )
            , ( "values"
              , E.list value v1.values
              )
            , ( "binops"
              , E.list binop v1.binops
              )
            ]


alias : Encoder A.Alias
alias =
    \v1 ->
        E.object
            [ ( "name"
              , E.string v1.name
              )
            , ( "comment"
              , E.string v1.comment
              )
            , ( "args"
              , E.list E.string v1.args
              )
            , ( "tipe"
              , Elm.Type.Encode.type_ v1.tipe
              )
            ]


union : Encoder A.Union
union =
    \v1 ->
        E.object
            [ ( "name"
              , E.string v1.name
              )
            , ( "comment"
              , E.string v1.comment
              )
            , ( "args"
              , E.list E.string v1.args
              )
            , ( "tags"
              , E.list (E_.tuple E.string (E.list Elm.Type.Encode.type_)) v1.tags
              )
            ]


value : Encoder A.Value
value =
    \v1 ->
        E.object
            [ ( "name"
              , E.string v1.name
              )
            , ( "comment"
              , E.string v1.comment
              )
            , ( "tipe"
              , Elm.Type.Encode.type_ v1.tipe
              )
            ]


binop : Encoder A.Binop
binop =
    \v1 ->
        E.object
            [ ( "name"
              , E.string v1.name
              )
            , ( "comment"
              , E.string v1.comment
              )
            , ( "tipe"
              , Elm.Type.Encode.type_ v1.tipe
              )
            , ( "associativity"
              , associativity v1.associativity
              )
            , ( "precedence"
              , E.int v1.precedence
              )
            ]


associativity : Encoder A.Associativity
associativity =
    \v1 ->
        case v1 of
            A.Left ->
                E.object [ ( "_", E.int 0 ) ]

            A.None ->
                E.object [ ( "_", E.int 1 ) ]

            A.Right ->
                E.object [ ( "_", E.int 2 ) ]


block : Encoder A.Block
block =
    \v1 ->
        case v1 of
            A.MarkdownBlock v2 ->
                E.object [ ( "_", E.int 0 ), ( "a", E.string v2 ) ]

            A.UnionBlock v2 ->
                E.object [ ( "_", E.int 1 ), ( "a", union v2 ) ]

            A.AliasBlock v2 ->
                E.object [ ( "_", E.int 2 ), ( "a", alias v2 ) ]

            A.ValueBlock v2 ->
                E.object [ ( "_", E.int 3 ), ( "a", value v2 ) ]

            A.BinopBlock v2 ->
                E.object [ ( "_", E.int 4 ), ( "a", binop v2 ) ]

            A.UnknownBlock v2 ->
                E.object [ ( "_", E.int 5 ), ( "a", E.string v2 ) ]
