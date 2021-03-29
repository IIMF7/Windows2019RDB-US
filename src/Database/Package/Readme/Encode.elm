module Database.Package.Readme.Encode exposing (..)

{-| Generated by elm-json-interop.
-}

import Database.Package.Readme as A
import Elm.Docs.Encode
import Elm.Module.NameDict.Encode
import Json.Encode as E
import Utils.Json.Encode_ as E_ exposing (Encoder)


readme : Encoder A.Readme
readme =
    \v1 ->
        E.object
            [ ( "readme"
              , E.string v1.readme
              )
            , ( "modules"
              , Elm.Module.NameDict.Encode.nameDict (E.list Elm.Docs.Encode.block) v1.modules
              )
            ]
