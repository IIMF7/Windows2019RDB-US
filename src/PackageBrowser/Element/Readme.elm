module PackageBrowser.Element.Readme exposing (..)

import Database.Package.Readme as Readme
import Database.Package.Readme.Decode
import Dict exposing (Dict)
import Element
import Element.Events exposing (onClick)
import Elm.Docs
import Elm.Module
import Elm.Module.NameDict as ModuleNameDict
import Elm.Package
import Elm.Package.NameDict as PackageNameDict
import Elm.Type
import Http
import Markdown.Parser
import Markdown.Renderer
import PackageBrowser.Element.Readme.Section as Section
import PackageBrowser.Router as Router
import PackageBrowser.Strings as Strings
import PackageBrowser.Ui exposing (..)
import PackageBrowser.Ui.Markdown as Markdown


type alias Context a b =
    { a
        | router :
            { b
                | view : Router.View
            }
    }



--


type alias Model =
    { readmes : PackageNameDict.NameDict (Result Error Readme.Readme)
    }


type Error
    = Loading
    | HttpError Http.Error


init : ( Model, Cmd Msg )
init =
    ( { readmes = PackageNameDict.empty
      }
    , Cmd.none
    )


getPackage : Elm.Package.Name -> Cmd Msg
getPackage a =
    Http.get
        { url = "db/" ++ (a |> Elm.Package.toString |> String.replace "/" " ") ++ ".json"
        , expect = Http.expectJson (GotReadme a) Database.Package.Readme.Decode.readme
        }



--


type Msg
    = ViewChanged
    | GotReadme Elm.Package.Name (Result Http.Error Readme.Readme)
    | Reveal Elm.Package.Name


update : Context a b -> Msg -> Model -> ( Model, Cmd Msg )
update ctx msg model =
    case msg of
        ViewChanged ->
            case ctx.router.view |> Router.viewToPackageName of
                Just b ->
                    case model.readmes |> PackageNameDict.get b of
                        Just _ ->
                            ( model
                            , Cmd.none
                            )

                        Nothing ->
                            ( { model | readmes = model.readmes |> PackageNameDict.insert b (Err Loading) }
                            , getPackage b
                            )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        Reveal _ ->
            ( model
            , Cmd.none
            )

        GotReadme a b ->
            ( { model | readmes = model.readmes |> PackageNameDict.insert a (b |> Result.mapError HttpError) }
            , Cmd.none
            )



--


view : Router.View -> Model -> Element Msg
view view_ model =
    column [ width fill, height fill ]
        (case view_ of
            Router.DefaultView ->
                []

            Router.PackageView b ->
                [ viewPackageHeader b
                , viewPackageReadme (PackageNameDict.get b model.readmes)
                ]

            Router.ModuleView b c ->
                [ viewPackageHeader b
                , viewModuleHeader b c
                , viewModuleReadme b c (PackageNameDict.get b model.readmes)
                ]
        )


viewPackageHeader : Elm.Package.Name -> Element Msg
viewPackageHeader a =
    row
        [ width fill
        , spacing 1
        , paddingXY 1 0.75
        , borderColor gray3
        , borderWidthEach 0 0 0 1
        ]
        [ h4 []
            [ link [ fontColor gray9, onClick (Reveal a) ]
                { label = text (Elm.Package.toString a)
                , url = Router.PackageView a |> Router.viewToUrl
                }
            ]
        , newTabLink []
            { label = text Strings.officialDocs
            , url = "https://package.elm-lang.org/packages/" ++ Elm.Package.toString a ++ "/latest/"
            }
        , newTabLink []
            { label = text Strings.source
            , url = "https://github.com/" ++ Elm.Package.toString a
            }
        ]


viewModuleHeader : Elm.Package.Name -> Elm.Module.Name -> Element Msg
viewModuleHeader a b =
    row
        [ width fill
        , spacing 1
        , paddingXY 1 0.75
        , borderColor gray3
        , borderWidthEach 0 0 0 1
        ]
        [ h5 []
            [ link [ fontColor gray9, onClick (Reveal a) ]
                { label = text (Elm.Module.toString b)
                , url = Router.ModuleView a b |> Router.viewToUrl
                }
            ]
        , newTabLink []
            { label = text Strings.officialDocs
            , url = "https://package.elm-lang.org/packages/" ++ Elm.Package.toString a ++ "/latest/" ++ (Elm.Module.toString b |> String.replace "." "-")
            }
        , newTabLink []
            { label = text Strings.source
            , url = "https://github.com/" ++ Elm.Package.toString a ++ "/blob/master/src/" ++ (Elm.Module.toString b |> String.replace "." "/") ++ ".elm"
            }
        ]


viewPackageReadme : Maybe (Result Error Readme.Readme) -> Element msg
viewPackageReadme a =
    let
        view_ : Readme.Readme -> Element msg
        view_ b =
            column
                [ width fill
                , spacing 1
                ]
                (b.readme
                    |> Markdown.Parser.parse
                    |> Result.toMaybe
                    |> Maybe.andThen (Markdown.Renderer.render Markdown.renderer >> Result.toMaybe)
                    |> Maybe.withDefault
                        [ status []
                            [ text Strings.readmeIsNotAvailable
                            ]
                        ]
                )
    in
    column
        [ width fill
        , height fill
        , paddingEach 2 2 1 4
        , Element.scrollbars
        ]
        [ viewLoading view_ a
        ]



--


viewModuleReadme : Elm.Package.Name -> Elm.Module.Name -> Maybe (Result Error Readme.Readme) -> Element Msg
viewModuleReadme _ b c =
    let
        view_ : Readme.Readme -> Element Msg
        view_ d =
            case d.modules |> ModuleNameDict.get b of
                Just e ->
                    let
                        defaultTitle : String
                        defaultTitle =
                            Elm.Module.toString b
                                |> String.split "."
                                |> List.reverse
                                |> List.head
                                |> Maybe.withDefault ""
                    in
                    case e.readme |> Section.fromMarkdown defaultTitle of
                        Ok f ->
                            column [ width fill, spacing 1.5 ]
                                (f
                                    |> List.map
                                        (\v ->
                                            column [ width fill, spacing 0.5 ]
                                                [ p [ fontWeight 7 ]
                                                    [ text v.name
                                                    ]
                                                , viewItems e v.items
                                                ]
                                        )
                                )

                        Err _ ->
                            status []
                                [ text Strings.readmeIsNotAvailable
                                ]

                Nothing ->
                    status []
                        [ text Strings.moduleNotFound
                        ]
    in
    column
        [ width fill
        , height fill
        , paddingEach 1 1 1 4
        , Element.scrollbars
        ]
        [ viewLoading view_ c
        ]


viewItems : Readme.ModuleReadme -> List Section.Item -> Element msg
viewItems module_ a =
    let
        viewItem : Section.Item -> Element msg
        viewItem b =
            case b of
                Section.Markdown c ->
                    column [ width fill, paddingXY 3 0 ]
                        [ column
                            [ width fill
                            , padding 1
                            , spacing 1
                            , fontSize 0.9375
                            , backgroundColor gray1
                            , borderRounded 0.25
                            ]
                            (c
                                |> Markdown.Renderer.render Markdown.renderer
                                |> Result.withDefault
                                    [ status []
                                        [ text Strings.readmeIsNotAvailable
                                        ]
                                    ]
                            )
                        ]

                Section.Member c ->
                    c
                        |> toMember
                        |> Maybe.map viewMember
                        |> Maybe.withDefault none

        toMember : String -> Maybe { name : String, type_ : String, comment : String }
        toMember b =
            Nothing
                |> onNothing (\_ -> module_.unions |> Dict.get b |> Maybe.map viewUnion)
                |> onNothing (\_ -> module_.aliases |> Dict.get b |> Maybe.map viewAlias)
                |> onNothing (\_ -> module_.values |> Dict.get b |> Maybe.map viewValue)
                |> onNothing (\_ -> module_.binops |> Dict.get (b |> String.dropLeft 1 |> String.dropRight 1) |> Maybe.map viewBinop)
    in
    column [ width fill, spacing 1 ]
        (a
            |> List.map viewItem
        )


viewMember : { name : String, type_ : String, comment : String } -> Element msg
viewMember a =
    column [ width fill, spacing 0.5, paddingXY 1.5 0 ]
        [ row [ width fill ]
            [ text a.name
            , el [ fontColor gray5 ] (text a.type_)
            ]
        , column [ width fill, paddingXY 1.5 0 ]
            [ column
                [ width fill
                , padding 1
                , spacing 1
                , fontSize 0.9375
                , backgroundColor gray1
                , borderRounded 0.25
                ]
                (a.comment
                    |> Markdown.Parser.parse
                    |> Result.toMaybe
                    |> Maybe.andThen (Markdown.Renderer.render Markdown.renderer >> Result.toMaybe)
                    |> Maybe.withDefault
                        [ status []
                            [ text Strings.readmeIsNotAvailable
                            ]
                        ]
                )
            ]
        ]



--


viewUnion : Elm.Docs.Union -> { name : String, type_ : String, comment : String }
viewUnion a =
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


viewAlias : Elm.Docs.Alias -> { name : String, type_ : String, comment : String }
viewAlias a =
    { name = a.name
    , type_ = "" :: a.args ++ [ "=" ] ++ typeToString a.tipe |> String.join " "
    , comment = a.comment
    }


viewValue : Elm.Docs.Value -> { name : String, type_ : String, comment : String }
viewValue a =
    { name = a.name
    , type_ = "" :: ":" :: typeToString a.tipe |> String.join " "
    , comment = a.comment
    }


viewBinop : Elm.Docs.Binop -> { name : String, type_ : String, comment : String }
viewBinop a =
    { name = "(" ++ a.name ++ ")"
    , type_ = "" :: ":" :: typeToString a.tipe |> String.join " "
    , comment = a.comment
    }


typeToString : Elm.Type.Type -> List String
typeToString a =
    case a of
        Elm.Type.Var b ->
            [ b
            ]

        Elm.Type.Lambda b c ->
            [ b |> typeToString |> maybeWrapInParents
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


viewLoading : (a -> Element msg) -> Maybe (Result Error a) -> Element msg
viewLoading fn a =
    case a of
        Just b ->
            case b of
                Ok c ->
                    fn c

                Err c ->
                    case c of
                        Loading ->
                            status []
                                [ text Strings.loading
                                ]

                        HttpError d ->
                            status []
                                [ text (Strings.httpError d)
                                ]

        Nothing ->
            status []
                [ text Strings.packageNotFound
                ]



--


onNothing : (() -> Maybe a) -> Maybe a -> Maybe a
onNothing fn b =
    case b of
        Just c ->
            Just c

        Nothing ->
            fn ()
