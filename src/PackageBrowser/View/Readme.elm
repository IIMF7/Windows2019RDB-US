module PackageBrowser.View.Readme exposing (..)

import Browser.Dom
import Database.Package.Readme as Readme
import Database.Package.Readme.Decode
import Element
import Element.Events exposing (onClick)
import Elm.Module
import Elm.Module.NameDict as ModuleNameDict
import Elm.Package
import Elm.Package.NameDict as PackageNameDict
import Http
import Markdown.Block
import Markdown.Parser
import Markdown.Renderer
import PackageBrowser.Router as Router
import PackageBrowser.Strings as Strings
import PackageBrowser.Ui exposing (..)
import PackageBrowser.Ui.Markdown as Markdown
import PackageBrowser.Ui.Status as Status
import PackageBrowser.View.Readme.Section as Section
import Task


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
    = UrlChanged
    | GotReadme Elm.Package.Name (Result Http.Error Readme.Readme)
    | Reveal Elm.Package.Name
    | ViewportSet (Result Browser.Dom.Error ())


update : Context a b -> Msg -> Model -> ( Model, Cmd Msg )
update ctx msg model =
    case msg of
        UrlChanged ->
            (case ctx.router.view |> Router.viewToPackageName of
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
            )
                |> Tuple.mapSecond
                    (\v ->
                        Cmd.batch
                            [ Browser.Dom.setViewportOf readmeId 0 0
                                |> Task.attempt ViewportSet
                            , v
                            ]
                    )

        Reveal _ ->
            ( model
            , Cmd.none
            )

        GotReadme a b ->
            ( { model | readmes = model.readmes |> PackageNameDict.insert a (b |> Result.mapError HttpError) }
            , Cmd.none
            )

        ViewportSet _ ->
            ( model
            , Cmd.none
            )



--


view : Router.View -> Model -> Element Msg
view view_ model =
    column [ width fill, height fill ]
        (case view_ of
            Router.DefaultView ->
                []

            Router.PackageView b _ ->
                [ viewPackageHeader b
                , viewPackageReadme (PackageNameDict.get b model.readmes)
                ]

            Router.ModuleView b c _ ->
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
        [ h5 []
            [ link [ fontColor gray9, onClick (Reveal a) ]
                { label = text (Elm.Package.toString a)
                , url = Router.PackageView a Nothing |> Router.viewToUrl
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
                , url = Router.ModuleView a b Nothing |> Router.viewToUrl
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



--


viewPackageReadme : Maybe (Result Error Readme.Readme) -> Element msg
viewPackageReadme a =
    let
        view_ : Readme.Readme -> Element msg
        view_ b =
            column [ width fill, spacing 1 ]
                (b.readme
                    |> Markdown.Parser.parse
                    |> Result.toMaybe
                    |> Maybe.andThen (Markdown.Renderer.render Markdown.renderer >> Result.toMaybe)
                    |> Maybe.withDefault
                        [ Status.view []
                            [ text Strings.readmeIsNotAvailable
                            ]
                        ]
                )
    in
    column
        [ width fill
        , height fill
        , paddingEach 2 2 1 4
        , id readmeId
        , Element.scrollbars
        ]
        [ viewLoading view_ a
        ]



--


viewModuleReadme : Elm.Package.Name -> Elm.Module.Name -> Maybe (Result Error Readme.Readme) -> Element Msg
viewModuleReadme _ b c =
    let
        defaultTitle : String
        defaultTitle =
            Elm.Module.toString b
                |> String.split "."
                |> List.reverse
                |> List.head
                |> Maybe.withDefault ""

        view_ : Readme.Readme -> Element Msg
        view_ d =
            case d.modules |> ModuleNameDict.get b of
                Just e ->
                    case e.readme |> Section.fromMarkdown e defaultTitle of
                        Ok f ->
                            viewSections f

                        Err _ ->
                            Status.view []
                                [ text Strings.readmeIsNotAvailable
                                ]

                Nothing ->
                    Status.view []
                        [ text Strings.moduleNotFound
                        ]
    in
    column
        [ width fill
        , height fill
        , paddingEach 1 1 1 4
        , id readmeId
        , Element.scrollbars
        ]
        [ viewLoading view_ c
        ]


viewSections : List Section.Section -> Element msg
viewSections a =
    let
        viewSection : Section.Section -> Element.Element msg
        viewSection b =
            column [ width fill, spacing 1 ]
                (p [ fontWeight 7 ]
                    [ link [ id (Markdown.idFromString b.name), fontColor gray9 ]
                        { label = text b.name
                        , url = "#" ++ Markdown.idFromString b.name
                        }
                    ]
                    :: (b.items |> List.map viewItem)
                )

        viewItem : Section.Item -> Element msg
        viewItem b =
            case b of
                Section.MarkdownItem c ->
                    viewMarkdown c

                Section.MemberItem c ->
                    viewMember c
    in
    column [ width fill, spacing 1.5 ]
        (viewIndex a :: column [ paddingXY 0 0.5 ] [] :: (a |> List.map viewSection))


viewIndex : List Section.Section -> Element msg
viewIndex a =
    let
        viewSection : Section.Section -> Element.Element msg
        viewSection b =
            column [ width fill, spacing 0.25, paddingXY 1.5 0 ]
                (p []
                    [ link [ fontColor gray9 ]
                        { label = text b.name
                        , url = "#" ++ Markdown.idFromString b.name
                        }
                    ]
                    :: (b.items |> List.map viewItem)
                )

        viewItem : Section.Item -> Element msg
        viewItem b =
            case b of
                Section.MarkdownItem _ ->
                    none

                Section.MemberItem c ->
                    row [ width fill, paddingXY 1.5 0 ]
                        [ link []
                            { label = text c.name
                            , url = "#" ++ Markdown.idFromString c.name
                            }
                        , el [ fontColor gray6 ]
                            (text c.type_)
                        ]
    in
    column [ width fill, spacing 1 ]
        (p [ fontWeight 7 ]
            [ text Strings.index
            ]
            :: (a |> List.map viewSection)
        )


viewMarkdown : List Markdown.Block.Block -> Element msg
viewMarkdown a =
    column [ width fill, paddingXY 3 0 ]
        [ column
            [ width fill
            , padding 1
            , spacing 1
            , fontSize 0.9375
            , backgroundColor gray1
            , borderRounded 0.25
            ]
            (a
                |> Markdown.Renderer.render Markdown.renderer
                |> Result.withDefault
                    [ Status.view []
                        [ text Strings.readmeIsNotAvailable
                        ]
                    ]
            )
        ]


viewMember : Section.Member -> Element msg
viewMember a =
    column
        [ width fill
        , spacing 0.5
        , paddingXY 1.5 0
        ]
        [ row [ width fill ]
            [ link [ id (Markdown.idFromString a.name), fontWeight 7 ]
                { label = text a.name
                , url = "#" ++ Markdown.idFromString a.name
                }
            , el [ fontColor gray6 ]
                (text a.type_)
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
                        [ Status.view []
                            [ text Strings.readmeIsNotAvailable
                            ]
                        ]
                )
            ]
        ]



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
                            Status.view []
                                [ text Strings.loading
                                ]

                        HttpError d ->
                            Status.view []
                                [ text (Strings.httpError d)
                                ]

        Nothing ->
            Status.view []
                [ text Strings.packageNotFound
                ]



--


readmeId =
    "readme-view"
