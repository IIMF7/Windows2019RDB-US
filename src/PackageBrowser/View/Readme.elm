module PackageBrowser.View.Readme exposing (..)

import Browser.Dom
import Database.PackageReadme as PackageReadme
import Database.PackageReadme.Decode
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
import PackageBrowser.Translation as Translation
import PackageBrowser.Ui exposing (..)
import PackageBrowser.Ui.Markdown as Markdown
import PackageBrowser.Ui.Status as Status
import PackageBrowser.View.Readme.Section as Section
import Task
import Url.Builder


type alias Context a b =
    { a
        | router :
            { b
                | view : Router.View
            }
    }



--


type alias Model =
    { readmes : PackageNameDict.NameDict (Result Error PackageReadme.PackageReadme)
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
        , expect = Http.expectJson (GotReadme a) Database.PackageReadme.Decode.packageReadme
        }



--


type Msg
    = UrlChanged
    | GotReadme Elm.Package.Name (Result Http.Error PackageReadme.PackageReadme)
    | Reveal Elm.Package.Name
    | ViewportSet (Result Browser.Dom.Error ())


update : Context a b -> Msg -> Model -> ( Model, Cmd Msg )
update ctx msg model =
    case msg of
        UrlChanged ->
            case ctx.router.view |> Router.viewToPackageName of
                Just b ->
                    case model.readmes |> PackageNameDict.get b of
                        Just _ ->
                            ( model
                            , scrollToFragment ctx
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
            , if (ctx.router.view |> Router.viewToPackageName) == Just a then
                scrollToFragment ctx

              else
                Cmd.none
            )

        ViewportSet _ ->
            ( model
            , Cmd.none
            )


scrollToFragment : Context a b -> Cmd Msg
scrollToFragment ctx =
    case ctx.router.view |> Router.viewToFragment of
        Just c ->
            Task.map2
                (\v vv ->
                    Browser.Dom.setViewportOf readmeId 0 (v.viewport.y + vv.element.y - 104)
                )
                (Browser.Dom.getViewportOf readmeId)
                (Browser.Dom.getElement (Markdown.idFromString c))
                |> Task.andThen identity
                |> Task.attempt ViewportSet

        Nothing ->
            Browser.Dom.setViewportOf readmeId 0 0
                |> Task.attempt ViewportSet



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
    let
        docsUrl : String
        docsUrl =
            Url.Builder.custom
                (Url.Builder.CrossOrigin "https://package.elm-lang.org")
                ("packages" :: (a |> Elm.Package.toString |> String.split "/") ++ [ "latest" ])
                []
                Nothing

        gitHubUrl : String
        gitHubUrl =
            Url.Builder.custom
                (Url.Builder.CrossOrigin "https://github.com")
                (a |> Elm.Package.toString |> String.split "/")
                []
                Nothing
    in
    row
        [ width fill
        , spacing 1
        , paddingXY 16 12
        , borderColor grey7
        , borderWidthEach 0 0 0 1
        ]
        [ h5 []
            [ link [ fontColor grey1, onClick (Reveal a) ]
                { label = text (Elm.Package.toString a)
                , url = Router.PackageView a Nothing |> Router.viewToUrl
                }
            ]
        , newTabLink []
            { label = text Translation.officialDocs
            , url = docsUrl
            }
        , newTabLink []
            { label = text Translation.source
            , url = gitHubUrl
            }
        ]


viewModuleHeader : Elm.Package.Name -> Elm.Module.Name -> Element Msg
viewModuleHeader a b =
    let
        docsUrl : String
        docsUrl =
            Url.Builder.custom
                (Url.Builder.CrossOrigin "https://package.elm-lang.org")
                ("packages"
                    :: (a |> Elm.Package.toString |> String.split "/")
                    ++ [ "latest"
                       , b |> Elm.Module.toString |> String.replace "." "-"
                       ]
                )
                []
                Nothing

        gitHubUrl : String
        gitHubUrl =
            Url.Builder.custom
                (Url.Builder.CrossOrigin "https://github.com")
                ((a |> Elm.Package.toString |> String.split "/")
                    ++ [ "blob", "master", "src" ]
                    ++ (b |> Elm.Module.toString |> String.replace "." "/" |> (\v -> v ++ ".elm") |> String.split "/")
                )
                []
                (Just "L1")
    in
    row
        [ width fill
        , spacing 1
        , paddingXY 16 12
        , borderColor grey7
        , borderWidthEach 0 0 0 1
        ]
        [ h5 []
            [ link [ fontColor grey1, onClick (Reveal a) ]
                { label = text (Elm.Module.toString b)
                , url = Router.ModuleView a b Nothing |> Router.viewToUrl
                }
            ]
        , newTabLink []
            { label = text Translation.officialDocs
            , url = docsUrl
            }
        , newTabLink []
            { label = text Translation.source
            , url = gitHubUrl
            }
        ]



--


viewPackageReadme : Maybe (Result Error PackageReadme.PackageReadme) -> Element msg
viewPackageReadme a =
    let
        view_ : PackageReadme.PackageReadme -> Element msg
        view_ b =
            textColumn [ spacing 1 ]
                (b.readme
                    |> Markdown.Parser.parse
                    |> Result.toMaybe
                    |> Maybe.andThen (Markdown.Renderer.render Markdown.renderer >> Result.toMaybe)
                    |> Maybe.withDefault
                        [ Status.view []
                            [ text Translation.readmeIsNotAvailable
                            ]
                        ]
                )
    in
    column
        [ width fill
        , height fill
        , paddingEach 32 32 16 64
        , id readmeId
        , Element.scrollbars
        ]
        [ viewLoading view_ a
        ]



--


viewModuleReadme : Elm.Package.Name -> Elm.Module.Name -> Maybe (Result Error PackageReadme.PackageReadme) -> Element Msg
viewModuleReadme _ b c =
    let
        defaultTitle : String
        defaultTitle =
            Elm.Module.toString b
                |> String.split "."
                |> List.reverse
                |> List.head
                |> Maybe.withDefault ""

        view_ : PackageReadme.PackageReadme -> Element Msg
        view_ d =
            case d.modules |> ModuleNameDict.get b of
                Just e ->
                    case e.readme |> Section.fromMarkdown e defaultTitle of
                        Ok f ->
                            viewSections f

                        Err _ ->
                            Status.view []
                                [ text Translation.readmeIsNotAvailable
                                ]

                Nothing ->
                    Status.view []
                        [ text Translation.moduleNotFound
                        ]
    in
    column
        [ width fill
        , height fill
        , paddingEach 16 16 16 64
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
                    [ link [ id (Markdown.idFromString b.name), fontColor grey1 ]
                        { label = text b.name
                        , url = Markdown.idToUrl b.name
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
        (viewIndex a :: column [ paddingXY 0 8 ] [] :: (a |> List.map viewSection))


viewIndex : List Section.Section -> Element msg
viewIndex a =
    let
        viewSection : Section.Section -> Element.Element msg
        viewSection b =
            column [ width fill, spacing 0.25, paddingXY 24 0 ]
                (p []
                    [ link [ fontColor grey1 ]
                        { label = text b.name
                        , url = Markdown.idToUrl b.name
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
                    row [ width fill, paddingXY 24 0 ]
                        [ link []
                            { label = text c.name
                            , url = Markdown.idToUrl c.name
                            }
                        , el [ fontColor grey4 ]
                            (text c.type_)
                        ]
    in
    column [ width fill, spacing 1 ]
        (p [ fontWeight 7 ]
            [ text Translation.index
            ]
            :: (a |> List.map viewSection)
        )


viewMarkdown : List Markdown.Block.Block -> Element msg
viewMarkdown a =
    column [ width fill, paddingXY 48 0 ]
        [ textColumn
            [ padding 16
            , spacing 1
            , fontSize 0.9375
            , bgColor grey9
            , borderRounded 0.25
            ]
            (a
                |> Markdown.Renderer.render Markdown.renderer
                |> Result.withDefault
                    [ Status.view []
                        [ text Translation.readmeIsNotAvailable
                        ]
                    ]
            )
        ]


viewMember : Section.Member -> Element msg
viewMember a =
    column
        [ width fill
        , spacing 0.5
        , paddingXY 24 0
        ]
        [ row [ width fill ]
            [ link [ id (Markdown.idFromString a.name), fontWeight 7 ]
                { label = text a.name
                , url = Markdown.idToUrl a.name
                }
            , el [ fontColor grey4 ]
                (text a.type_)
            ]
        , column [ width fill, paddingXY 24 0 ]
            [ textColumn
                [ padding 16
                , spacing 1
                , fontSize 0.9375
                , bgColor grey9
                , borderRounded 0.25
                ]
                (a.comment
                    |> Markdown.Parser.parse
                    |> Result.toMaybe
                    |> Maybe.andThen (Markdown.Renderer.render Markdown.renderer >> Result.toMaybe)
                    |> Maybe.withDefault
                        [ Status.view []
                            [ text Translation.readmeIsNotAvailable
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
                                [ text Translation.loading
                                ]

                        HttpError d ->
                            Status.view []
                                [ text (Translation.httpError d)
                                ]

        Nothing ->
            Status.view []
                [ text Translation.packageNotFound
                ]



--


readmeId =
    "readme-view"
