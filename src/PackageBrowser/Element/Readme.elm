module PackageBrowser.Element.Readme exposing (..)

import Database.Package.Readme as Readme
import Database.Package.Readme.Decode
import Dict
import Element
import Elm.Docs as Docs
import Elm.Module
import Elm.Package
import Elm.Package.NameDict as NameDict
import Http
import Markdown
import PackageBrowser.Router as Router
import PackageBrowser.Strings as Strings
import PackageBrowser.Ui exposing (..)


type alias Context a b =
    { a
        | router :
            { b
                | view : Router.View
                , recent : NameDict.NameDict ()
            }
    }



--


type alias Model =
    { readmes : NameDict.NameDict (Result Error Readme.Readme)
    }


type Error
    = Loading
    | HttpError Http.Error


init : ( Model, Cmd Msg )
init =
    ( { readmes = NameDict.fromList []
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


update : Context a b -> Msg -> Model -> ( Model, Cmd Msg )
update ctx msg model =
    case msg of
        UrlChanged ->
            case ctx.router.view |> Router.viewToPackageName of
                Just b ->
                    case model.readmes |> NameDict.get b of
                        Just _ ->
                            ( model
                            , Cmd.none
                            )

                        Nothing ->
                            ( { model | readmes = model.readmes |> NameDict.insert b (Err Loading) }
                            , getPackage b
                            )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        GotReadme a b ->
            ( { model | readmes = model.readmes |> NameDict.insert a (b |> Result.mapError HttpError) }
            , Cmd.none
            )



--


view : Router.View -> Model -> Element msg
view view_ model =
    column
        [ Element.width (Element.px 800)
        , Element.height Element.fill
        , Element.spacing 0
        ]
        (case view_ of
            Router.DefaultView ->
                []

            Router.PackageView b ->
                [ viewPackageHeader b
                , viewReadme viewPackageReadme (NameDict.get b model.readmes)
                ]

            Router.ModuleView b c ->
                [ viewPackageHeader b
                , viewModuleHeader b c
                , viewReadme (viewModuleReadme c) (NameDict.get b model.readmes)
                ]
        )


viewReadme : (a -> Element msg) -> Maybe (Result Error a) -> Element msg
viewReadme fn a =
    column
        [ Element.height Element.fill
        , Element.scrollbars
        , Element.paddingEach { left = 16, right = 16, top = 16, bottom = 32 }
        ]
        [ case a of
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
        ]


viewPackageHeader : Elm.Package.Name -> Element msg
viewPackageHeader a =
    row
        [ Element.paddingXY 16 12
        , defaultBorderColor
        , borderBottom
        ]
        [ h4 []
            [ link [ defaultTextColor ]
                { label = text (Elm.Package.toString a)
                , url = Router.PackageView a |> Router.viewToUrl
                }
            ]
        , newTabLink []
            { label = text Strings.source
            , url = "https://github.com/" ++ Elm.Package.toString a
            }
        , newTabLink []
            { label = text Strings.officialDocs
            , url = "https://package.elm-lang.org/packages/" ++ Elm.Package.toString a ++ "/latest/"
            }
        ]


viewModuleHeader : Elm.Package.Name -> Elm.Module.Name -> Element msg
viewModuleHeader a b =
    row
        [ Element.paddingXY 16 12
        , defaultBorderColor
        , borderBottom
        ]
        [ h5 []
            [ link [ defaultTextColor ]
                { label = text (Elm.Module.toString b)
                , url = Router.ModuleView a b |> Router.viewToUrl
                }
            ]
        , newTabLink []
            { label = text Strings.source
            , url = "https://github.com/" ++ Elm.Package.toString a ++ "/blob/master/src/" ++ (Elm.Module.toString b |> String.replace "." "/") ++ ".elm"
            }
        , newTabLink []
            { label = text Strings.officialDocs
            , url = "https://package.elm-lang.org/packages/" ++ Elm.Package.toString a ++ "/latest/" ++ (Elm.Module.toString b |> String.replace "." "-")
            }
        ]


viewPackageReadme : Readme.Readme -> Element msg
viewPackageReadme a =
    p []
        [ Element.html (Markdown.toHtml [] a.readme)
        ]



--


viewModuleReadme : Elm.Module.Name -> Readme.Readme -> Element msg
viewModuleReadme b a =
    case a.modules |> Dict.get (Elm.Module.toString b) of
        Just c ->
            section []
                (c
                    |> blocksToSections (Elm.Module.toString b)
                    |> List.map
                        (\( v, vv ) ->
                            section [ Element.spacing 0 ]
                                [ p [ mutedTextColor ]
                                    [ text v
                                    ]
                                , column
                                    [ Element.spacing 0
                                    , Element.paddingXY 16 0
                                    ]
                                    (vv |> List.map (viewBlock False))
                                ]
                        )
                )

        Nothing ->
            status []
                [ text Strings.moduleNotFound
                ]


blocksToSections : String -> List Docs.Block -> List ( String, List Docs.Block )
blocksToSections defaultTitle a =
    let
        getTitle : Docs.Block -> Maybe String
        getTitle b =
            case b of
                Docs.MarkdownBlock c ->
                    String.lines c
                        |> List.filter (String.startsWith "#")
                        |> List.head
                        |> Maybe.map (String.dropLeft 2)

                _ ->
                    Nothing

        fold : Docs.Block -> List ( String, List Docs.Block ) -> List ( String, List Docs.Block )
        fold b acc =
            case getTitle b of
                Just c ->
                    ( c, [] ) :: acc

                Nothing ->
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


viewBlock : Bool -> Docs.Block -> Element msg
viewBlock expand a =
    case a of
        Docs.MarkdownBlock b ->
            viewMarkdownBlock expand b

        Docs.UnionBlock b ->
            viewUnionBlock expand b

        Docs.AliasBlock b ->
            viewAliasBlock expand b

        Docs.ValueBlock b ->
            viewValueBlock expand b

        Docs.BinopBlock b ->
            viewBinopBlock expand b

        Docs.UnknownBlock b ->
            viewMarkdownBlock expand b


viewMarkdownBlock : Bool -> String -> Element msg
viewMarkdownBlock expand a =
    if expand then
        p []
            [ Element.html (Markdown.toHtml [] a)
            ]

    else
        none


viewUnionBlock : Bool -> Docs.Union -> Element msg
viewUnionBlock expand a =
    p []
        [ text a.name
        , if expand then
            p []
                [ Element.html (Markdown.toHtml [] a.comment)
                ]

          else
            none
        ]


viewAliasBlock : Bool -> Docs.Alias -> Element msg
viewAliasBlock expand a =
    p []
        [ text a.name
        , if expand then
            p []
                [ Element.html (Markdown.toHtml [] a.comment)
                ]

          else
            none
        ]


viewValueBlock : Bool -> Docs.Value -> Element msg
viewValueBlock expand a =
    p []
        [ text a.name
        , if expand then
            p []
                [ Element.html (Markdown.toHtml [] a.comment)
                ]

          else
            none
        ]


viewBinopBlock : Bool -> Docs.Binop -> Element msg
viewBinopBlock expand a =
    p []
        [ text ("(" ++ a.name ++ ")")
        , if expand then
            p []
                [ Element.html (Markdown.toHtml [] a.comment)
                ]

          else
            none
        ]
