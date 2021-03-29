module PackageBrowser.Page.Home exposing (..)

import Browser.Dom
import Database.Package as Package
import Database.Package.Decode
import Database.Package.Readme as Readme
import Database.Package.Readme.Decode
import Dict
import Element
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Element.Keyed
import Element.Lazy as Lazy
import Element.Virtualized
import Elm.Docs as Docs
import Elm.Module
import Elm.Package
import Elm.Package.NameDict as NameDict
import Http
import Json.Decode as Decode
import Markdown
import PackageBrowser.Router as Router
import PackageBrowser.Strings as Strings
import PackageBrowser.Ui exposing (..)
import Regex
import Task


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
    { packages : Result Error (List Package.Package)
    , readmes : NameDict.NameDict (Result Error Readme.Readme)
    , showInfo : Bool
    , search : String
    , scrollOffset : Float
    }


type Error
    = Loading
    | HttpError Http.Error


init : ( Model, Cmd Msg )
init =
    ( { packages = Err Loading
      , readmes = NameDict.fromList []
      , showInfo = False
      , search = ""
      , scrollOffset = 0
      }
    , getPackages
    )


getPackages : Cmd Msg
getPackages =
    Http.get
        { url = "db/packages.json"
        , expect = Http.expectJson GotPackages (Decode.list Database.Package.Decode.package)
        }


getPackage : Elm.Package.Name -> Cmd Msg
getPackage a =
    Http.get
        { url = "db/" ++ (a |> Elm.Package.toString |> String.replace "/" " ") ++ ".json"
        , expect = Http.expectJson (GotReadme a) Database.Package.Readme.Decode.readme
        }



--


type Msg
    = GotPackages (Result Http.Error (List Package.Package))
    | UrlChanged
    | GotReadme Elm.Package.Name (Result Http.Error Readme.Readme)
    | ToggleInfo
    | ViewportChanged (Result Browser.Dom.Error ())
    | SearchChanged String
    | ScrollOffsetChanged Float


update : Context a b -> Msg -> Model -> ( Model, Cmd Msg )
update ctx msg model =
    case msg of
        GotPackages a ->
            let
                scrollToPackage : Cmd Msg
                scrollToPackage =
                    a
                        |> Result.toMaybe
                        |> Maybe.andThen
                            (\v ->
                                Element.Virtualized.getScrollOffset
                                    { data = v
                                    , getKey = .name >> Elm.Package.toString
                                    , getSize = \vv -> computeSize (NameDict.member vv.name ctx.router.recent) vv
                                    , key = package
                                    }
                            )
                        |> Maybe.map
                            (\v ->
                                Browser.Dom.setViewportOf packagesId 0 (toFloat v)
                                    |> Task.attempt ViewportChanged
                            )
                        |> Maybe.withDefault Cmd.none

                package : String
                package =
                    ctx.router.view
                        |> Router.viewToPackageName
                        |> Maybe.map Elm.Package.toString
                        |> Maybe.withDefault "elm/core"
            in
            ( { model | packages = a |> Result.mapError HttpError }
            , scrollToPackage
            )

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

        ToggleInfo ->
            ( { model | showInfo = not model.showInfo }
            , Cmd.none
            )

        ViewportChanged _ ->
            ( model
            , Cmd.none
            )

        SearchChanged a ->
            ( { model | search = a }
            , Cmd.none
            )

        ScrollOffsetChanged a ->
            ( { model | scrollOffset = a }
            , Cmd.none
            )


packagesId =
    "packages"



--


view : Context a b -> Model -> Element Msg
view ctx model =
    let
        border_ =
            el [ Element.height Element.fill, defaultBorderColor, borderRight ] none

        info : Element.Attribute Msg
        info =
            if model.showInfo then
                Element.inFront viewInfo

            else
                noneAttribute
    in
    row
        [ Element.height Element.fill
        , Element.width Element.shrink
        , Element.centerX
        , Element.spacing 0
        , Background.color white
        , info
        ]
        [ border_
        , Lazy.lazy3 viewLeftColumn ctx.router.view ctx.router.recent model
        , border_
        , Lazy.lazy2 viewRightColumn ctx.router.view model.readmes
        , border_
        ]


viewInfo : Element Msg
viewInfo =
    modal [ Element.moveDown 16, Element.moveRight 16, Element.padding 32 ]
        [ h5 [ Font.center ]
            [ text Strings.info
            ]
        , section []
            [ p []
                [ text Strings.infoText1
                ]
            , p []
                [ text Strings.infoText2
                ]
            , p []
                [ text Strings.infoText3
                ]
            , p []
                [ newTabLink []
                    { label = text Strings.source
                    , url = "https://github.com/pravdomil/Elm-Packages"
                    }
                , text ". "
                , newTabLink []
                    { label = text Strings.proposalLink
                    , url = "https://github.com/elm/package.elm-lang.org/issues"
                    }
                , text "."
                ]
            ]
        , buttonLink [ Element.centerX, Element.padding 8 ]
            { label = text Strings.ok
            , onPress = Just ToggleInfo
            }
        ]



--


viewLeftColumn : Router.View -> NameDict.NameDict () -> Model -> Element Msg
viewLeftColumn view_ recent model =
    column
        [ Element.height Element.fill
        , Element.width (Element.px 400)
        , Element.spacing 0
        ]
        [ column
            [ Element.spacing 8
            , Element.paddingXY 0 8
            , defaultBorderColor
            , borderBottom
            ]
            [ row [ Element.paddingXY 16 0 ]
                [ h5 []
                    [ link [ defaultTextColor ]
                        { label = text Strings.title
                        , url = Router.DefaultView |> Router.viewToUrl
                        }
                    ]
                , buttonLink []
                    { label = text Strings.info
                    , onPress = Just ToggleInfo
                    }
                ]
            , row
                [ Element.paddingXY 16 0
                ]
                [ searchInput [ Input.focusedOnLoad ]
                    { label = labelHidden Strings.searchInput
                    , placeholder = Just (placeholder [] (text Strings.searchInput))
                    , text = model.search
                    , onChange = SearchChanged
                    }
                ]
            ]
        , viewPackages view_ recent model
        ]


modulesLimit : Int
modulesLimit =
    6


viewPackages : Router.View -> NameDict.NameDict () -> Model -> Element Msg
viewPackages view_ recent model =
    case model.packages of
        Ok b ->
            case filterPackages model.search b of
                [] ->
                    status []
                        [ text Strings.noPackagesFound
                        ]

                c ->
                    Element.Virtualized.column [ id packagesId ]
                        { data = c
                        , getKey = .name >> Elm.Package.toString
                        , getSize = \v -> computeSize (NameDict.member v.name recent) v
                        , scrollOffset = model.scrollOffset
                        , view =
                            \v ->
                                Lazy.lazy3 viewPackage
                                    (NameDict.member v.name recent)
                                    (activePackageAndModule view_ v)
                                    v
                        , onScroll = ScrollOffsetChanged
                        }

        Err b ->
            status []
                [ case b of
                    Loading ->
                        text Strings.loading

                    HttpError c ->
                        text (Strings.httpError c)
                ]


computeSize : Bool -> Package.Package -> Int
computeSize expand a =
    let
        len =
            a.exposed |> Package.exposedToList |> List.length
    in
    32
        + (if not expand && len > modulesLimit then
            modulesLimit * 16

           else
            len * 16
          )
        + 12


viewPackage : Bool -> Maybe (Maybe Elm.Module.Name) -> Package.Package -> Element msg
viewPackage expand active a =
    let
        ( shortened, modules ) =
            let
                modules_ : List Elm.Module.Name
                modules_ =
                    a.exposed |> Package.exposedToList
            in
            if expand == False && List.length modules_ > modulesLimit then
                ( True, modules_ |> List.take (modulesLimit - 1) )

            else
                ( False, modules_ )

        packageColor : Element.Attribute msg
        packageColor =
            if active == Just Nothing then
                noneAttribute

            else
                mutedTextColor

        moduleColor : Elm.Module.Name -> Element.Attribute msg
        moduleColor b =
            if active == Just (Just b) then
                noneAttribute

            else
                defaultTextColor
    in
    column
        [ Element.height Element.fill
        , Element.spacing 0
        , defaultBorderColor
        ]
        [ link [ Element.width Element.fill, Element.paddingXY 16 8, packageColor ]
            { label = text (Elm.Package.toString a.name)
            , url = Router.viewToUrl (Router.PackageView a.name)
            }
        , Element.Keyed.column [ Element.width Element.fill ]
            (modules
                |> List.map
                    (\v ->
                        ( Elm.Module.toString v
                        , link [ Element.width Element.fill, Element.paddingXY 40 0, moduleColor v ]
                            { label = text (Elm.Module.toString v)
                            , url = Router.viewToUrl (Router.ModuleView a.name v)
                            }
                        )
                    )
            )
        , if shortened then
            link [ Element.width Element.fill, Element.paddingXY 40 0, defaultTextColor ]
                { label = text Strings.ellipsis
                , url = Router.viewToUrl (Router.PackageView a.name)
                }

          else
            none
        ]



--


viewRightColumn : Router.View -> NameDict.NameDict (Result Error Readme.Readme) -> Element msg
viewRightColumn view_ readmes =
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
                , viewReadme viewPackageReadme (NameDict.get b readmes)
                ]

            Router.ModuleView b c ->
                [ viewPackageHeader b
                , viewModuleHeader b c
                , viewReadme (viewModuleReadme c) (NameDict.get b readmes)
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
            column []
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
                                    (vv |> List.map viewBlock)
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


viewBlock : Docs.Block -> Element msg
viewBlock a =
    case a of
        Docs.MarkdownBlock b ->
            viewMarkdownBlock b

        Docs.UnionBlock b ->
            viewUnionBlock b

        Docs.AliasBlock b ->
            viewAliasBlock b

        Docs.ValueBlock b ->
            viewValueBlock b

        Docs.BinopBlock b ->
            viewBinopBlock b

        Docs.UnknownBlock b ->
            viewMarkdownBlock b


viewMarkdownBlock : String -> Element msg
viewMarkdownBlock a =
    p []
        [ Element.html (Markdown.toHtml [] a)
        ]


viewUnionBlock : Docs.Union -> Element msg
viewUnionBlock a =
    p []
        [ text a.name
        ]


viewAliasBlock : Docs.Alias -> Element msg
viewAliasBlock a =
    p []
        [ text a.name
        ]


viewValueBlock : Docs.Value -> Element msg
viewValueBlock a =
    p []
        [ text a.name
        ]


viewBinopBlock : Docs.Binop -> Element msg
viewBinopBlock a =
    p []
        [ text ("(" ++ a.name ++ ")")
        ]



--


activePackageAndModule : Router.View -> Package.Package -> Maybe (Maybe Elm.Module.Name)
activePackageAndModule view_ a =
    case view_ of
        Router.DefaultView ->
            Nothing

        Router.PackageView b ->
            if a.name == b then
                Just Nothing

            else
                Nothing

        Router.ModuleView b c ->
            if a.name == b then
                Just (Just c)

            else
                Nothing


filterPackages : String -> List Package.Package -> List Package.Package
filterPackages search a =
    let
        keywords : List String
        keywords =
            search |> toKeywords

        isRelevant : String -> Bool
        isRelevant b =
            let
                c =
                    b |> toKeywords
            in
            keywords |> List.all (\v -> c |> List.any (String.startsWith v))

        keywordsRegex : Regex.Regex
        keywordsRegex =
            Regex.fromString "[A-Za-z0-9]+" |> Maybe.withDefault Regex.never

        toKeywords : String -> List String
        toKeywords b =
            b |> String.toLower |> Regex.find keywordsRegex |> List.map .match
    in
    a
        |> List.filter
            (\v ->
                (v.name |> Elm.Package.toString |> isRelevant)
                    || (v.exposed |> Package.exposedToList |> List.any (Elm.Module.toString >> isRelevant))
            )
