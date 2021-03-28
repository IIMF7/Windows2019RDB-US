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
import Elm.Module
import Elm.Package
import Elm.Package.NameDict as NameDict
import Http
import Json.Decode as Decode
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
    | ViewportChanged (Result Browser.Dom.Error ())
    | SearchChanged String
    | ScrollOffsetChanged Float


update : Context a b -> Msg -> Model -> ( Model, Cmd Msg )
update ctx msg model =
    case msg of
        GotPackages a ->
            ( { model | packages = a |> Result.mapError HttpError }
            , Browser.Dom.setViewportOf packagesId 0 6240 |> Task.attempt ViewportChanged
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
    in
    row
        [ Element.height Element.fill
        , Element.width Element.shrink
        , Element.centerX
        , Element.spacing 0
        , Background.color white
        ]
        [ border_
        , Lazy.lazy3 viewLeftColumn ctx.router.view ctx.router.recent model
        , border_
        , Lazy.lazy2 viewRightColumn ctx.router.view model.readmes
        , border_
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
            [ h5 [ Element.paddingXY 16 0 ]
                [ link [ defaultTextColor ]
                    { label = text Strings.title
                    , url = Router.DefaultView |> Router.viewToUrl
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
    let
        getSize : Bool -> Package.Package -> Int
        getSize expand a =
            let
                len =
                    a.exposed |> Package.exposedToList |> List.length
            in
            32
                + (if not expand && len > modulesLimit then
                    (modulesLimit + 1) * 16

                   else
                    len * 16
                  )
                + 12
    in
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
                        , getSize = \v -> getSize (NameDict.member v.name recent) v
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
                ( True, modules_ |> List.take modulesLimit )

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
        , borderBottom
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
                { label = text "..."
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
                , viewPackageReadme readmes b
                ]

            Router.ModuleView b c ->
                [ text ""
                , h5
                    [ Element.spacing 2
                    , Element.paddingXY 16 0
                    ]
                    [ text (Elm.Package.toString b ++ " - " ++ Elm.Module.toString c)
                    ]
                ]
        )


viewPackageHeader : Elm.Package.Name -> Element msg
viewPackageHeader a =
    row
        [ Element.paddingXY 16 12
        , defaultBorderColor
        , borderBottom
        ]
        [ h5 []
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
        [ h6 []
            [ text (Elm.Module.toString b)
            ]
        , newTabLink []
            { label = text Strings.source
            , url = "https://github.com/" ++ Elm.Package.toString a ++ "/tree/master/src/" ++ (Elm.Module.toString b |> String.replace "." "/") ++ ".elm"
            }
        , newTabLink []
            { label = text Strings.officialDocs
            , url = "https://package.elm-lang.org/packages/" ++ Elm.Package.toString a ++ "/latest/" ++ (Elm.Module.toString b |> String.replace "." "-")
            }
        ]


viewPackageReadme : NameDict.NameDict (Result Error Readme.Readme) -> Elm.Package.Name -> Element msg
viewPackageReadme readmes a =
    column
        [ Element.height Element.fill
        , Element.scrollbars
        , Element.padding 16
        ]
        [ case readmes |> NameDict.get a of
            Just b ->
                case b of
                    Ok c ->
                        text c.readme

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
