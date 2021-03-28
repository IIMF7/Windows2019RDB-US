module PackageBrowser.Page.Home exposing (..)

import Database.Package as Package
import Database.Package.Decode
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
    { packages : Result PackagesError (List Package.Package)
    , search : String
    , scrollOffset : Float
    }


type PackagesError
    = LoadingPackages
    | PackagesHttpError Http.Error


init : ( Model, Cmd Msg )
init =
    ( { packages = Err LoadingPackages
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



--


type Msg
    = GotPackages (Result Http.Error (List Package.Package))
    | SearchChanged String
    | ScrollOffsetChanged Float


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        GotPackages a ->
            ( { model | packages = a |> Result.mapError PackagesHttpError }
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



--


view : Context a b -> Model -> Element Msg
view ctx model =
    let
        border_ =
            el [ Element.height Element.fill, borderColor, borderRight ] none
    in
    row
        [ Element.height Element.fill
        , Element.width Element.shrink
        , Element.centerX
        , Element.spacing 0
        , Background.color white
        ]
        [ border_
        , Lazy.lazy3 viewFirst ctx.router.view ctx.router.recent model
        , border_
        , Lazy.lazy viewSecond ctx.router.view
        , border_
        ]


viewFirst : Router.View -> NameDict.NameDict () -> Model -> Element Msg
viewFirst view_ recent model =
    column
        [ Element.height Element.fill
        , Element.width (Element.px 400)
        , Element.spacing 8
        ]
        [ text ""
        , h5
            [ Element.spacing 2
            , Element.paddingXY 16 0
            ]
            [ link [ bodyTextColor ]
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
        , viewPackages view_ recent model
        ]


viewPackages : Router.View -> NameDict.NameDict () -> Model -> Element.Element Msg
viewPackages view_ recent model =
    case model.packages of
        Ok b ->
            Element.Virtualized.column
                [ borderColor
                , borderTop
                ]
                { data = b
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
            column
                [ borderColor
                , borderTop
                ]
                [ p [ Font.center, mutedTextColor, Element.padding 16 ]
                    [ case b of
                        LoadingPackages ->
                            text Strings.loading

                        PackagesHttpError c ->
                            text (Strings.httpError c)
                    ]
                ]


getSize : Bool -> Package.Package -> Int
getSize expand a =
    let
        len =
            a.exposed |> Package.exposedToList |> List.length
    in
    32
        + (if not expand && len > limit then
            (limit + 1) * 16

           else
            len * 16
          )
        + 12


limit : Int
limit =
    6


viewPackage : Bool -> Maybe (Maybe Elm.Module.Name) -> Package.Package -> Element msg
viewPackage expand active a =
    let
        ( shortened, exposed ) =
            let
                exposed_ : List Elm.Module.Name
                exposed_ =
                    a.exposed |> Package.exposedToList
            in
            if expand == False && List.length exposed_ > limit then
                ( True, exposed_ |> List.take limit )

            else
                ( False, exposed_ )

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
                bodyTextColor
    in
    column
        [ Element.height Element.fill
        , Element.spacing 0
        , borderColor
        , borderBottom
        ]
        [ link [ Element.width Element.fill, Element.paddingXY 16 8, packageColor ]
            { label = text (Elm.Package.toString a.name)
            , url = Router.viewToUrl (Router.PackageView a.name)
            }
        , Element.Keyed.column [ Element.width Element.fill ]
            (exposed
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
            link [ Element.width Element.fill, Element.paddingXY 40 0, bodyTextColor ]
                { label = text "..."
                , url = Router.viewToUrl (Router.PackageView a.name)
                }

          else
            none
        ]


viewSecond : Router.View -> Element.Element msg
viewSecond a =
    column
        [ Element.width (Element.px 800)
        , Element.height Element.fill
        ]
        (case a of
            Router.DefaultView ->
                []

            Router.PackageView b ->
                [ text ""
                , h5
                    [ Element.spacing 2
                    , Element.paddingXY 16 0
                    ]
                    [ text (Elm.Package.toString b)
                    ]
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
