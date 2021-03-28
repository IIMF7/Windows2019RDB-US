module PackageBrowser.Page.Home exposing (..)

import Database.Package as Package
import Database.Package.Decode
import Element
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Element.Keyed
import Element.Lazy as Lazy
import Elm.Module
import Elm.Package
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
            }
    }



--


type alias Model =
    { search : String
    , packages : Result PackagesError (List Package.Package)
    }


type PackagesError
    = LoadingPackages
    | PackagesHttpError Http.Error


init : ( Model, Cmd Msg )
init =
    ( { search = ""
      , packages = Err LoadingPackages
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
    = SearchChanged String
    | GotPackages (Result Http.Error (List Package.Package))


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        SearchChanged a ->
            ( { model | search = a }
            , Cmd.none
            )

        GotPackages a ->
            ( { model | packages = a |> Result.mapError PackagesHttpError }
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
        , Lazy.lazy2 viewFirst ctx.router.view model
        , border_
        , Lazy.lazy viewSecond ctx.router.view
        , border_
        ]


viewFirst : Router.View -> Model -> Element Msg
viewFirst view_ model =
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
            [ text Strings.title
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
        , viewPackages view_ model
        ]


viewPackages : Router.View -> Model -> Element.Element msg
viewPackages view_ model =
    Element.Keyed.column
        [ Element.height Element.fill
        , Element.width Element.fill
        , Element.scrollbars
        , borderColor
        , borderTop
        ]
        (case model.packages of
            Ok b ->
                b
                    |> List.map
                        (\v ->
                            ( Elm.Package.toString v.name
                            , Lazy.lazy3 viewPackage False (activePackageAndModule view_ v) v
                            )
                        )

            Err b ->
                [ ( ""
                  , p [ Font.center, mutedTextColor, Element.padding 16 ]
                        [ case b of
                            LoadingPackages ->
                                text Strings.loading

                            PackagesHttpError c ->
                                text (Strings.httpError c)
                        ]
                  )
                ]
        )


viewPackage : Bool -> Package.Package -> Element msg
viewPackage expand a =
    let
        limit : Int
        limit =
            6

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
    in
    column
        [ Element.spacing 0
        , borderColor
        , borderBottom
        ]
        [ link [ Element.width Element.fill, Element.paddingXY 16 8, mutedTextColor ]
            { label = text (Elm.Package.toString a.name)
            , url = Router.viewToUrl (Router.PackageView a.name)
            }
        , column [ Element.spacing 0 ]
            (exposed
                |> List.map
                    (\v ->
                        link [ Element.width Element.fill, Element.paddingXY 40 0, bodyTextColor ]
                            { label = text (Elm.Module.toString v)
                            , url = Router.viewToUrl (Router.ModuleView a.name v)
                            }
                    )
            )
        , if shortened then
            link [ Element.width Element.fill, Element.paddingXY 40 0, bodyTextColor ]
                { label = text "..."
                , url = Router.viewToUrl (Router.PackageView a.name)
                }

          else
            none
        , el [ Element.padding 6 ]
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
