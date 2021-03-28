module PackageBrowser.Page.Home exposing (..)

import Database.Package as Package
import Database.Package.Decode
import Element
import Element.Background as Background
import Element.Font as Font
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
            el [ Element.height Element.fill, border, borderRight ] none
    in
    row
        [ Element.height Element.fill
        , Element.width Element.shrink
        , Element.centerX
        , Element.spacing 0
        , Background.color white
        ]
        [ border_
        , Lazy.lazy viewFirst model
        , border_
        , Lazy.lazy viewSecond ctx.router.view
        , border_
        ]


viewFirst : Model -> Element Msg
viewFirst model =
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
            [ searchInput []
                { label = labelHidden Strings.searchInput
                , placeholder = Just (placeholder [] (text Strings.searchInput))
                , text = model.search
                , onChange = SearchChanged
                }
            ]
        , viewPackages model.packages
        ]


viewPackages : Result PackagesError (List Package.Package) -> Element.Element msg
viewPackages a =
    Element.Keyed.column
        [ Element.height Element.fill
        , Element.width Element.fill
        , Element.scrollbars
        , border
        , borderTop
        ]
        (case a of
            Ok b ->
                b
                    |> List.map
                        (\v ->
                            ( Elm.Package.toString v.name
                            , Lazy.lazy2 viewPackage False v
                            )
                        )

            Err b ->
                [ ( ""
                  , p [ Font.center, muted, Element.padding 16 ]
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
    column
        [ Element.spacing 0
        , Element.paddingXY 16 8
        , border
        , borderBottom
        ]
        [ p [ muted ]
            [ text (Elm.Package.toString a.name)
            ]
        , column [ Element.spacing 0, Element.paddingXY 20 4 ]
            (a.exposed
                |> Package.exposedToList
                |> List.map
                    (\v ->
                        text (Elm.Module.toString v)
                    )
            )
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
