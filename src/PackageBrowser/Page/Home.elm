module PackageBrowser.Page.Home exposing (..)

import Database.Package as Package
import Database.Package.Decode
import Element
import Element.Background as Background
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
