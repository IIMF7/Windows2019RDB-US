module PackageBrowser.Main exposing (..)

import Browser
import Browser.Navigation as Navigation
import Element
import Json.Decode as Decode
import PackageBrowser.Page.Home as Home
import PackageBrowser.Router as Router
import PackageBrowser.Strings as Strings
import PackageBrowser.Ui as Ui
import Url exposing (Url)


main : Program Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = Router.UrlRequested >> RouterMsg
        , onUrlChange = Router.UrlChanged >> RouterMsg
        }



--


type alias Model =
    { router : Router.Model
    , home : Home.Model
    }


init : Decode.Value -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( home, homeCmd ) =
            Home.init
    in
    ( { router = Router.init url key
      , home = home
      }
    , homeCmd |> Cmd.map HomeMsg
    )



--


type Msg
    = RouterMsg Router.Msg
    | HomeMsg Home.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RouterMsg a ->
            Router.update a model.router
                |> Tuple.mapBoth (\v -> { model | router = v }) (Cmd.map RouterMsg)

        HomeMsg a ->
            Home.update a model.home
                |> Tuple.mapBoth (\v -> { model | home = v }) (Cmd.map HomeMsg)



--


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



--


view : Model -> Browser.Document Msg
view model =
    { title = Strings.title
    , body =
        [ Element.layout (Ui.rootStyle [])
            (Home.view model model.home
                |> Element.map HomeMsg
            )
        ]
    }
