module PackageBrowser.Main exposing (..)

import Browser
import Browser.Navigation as Navigation
import Element
import Element.Background as Background
import Html
import Json.Decode as Decode
import PackageBrowser.Page.Home as Home
import PackageBrowser.Router as Router
import PackageBrowser.Strings as Strings
import PackageBrowser.Ui as Ui
import Task
import Url exposing (Url)
import Utils.Update as Update


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
        ( router, routerCmd ) =
            Router.init url key

        ( home, homeCmd ) =
            Home.init
    in
    ( { router = router
      , home = home
      }
    , Cmd.batch
        [ routerCmd |> Cmd.map RouterMsg
        , homeCmd |> Cmd.map HomeMsg
        ]
    )



--


type Msg
    = RouterMsg Router.Msg
    | HomeMsg Home.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case msg of
        RouterMsg a ->
            Router.update a model.router
                |> Tuple.mapBoth (\v -> { model | router = v }) (Cmd.map RouterMsg)

        HomeMsg a ->
            Home.update model a model.home
                |> Tuple.mapBoth (\v -> { model | home = v }) (Cmd.map HomeMsg)
    )
        |> Update.andThen
            (\v ->
                case msg of
                    RouterMsg (Router.UrlChanged _) ->
                        Home.update v Home.UrlChanged v.home
                            |> Tuple.mapBoth (\vv -> { v | home = vv }) (Cmd.map HomeMsg)

                    _ ->
                        ( v
                        , Cmd.none
                        )
            )



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
        , scaleUi
        ]
    }


scaleUi : Html.Html msg
scaleUi =
    Html.node "style"
        []
        [ Html.text "@media only screen and (pointer: fine) { body { zoom: 0.875 } }"
        ]
