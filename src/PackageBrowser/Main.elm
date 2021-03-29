module PackageBrowser.Main exposing (..)

import Browser
import Browser.Navigation as Navigation
import Element
import Element.Background as Background
import Element.Lazy as Lazy
import Html
import Json.Decode as Decode
import PackageBrowser.Element.Info as Info
import PackageBrowser.Element.Packages as Packages
import PackageBrowser.Element.Readme as Readme
import PackageBrowser.Router as Router
import PackageBrowser.Strings as Strings
import PackageBrowser.Ui as Ui exposing (..)
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
    , info : Info.Model
    , packages : Packages.Model
    , readme : Readme.Model
    }


init : Decode.Value -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( router, routerCmd ) =
            Router.init url key

        ( packages, packagesCmd ) =
            Packages.init

        ( readme, readmeCmd ) =
            Readme.init
    in
    ( { router = router
      , info = Info.init
      , packages = packages
      , readme = readme
      }
    , Cmd.batch
        [ routerCmd |> Cmd.map RouterMsg
        , packagesCmd |> Cmd.map PackagesMsg
        , readmeCmd |> Cmd.map ReadmeMsg
        ]
    )



--


type Msg
    = RouterMsg Router.Msg
    | InfoMsg Info.Msg
    | PackagesMsg Packages.Msg
    | ReadmeMsg Readme.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case msg of
        RouterMsg a ->
            Router.update a model.router
                |> Tuple.mapBoth (\v -> { model | router = v }) (Cmd.map RouterMsg)

        InfoMsg a ->
            Info.update a model.info
                |> Tuple.mapBoth (\v -> { model | info = v }) (Cmd.map InfoMsg)

        PackagesMsg a ->
            Packages.update model a model.packages
                |> Tuple.mapBoth (\v -> { model | packages = v }) (Cmd.map PackagesMsg)

        ReadmeMsg a ->
            Readme.update model a model.readme
                |> Tuple.mapBoth (\v -> { model | readme = v }) (Cmd.map ReadmeMsg)
    )
        |> Update.andThen
            (\v ->
                case msg of
                    RouterMsg (Router.UrlChanged _) ->
                        Readme.update v Readme.UrlChanged v.readme
                            |> Tuple.mapBoth (\vv -> { v | readme = vv }) (Cmd.map ReadmeMsg)

                    PackagesMsg Packages.ToggleInfo ->
                        Info.update Info.ToggleInfo v.info
                            |> Tuple.mapBoth (\vv -> { v | info = vv }) (Cmd.map InfoMsg)

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
        [ Element.layout (Ui.rootStyle []) (viewBody model)
        , scaleUi
        ]
    }


viewBody : Model -> Element Msg
viewBody model =
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
        , Element.inFront (Info.view model.info |> Element.map InfoMsg)
        ]
        [ border_
        , Lazy.lazy3 Packages.view model.router.view model.router.recent model.packages
            |> Element.map PackagesMsg
        , border_
        , Lazy.lazy2 Readme.view model.router.view model.readme
            |> Element.map ReadmeMsg
        , border_
        ]


scaleUi : Html.Html msg
scaleUi =
    Html.node "style"
        []
        [ Html.text "@media only screen and (pointer: fine) { body { zoom: 0.875 } }"
        ]
