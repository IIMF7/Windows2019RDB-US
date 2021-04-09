module PackageBrowser.Main exposing (..)

import Browser
import Browser.Navigation as Navigation
import Element
import Element.Lazy as Lazy
import Html
import Json.Decode as Decode
import PackageBrowser.Router as Router
import PackageBrowser.Strings as Strings
import PackageBrowser.Ui as Ui exposing (..)
import PackageBrowser.View.Header as Header
import PackageBrowser.View.Info as Info
import PackageBrowser.View.Modules as Modules
import PackageBrowser.View.Packages as Packages
import PackageBrowser.View.Readme as Readme
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
    , header : Header.Model
    , packages : Packages.Model
    , modules : Modules.Model
    , readme : Readme.Model
    }


init : Decode.Value -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( router, routerCmd ) =
            Router.init url key

        ( packages, packagesCmd ) =
            Packages.init

        ( modules, modulesCmd ) =
            Modules.init

        ( readme, readmeCmd ) =
            Readme.init
    in
    ( { router = router
      , info = Info.init
      , header = Header.init
      , packages = packages
      , modules = modules
      , readme = readme
      }
    , Cmd.batch
        [ routerCmd |> Cmd.map RouterMsg
        , packagesCmd |> Cmd.map PackagesMsg
        , modulesCmd |> Cmd.map ModulesMsg
        , readmeCmd |> Cmd.map ReadmeMsg
        ]
    )



--


type Msg
    = RouterMsg Router.Msg
    | InfoMsg Info.Msg
    | HeaderMsg Header.Msg
    | PackagesMsg Packages.Msg
    | ModulesMsg Modules.Msg
    | ReadmeMsg Readme.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model |> Update.multiple (msg :: forwardMsg msg) update_


update_ : Msg -> Model -> ( Model, Cmd Msg )
update_ msg model =
    case msg of
        RouterMsg a ->
            Router.update a model.router
                |> Tuple.mapBoth (\v -> { model | router = v }) (Cmd.map RouterMsg)

        InfoMsg a ->
            Info.update a model.info
                |> Tuple.mapBoth (\v -> { model | info = v }) (Cmd.map InfoMsg)

        HeaderMsg a ->
            Header.update a model.header
                |> Tuple.mapBoth (\v -> { model | header = v }) (Cmd.map HeaderMsg)

        PackagesMsg a ->
            Packages.update model a model.packages
                |> Tuple.mapBoth (\v -> { model | packages = v }) (Cmd.map PackagesMsg)

        ModulesMsg a ->
            Modules.update model a model.modules
                |> Tuple.mapBoth (\v -> { model | modules = v }) (Cmd.map ModulesMsg)

        ReadmeMsg a ->
            Readme.update model a model.readme
                |> Tuple.mapBoth (\v -> { model | readme = v }) (Cmd.map ReadmeMsg)


forwardMsg : Msg -> List Msg
forwardMsg msg =
    case msg of
        RouterMsg (Router.UrlChanged _) ->
            [ PackagesMsg Packages.UrlChanged
            , ModulesMsg Modules.UrlChanged
            , ReadmeMsg Readme.UrlChanged
            ]

        HeaderMsg Header.ToggleInfo ->
            [ InfoMsg Info.ToggleInfo
            ]

        HeaderMsg (Header.SearchChanged _) ->
            [ PackagesMsg Packages.SearchChanged
            , ModulesMsg Modules.SearchChanged
            ]

        HeaderMsg (Header.GroupByChanged _) ->
            [ PackagesMsg Packages.RestoreScrollOffset
            , ModulesMsg Modules.RestoreScrollOffset
            ]

        ReadmeMsg (Readme.Reveal a) ->
            [ PackagesMsg (Packages.Reveal a)
            ]

        _ ->
            []



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
        border_ : Element msg
        border_ =
            el [ height fill, borderColor gray3, borderWidthEach 1 0 0 0 ] none

        infoView : Element.Attribute Msg
        infoView =
            Element.inFront
                (el
                    [ Element.moveDown 16
                    , Element.moveRight 16
                    ]
                    (Info.view model.info |> Element.map InfoMsg)
                )
    in
    row
        [ width fill
        , height fill
        , infoView
        ]
        [ el [ width fill ] none
        , border_
        , column [ width (px 320), height fill, bgColor gray0 ]
            [ Lazy.lazy Header.view model.header
                |> Element.map HeaderMsg
            , case model.header.groupBy of
                Header.GroupByPackages ->
                    Lazy.lazy3 Packages.view model.header.search model.router.view model.packages
                        |> Element.map PackagesMsg

                Header.GroupByModules ->
                    Lazy.lazy3 Modules.view model.router.view model.header.search model.modules
                        |> Element.map ModulesMsg
            ]
        , border_
        , el [ width (px 880), height fill, bgColor gray0 ]
            (Lazy.lazy2 Readme.view model.router.view model.readme
                |> Element.map ReadmeMsg
            )
        , border_
        , el [ width fill ] none
        ]


scaleUi : Html.Html msg
scaleUi =
    Html.node "style"
        []
        [ Html.text "@media only screen and (pointer: fine) { body { zoom: 0.875 } }"
        ]
