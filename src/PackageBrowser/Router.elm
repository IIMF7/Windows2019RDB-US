module PackageBrowser.Router exposing (..)

import Browser
import Browser.Navigation as Navigation
import Elm.Module
import Elm.Package
import Task
import Url
import Url.Builder
import Url.Parser as Parser
import Url.Parser.Query as Query


type alias Model =
    { key : Navigation.Key
    , baseUrl : Url.Url
    , view : View
    }


init : Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init url key =
    ( { key = key
      , baseUrl = { url | query = Nothing, fragment = Nothing }
      , view = DefaultView
      }
    , Task.succeed () |> Task.perform (\_ -> UrlChanged url)
    )



--


type View
    = DefaultView
    | PackageView Elm.Package.Name (Maybe String)
    | ModuleView Elm.Package.Name Elm.Module.Name (Maybe String)


viewFromUrl : Url.Url -> View
viewFromUrl a =
    let
        parser : Parser.Parser (( Maybe Elm.Package.Name, Maybe Elm.Module.Name ) -> a) a
        parser =
            Parser.query
                (Query.map2
                    Tuple.pair
                    (Query.map (Maybe.andThen Elm.Package.fromString) (Query.string "package"))
                    (Query.map (Maybe.andThen Elm.Module.fromString) (Query.string "module"))
                )
    in
    case a |> (\v -> { v | path = "/" }) |> Parser.parse parser of
        Just ( Just b, Just c ) ->
            ModuleView b c a.fragment

        Just ( Just b, _ ) ->
            PackageView b a.fragment

        _ ->
            DefaultView


viewToPackageName : View -> Maybe Elm.Package.Name
viewToPackageName a =
    case a of
        DefaultView ->
            Nothing

        PackageView b _ ->
            Just b

        ModuleView b _ _ ->
            Just b


viewToModuleName : View -> Maybe ( Elm.Package.Name, Elm.Module.Name )
viewToModuleName a =
    case a of
        DefaultView ->
            Nothing

        PackageView _ _ ->
            Nothing

        ModuleView b c _ ->
            Just ( b, c )


viewToFragment : View -> Maybe String
viewToFragment a =
    case a of
        DefaultView ->
            Nothing

        PackageView _ b ->
            b

        ModuleView _ _ b ->
            b


viewToUrl : View -> String
viewToUrl a =
    (case a of
        DefaultView ->
            Url.Builder.custom
                Url.Builder.Relative
                []
                []
                Nothing

        PackageView b c ->
            Url.Builder.custom
                Url.Builder.Relative
                []
                [ Url.Builder.string "package" (Elm.Package.toString b)
                ]
                c

        ModuleView b c d ->
            Url.Builder.custom
                Url.Builder.Relative
                []
                [ Url.Builder.string "package" (Elm.Package.toString b)
                , Url.Builder.string "module" (Elm.Module.toString c)
                ]
                d
    )
        |> (++) "./"



--


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested a ->
            case a of
                Browser.Internal url ->
                    if { url | query = Nothing, fragment = Nothing } == model.baseUrl then
                        ( model
                        , Navigation.pushUrl model.key (Url.toString url)
                        )

                    else
                        ( model
                        , Navigation.load (Url.toString url)
                        )

                Browser.External url ->
                    ( model
                    , Navigation.load url
                    )

        UrlChanged a ->
            ( { model | view = viewFromUrl a }
            , Cmd.none
            )
