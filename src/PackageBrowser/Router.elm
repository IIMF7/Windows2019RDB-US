module PackageBrowser.Router exposing (..)

import Browser
import Browser.Navigation as Navigation
import Elm.Module
import Elm.Package
import Elm.Package.NameDict as NameDict
import Task
import Url
import Url.Builder
import Url.Parser as Parser
import Url.Parser.Query as Query


type alias Model =
    { key : Navigation.Key
    , baseUrl : Url.Url
    , view : View
    , recent : NameDict.NameDict ()
    }


init : Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init url key =
    ( { key = key
      , baseUrl = { url | query = Nothing, fragment = Nothing }
      , view = DefaultView
      , recent = NameDict.fromList []
      }
    , Task.succeed () |> Task.perform (\_ -> UrlChanged url)
    )



--


type View
    = DefaultView
    | PackageView Elm.Package.Name
    | ModuleView Elm.Package.Name Elm.Module.Name


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
            ModuleView b c

        Just ( Just b, _ ) ->
            PackageView b

        _ ->
            DefaultView


viewToPackageName : View -> Maybe Elm.Package.Name
viewToPackageName a =
    case a of
        DefaultView ->
            Nothing

        PackageView b ->
            Just b

        ModuleView b _ ->
            Just b


viewToUrl : View -> String
viewToUrl a =
    (case a of
        DefaultView ->
            []

        PackageView b ->
            [ Url.Builder.string "package" (Elm.Package.toString b)
            ]

        ModuleView b c ->
            [ Url.Builder.string "package" (Elm.Package.toString b)
            , Url.Builder.string "module" (Elm.Module.toString c)
            ]
    )
        |> Url.Builder.toQuery
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
            let
                view : View
                view =
                    viewFromUrl a

                recent : NameDict.NameDict ()
                recent =
                    if view == DefaultView then
                        [ Elm.Package.fromString "elm/core" ]
                            |> List.filterMap identity
                            |> List.map (\v -> ( v, () ))
                            |> NameDict.fromList

                    else
                        case view |> viewToPackageName of
                            Just b ->
                                model.recent |> NameDict.insert b ()

                            Nothing ->
                                model.recent
            in
            ( { model
                | view = viewFromUrl a
                , recent = recent
              }
            , Cmd.none
            )
