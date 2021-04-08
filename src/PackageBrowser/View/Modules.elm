module PackageBrowser.View.Modules exposing (..)

import Browser.Dom
import Database.ModuleGroup as ModuleGroup
import Database.ModuleGroup.Decode
import Element
import Element.Keyed
import Element.Lazy as Lazy
import Element.Virtualized
import Elm.Module
import Elm.Module.NameDict as NameDict
import Elm.Package
import Http
import Json.Decode as Decode
import PackageBrowser.Router as Router
import PackageBrowser.Strings as Strings
import PackageBrowser.Ui exposing (..)
import PackageBrowser.Ui.Status as Status
import Task


type alias Context a b =
    { a
        | router :
            { b
                | view : Router.View
            }
    }



--


type alias Model =
    { modules : Result Error (List ModuleGroup.ModuleGroup)
    , scrollOffset : Float
    , expanded : NameDict.NameDict ()
    }


type Error
    = Loading
    | HttpError Http.Error


init : ( Model, Cmd Msg )
init =
    ( { modules = Err Loading
      , scrollOffset = 0
      , expanded = NameDict.empty
      }
    , getModules
    )


getModules : Cmd Msg
getModules =
    Http.get
        { url = "db/modules.json"
        , expect = Http.expectJson GotModules (Decode.list Database.ModuleGroup.Decode.moduleGroup)
        }



--


type Msg
    = UrlChanged
    | GotModules (Result Http.Error (List ModuleGroup.ModuleGroup))
    | ScrollOffsetChanged Float
    | ToggleModuleGroup Elm.Module.Name
    | SearchChanged
    | ViewportSet (Result Browser.Dom.Error ())


update : Context a b -> Msg -> Model -> ( Model, Cmd Msg )
update ctx msg model =
    case msg of
        UrlChanged ->
            case ctx.router.view of
                Router.DefaultView ->
                    ( { model | expanded = NameDict.empty }
                    , scrollToTop
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        GotModules a ->
            ( { model | modules = a |> Result.mapError HttpError }
            , Cmd.none
            )

        ScrollOffsetChanged a ->
            ( { model | scrollOffset = a }
            , Cmd.none
            )

        ToggleModuleGroup a ->
            ( { model
                | expanded =
                    model.expanded
                        |> NameDict.update a
                            (\v ->
                                if v == Nothing then
                                    Just ()

                                else
                                    Nothing
                            )
              }
            , Cmd.none
            )

        SearchChanged ->
            ( model
            , scrollToTop
            )

        ViewportSet _ ->
            ( model
            , Cmd.none
            )


scrollToTop : Cmd Msg
scrollToTop =
    Browser.Dom.setViewportOf modulesId 0 0
        |> Task.attempt ViewportSet



--


view : Router.View -> String -> Model -> Element Msg
view view_ search model =
    case model.modules of
        Ok b ->
            case filterPackages search b of
                [] ->
                    Status.view []
                        [ text Strings.noModulesFound
                        ]

                c ->
                    Element.Virtualized.column [ paddingXY 0 4, id modulesId ]
                        { data = c
                        , getKey = .name >> Elm.Module.toString
                        , getSize = \v -> computeSize (NameDict.member v.name model.expanded) v
                        , scrollOffset = model.scrollOffset
                        , view =
                            \v ->
                                Lazy.lazy3 viewModuleGroup (Router.viewToModuleName view_) (NameDict.member v.name model.expanded) v
                        , onScroll = ScrollOffsetChanged
                        }

        Err b ->
            Status.view []
                [ case b of
                    Loading ->
                        text Strings.loading

                    HttpError c ->
                        text (Strings.httpError c)
                ]


computeSize : Bool -> ModuleGroup.ModuleGroup -> Int
computeSize expand a =
    if expand then
        32 + (List.length a.modules |> (\v -> v * 16 + (v - 1) * 4)) + 12

    else
        32


viewModuleGroup : Maybe ( Elm.Package.Name, Elm.Module.Name ) -> Bool -> ModuleGroup.ModuleGroup -> Element Msg
viewModuleGroup active expand a =
    let
        linkColor : Elm.Package.Name -> Elm.Module.Name -> Element.Attribute msg
        linkColor b c =
            if Just ( b, c ) == active then
                noneAttribute

            else
                fontColor gray9
    in
    column [ width fill, height fill ]
        [ buttonLink [ width fill, paddingXY 1 0.5, fontColor gray6 ]
            { label = text (Elm.Module.toString a.name)
            , onPress = Just (ToggleModuleGroup a.name)
            }
        , if expand then
            Element.Keyed.column [ width fill, spacing 0.25 ]
                (a.modules
                    |> List.map
                        (\( v, vv ) ->
                            ( Elm.Package.toString v ++ Elm.Module.toString vv
                            , link [ width fill, paddingXY 2.5 0, linkColor v vv ]
                                { label =
                                    row []
                                        [ text (Elm.Module.toString vv)
                                        , text " "
                                        , el [ fontColor gray5, fontSize 0.75 ]
                                            (text (Elm.Package.toString v))
                                        ]
                                , url = Router.viewToUrl (Router.ModuleView v vv Nothing)
                                }
                            )
                        )
                )

          else
            none
        ]



--


modulesId =
    "modules-view"


filterPackages : String -> List ModuleGroup.ModuleGroup -> List ModuleGroup.ModuleGroup
filterPackages search a =
    a
        |> List.filter
            (\v ->
                v
                    |> .name
                    |> Elm.Module.toString
                    |> String.toLower
                    |> String.startsWith (String.toLower search)
            )
