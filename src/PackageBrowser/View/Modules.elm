module PackageBrowser.View.Modules exposing (..)

import Database.ModuleGroup as ModuleGroup
import Database.ModuleGroup.Decode
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
    = GotModules (Result Http.Error (List ModuleGroup.ModuleGroup))
    | ScrollOffsetChanged Float
    | ToggleModuleGroup Elm.Module.Name


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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



--


view : String -> Model -> Element Msg
view search model =
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
                                Lazy.lazy2 viewModuleGroup (NameDict.member v.name model.expanded) v
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
