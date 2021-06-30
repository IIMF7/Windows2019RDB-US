module PackageBrowser.View.Header exposing (..)

import PackageBrowser.Router as Router
import PackageBrowser.Translation as Translation
import PackageBrowser.Ui.Base exposing (..)
import PackageBrowser.Ui.Extra exposing (..)


type alias Model =
    { search : String
    , groupBy : GroupBy
    }


type GroupBy
    = GroupByPackages
    | GroupByModules


init : Model
init =
    { search = ""
    , groupBy = GroupByPackages
    }



--


type Msg
    = ToggleInfo
    | SearchChanged String
    | GroupByChanged GroupBy


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        ToggleInfo ->
            ( model
            , Cmd.none
            )

        SearchChanged a ->
            ( { model | search = a }
            , Cmd.none
            )

        GroupByChanged a ->
            ( { model | groupBy = a }
            , Cmd.none
            )



--


view : Model -> Element Msg
view model =
    column
        [ width fill
        , spacing 8
        , paddingXY 0 8
        , borderColor grey7
        , borderWidthEach 0 0 0 1
        ]
        [ row
            [ width fill
            , spacing 16
            , paddingXY 16 0
            ]
            [ h1 []
                [ link [ fontColor grey1 ]
                    { label = text Translation.title
                    , url = Router.DefaultView |> Router.viewToUrl
                    }
                ]
            , link_ []
                { label = text Translation.info
                , onPress = Just ToggleInfo
                }
            ]
        , column
            [ width fill
            , paddingXY 16 0
            , spacing 8
            ]
            [ inputSearch [ inputFocusedOnLoad ]
                { label = inputLabelHidden Translation.searchInput
                , placeholder = Just (inputPlaceholder [] (text Translation.searchInput))
                , text = model.search
                , onChange = SearchChanged
                }
            , segmentedControl []
                { label = labelLeft [] (text (Translation.groupBy ++ " "))
                , options =
                    [ segmentOption GroupByPackages (text (Translation.packageOption ++ " "))
                    , segmentOption GroupByModules (text (Translation.moduleOption ++ " "))
                    ]
                , selected = Just model.groupBy
                , onChange = GroupByChanged
                }
            ]
        ]
