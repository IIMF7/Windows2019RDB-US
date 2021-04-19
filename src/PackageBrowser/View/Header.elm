module PackageBrowser.View.Header exposing (..)

import Element
import Element.Input as Input
import PackageBrowser.Router as Router
import PackageBrowser.Translation as Translation
import PackageBrowser.Ui exposing (..)


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


view : Model -> Element.Element Msg
view model =
    column
        [ width fill
        , spacing 0.5
        , paddingXY 0 0.5
        , borderColor gray3
        , borderWidthEach 0 0 0 1
        ]
        [ row
            [ width fill
            , spacing 1
            , paddingXY 1 0
            ]
            [ h5 []
                [ link [ fontColor gray9 ]
                    { label = text Translation.title
                    , url = Router.DefaultView |> Router.viewToUrl
                    }
                ]
            , buttonLink []
                { label = text Translation.info
                , onPress = Just ToggleInfo
                }
            ]
        , column
            [ width fill
            , paddingXY 1 0
            , spacing 0.5
            ]
            [ searchInput [ Input.focusedOnLoad ]
                { label = labelHidden Translation.searchInput
                , placeholder = Just (placeholder [] (text Translation.searchInput))
                , text = model.search
                , onChange = SearchChanged
                }
            , inputRadioRow []
                { label = labelLeft [] (text (Translation.groupBy ++ " "))
                , options =
                    [ inputOption GroupByPackages (text (Translation.packageOption ++ " "))
                    , inputOption GroupByModules (text (Translation.moduleOption ++ " "))
                    ]
                , selected = Just model.groupBy
                , onChange = GroupByChanged
                }
            ]
        ]
