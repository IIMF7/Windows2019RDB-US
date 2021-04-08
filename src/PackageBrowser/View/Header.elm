module PackageBrowser.View.Header exposing (..)

import Element
import Element.Input as Input
import PackageBrowser.Router as Router
import PackageBrowser.Strings as Strings
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
                    { label = text Strings.title
                    , url = Router.DefaultView |> Router.viewToUrl
                    }
                ]
            , buttonLink []
                { label = text Strings.info
                , onPress = Just ToggleInfo
                }
            ]
        , column
            [ width fill
            , paddingXY 1 0
            , spacing 0.5
            ]
            [ searchInput [ Input.focusedOnLoad ]
                { label = labelHidden Strings.searchInput
                , placeholder = Just (placeholder [] (text Strings.searchInput))
                , text = model.search
                , onChange = SearchChanged
                }
            , inputRadioRow []
                { label = labelLeft [] (text (Strings.groupBy ++ " "))
                , options =
                    [ inputOption GroupByPackages (text (Strings.packageOption ++ " "))
                    , inputOption GroupByModules (text (Strings.moduleOption ++ " "))
                    ]
                , selected = Just model.groupBy
                , onChange = GroupByChanged
                }
            ]
        ]
