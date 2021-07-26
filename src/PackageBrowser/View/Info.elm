module PackageBrowser.View.Info exposing (..)

import PackageBrowser.Translation as Translation
import PackageBrowser.Ui.Base exposing (..)
import PackageBrowser.Ui.Modal as Modal


type alias Model =
    { show : Bool
    }


init : Model
init =
    { show = False
    }



--


type Msg
    = ToggleInfo


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        ToggleInfo ->
            ( { model | show = not model.show }
            , Cmd.none
            )



--


view : Model -> Element Msg
view model =
    if model.show then
        Modal.view [ padding 32 ]
            [ h1 [ fontCenter ]
                [ text Translation.info
                ]
            , column [ width fill, spacing 16 ]
                [ p []
                    [ text Translation.infoText1
                    ]
                , p []
                    [ text Translation.infoText2
                    ]
                , p []
                    (Translation.infoText3
                        |> String.trim
                        |> String.lines
                        |> List.map text
                        |> List.intersperse br
                    )
                , p []
                    [ newTabLink []
                        { label = text Translation.source
                        , url = "https://github.com/pravdomil/Elm-Packages-Browser"
                        }
                    , text ". "
                    , newTabLink []
                        { label = text Translation.tampermonkey
                        , url = "elm.user.js"
                        }
                    , text "."
                    ]
                ]
            , link_ [ centerX, padding 8 ]
                { label = text Translation.ok
                , onPress = Just ToggleInfo
                }
            ]

    else
        none
