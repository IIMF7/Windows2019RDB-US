module PackageBrowser.View.Info exposing (..)

import Element.Font as Font
import PackageBrowser.Strings as Strings
import PackageBrowser.Ui exposing (..)
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
        Modal.view [ padding 2 ]
            [ h5 [ Font.center ]
                [ text Strings.info
                ]
            , column [ width fill, spacing 1 ]
                [ p []
                    [ text Strings.infoText1
                    ]
                , p []
                    [ text Strings.infoText2
                    ]
                , p []
                    [ text Strings.infoText3
                    ]
                , p []
                    [ newTabLink []
                        { label = text Strings.source
                        , url = "https://github.com/pravdomil/Elm-Packages"
                        }
                    , text ". "
                    , newTabLink []
                        { label = text Strings.tampermonkey
                        , url = "elm.user.js"
                        }
                    , text "."
                    ]
                ]
            , buttonLink [ centerX, padding 0.5 ]
                { label = text Strings.ok
                , onPress = Just ToggleInfo
                }
            ]

    else
        none
