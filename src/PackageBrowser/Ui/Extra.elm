module PackageBrowser.Ui.Extra exposing (..)

import Element.Input as Input
import PackageBrowser.Ui.Base exposing (..)
import PackageBrowser.Ui.Style as Style


segmentedControl :
    List (Attribute msg)
    ->
        { label : Input.Label msg
        , options : List (Input.Option a msg)
        , selected : Maybe a
        , onChange : a -> msg
        }
    -> Element msg
segmentedControl a =
    inputRadioRow a


segmentOption : a -> Element msg -> Input.Option a msg
segmentOption a b =
    inputOptionWith a
        (\v ->
            case v of
                Input.Idle ->
                    el [ fontSize Style.labelFontSize ] b

                Input.Focused ->
                    el [ fontSize Style.labelFontSize ] b

                Input.Selected ->
                    el [ fontSize Style.labelFontSize, fontColor primary ] b
        )
