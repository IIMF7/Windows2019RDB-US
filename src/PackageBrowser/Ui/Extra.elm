module PackageBrowser.Ui.Extra exposing (..)

import Element.Input as Input
import PackageBrowser.Ui.Base exposing (..)


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
    inputRadioRow (fontSize (rem 0.875) :: a)


segmentOption : a -> Element msg -> Input.Option a msg
segmentOption a b =
    inputOptionWith a
        (\v ->
            case v of
                Input.Idle ->
                    el [] b

                Input.Focused ->
                    el [] b

                Input.Selected ->
                    el [ fontColor primary ] b
        )
