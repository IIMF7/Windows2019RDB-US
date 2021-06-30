module PackageBrowser.Ui.Modal exposing (..)

import PackageBrowser.Ui exposing (..)


view a =
    column
        (width (shrink |> maximum 512)
            :: spacing 32
            :: bgColor grey10
            :: borderWidth 1
            :: borderRounded 16
            :: borderShadow 48
            :: borderColor grey7
            :: a
        )
