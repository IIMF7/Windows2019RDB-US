module PackageBrowser.Ui.Modal exposing (..)

import PackageBrowser.Ui exposing (..)


view a =
    column
        (width (shrink |> maximum 512)
            :: spacing 32
            :: bgColor grey10
            :: borderWidth 1
            :: borderRounded 1
            :: borderShadow 3
            :: borderColor grey7
            :: a
        )
