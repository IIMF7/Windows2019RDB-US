module PackageBrowser.Ui.Modal exposing (..)

import PackageBrowser.Ui exposing (..)


view a =
    column
        (width (shrink |> maximum 512)
            :: spacing 2
            :: bgColor gray10
            :: borderWidth 1
            :: borderRounded 1
            :: borderShadow 3
            :: borderColor gray7
            :: a
        )
