module PackageBrowser.Ui.Modal exposing (..)

import PackageBrowser.Ui exposing (..)


view a =
    column
        (spacing 2
            :: width (shrink |> maximum 512)
            :: backgroundColor gray0
            :: borderShadow 3
            :: borderWidth 1
            :: borderColor gray3
            :: borderRounded 1
            :: a
        )
