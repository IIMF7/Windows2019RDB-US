module PackageBrowser.Ui.Modal exposing (..)

import PackageBrowser.Ui exposing (..)


view a =
    column
        (Element.spacing 32
            :: Element.width Element.fill
            :: Element.width (Element.shrink |> Element.maximum 512)
            :: Color.bgGray0
            :: borderShadow
            :: border
            :: Color.borderGray3
            :: Border.rounded 16
            :: a
        )
