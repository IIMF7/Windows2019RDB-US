module PackageBrowser.Ui.Status exposing (..)

import PackageBrowser.Ui.Base exposing (..)


view a =
    p (padding 16 :: fontCenter :: fontColor grey4 :: a)
