module PackageBrowser.Ui.Status exposing (..)

import PackageBrowser.Ui exposing (..)


view a =
    p (padding 1 :: fontCenter :: fontColor gray6 :: a)
