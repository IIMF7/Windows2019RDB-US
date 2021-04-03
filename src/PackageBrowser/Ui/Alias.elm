module PackageBrowser.Ui.Alias exposing (..)

import Element


type alias Element msg =
    Element.Element msg


none =
    Element.none


text =
    Element.text


el =
    Element.el



--


row =
    Element.row


wrappedRow =
    Element.wrappedRow


column =
    Element.column



--


paragraph a =
    Element.paragraph (width fill :: spacing 0 :: a)


textColumn a =
    Element.textColumn (width shrink :: a)
