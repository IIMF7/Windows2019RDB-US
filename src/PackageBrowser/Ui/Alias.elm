module PackageBrowser.Ui.Alias exposing (..)

import Element


type alias Element msg =
    Element.Element msg



--


none =
    Element.none


text =
    Element.text



-- SHRINK BY DEFAULT


el =
    Element.el


row =
    Element.row


wrappedRow =
    Element.wrappedRow


column =
    Element.column


textColumn a =
    Element.textColumn (width shrink :: a)



-- FILL WIDTH BY DEFAULT


paragraph a =
    Element.paragraph (spacing 8 :: a)


type alias Column record msg =
    Element.Column record msg


table =
    Element.table


type alias IndexedColumn =
    IndexedColumn


indexedTable =
    Element.indexedTable
