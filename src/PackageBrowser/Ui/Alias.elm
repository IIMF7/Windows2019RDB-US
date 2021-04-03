module PackageBrowser.Ui.Alias exposing (..)

import Element



-- ELEMENTS THAT SHRINK BY DEFAULT


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



-- ELEMENTS THAT FILL WIDTH BY DEFAULT


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



-- THE REST


type alias Element msg =
    Element.Element msg


none =
    Element.none


text =
    Element.text


type alias Attribute msg =
    Element.Attribute msg


width =
    Element.width


height =
    Element.height


type alias Length =
    Element.Length


px =
    Element.px


shrink =
    Element.shrink


fill =
    Element.fill


fillPortion =
    Element.fillPortion


maximum =
    Element.maximum


minimum =
    Element.minimum


explain =
    Element.explain


padding =
    Element.padding


paddingXY =
    Element.paddingXY


paddingEach =
    Element.paddingEach


spacing =
    Element.spacing


spacingXY =
    Element.spacingXY


spaceEvenly =
    Element.spaceEvenly


centerX =
    Element.centerX


centerY =
    Element.centerY


alignLeft =
    Element.alignLeft


alignRight =
    Element.alignRight


alignTop =
    Element.alignTop


alignBottom =
    Element.alignBottom


transparent =
    Element.transparent


alpha =
    Element.alpha


pointer =
    Element.pointer


moveUp =
    Element.moveUp


moveDown =
    Element.moveDown


moveRight =
    Element.moveRight


moveLeft =
    Element.moveLeft


rotate =
    Element.rotate


scale =
    Element.scale


clip =
    Element.clip


clipX =
    Element.clipX


clipY =
    Element.clipY


scrollbars =
    Element.scrollbars


scrollbarX =
    Element.scrollbarX


scrollbarY =
    Element.scrollbarY


layout =
    Element.layout


layoutWith =
    Element.layoutWith


type alias Option =
    Element.Option


noStaticStyleSheet =
    Element.noStaticStyleSheet


forceHover =
    Element.forceHover


noHover =
    Element.noHover


focusStyle =
    Element.focusStyle


type alias FocusStyle =
    Element.FocusStyle


link =
    Element.link


newTabLink =
    Element.newTabLink


download =
    Element.download


downloadAs =
    Element.downloadAs


image =
    Element.image


type alias Color =
    Element.Color


rgba =
    Element.rgba


rgb =
    Element.rgb


rgb255 =
    Element.rgb255


rgba255 =
    Element.rgba255


fromRgb =
    Element.fromRgb


fromRgb255 =
    Element.fromRgb255


toRgb =
    Element.toRgb


above =
    Element.above


below =
    Element.below


onRight =
    Element.onRight


onLeft =
    Element.onLeft


inFront =
    Element.inFront


behindContent =
    Element.behindContent


type alias Attr decorative msg =
    Element.Attr decorative msg


type alias Decoration =
    Element.Decoration


mouseOver =
    Element.mouseOver


mouseDown =
    Element.mouseDown


focused =
    Element.focused


type alias Device =
    Element.Device


type alias DeviceClass =
    Element.DeviceClass


type alias Orientation =
    Element.Orientation


classifyDevice =
    Element.classifyDevice


modular =
    Element.modular


map =
    Element.map


mapAttribute =
    Element.mapAttribute


html =
    Element.html


htmlAttribute =
    Element.htmlAttribute
