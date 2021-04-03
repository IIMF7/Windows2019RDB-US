module PackageBrowser.Ui.Alias exposing (..)

import Element
import Html



-- ELEMENTS THAT SHRINK BY DEFAULT


el : List (Attribute msg) -> Element msg -> Element msg
el =
    Element.el


row : List (Attribute msg) -> List (Element msg) -> Element msg
row =
    Element.row


wrappedRow : List (Attribute msg) -> List (Element msg) -> Element msg
wrappedRow =
    Element.wrappedRow


column : List (Attribute msg) -> List (Element msg) -> Element msg
column =
    Element.column


textColumn : List (Attribute msg) -> List (Element msg) -> Element msg
textColumn a =
    Element.textColumn (width shrink :: a)



-- ELEMENTS THAT FILL WIDTH BY DEFAULT


paragraph : List (Attribute msg) -> List (Element msg) -> Element msg
paragraph a =
    Element.paragraph (spacing 8 :: a)


type alias Column a msg =
    { header : Element msg
    , width : Length
    , view : a -> Element msg
    }


table : List (Attribute msg) -> { data : List a, columns : List (Column a msg) } -> Element msg
table =
    Element.table


type alias IndexedColumn a msg =
    { header : Element msg
    , width : Length
    , view : Int -> a -> Element msg
    }


indexedTable : List (Attribute msg) -> { data : List a, columns : List (IndexedColumn a msg) } -> Element msg
indexedTable =
    Element.indexedTable



-- EXTRAS


br : Element msg
br =
    html (Html.br [] [])



-- THE REST


type alias Element msg =
    Element.Element msg


none : Element msg
none =
    Element.none


text : String -> Element msg
text =
    Element.text


type alias Attribute msg =
    Element.Attribute msg


width : Length -> Attribute msg
width =
    Element.width


height : Length -> Attribute msg
height =
    Element.height


type alias Length =
    Element.Length


px : Int -> Length
px =
    Element.px


shrink =
    Element.shrink


fill : Length
fill =
    Element.fill


fillPortion : Int -> Length
fillPortion =
    Element.fillPortion


maximum : Int -> Length -> Length
maximum =
    Element.maximum


minimum : Int -> Length -> Length
minimum =
    Element.minimum


explain : (String -> Never) -> Attribute msg
explain =
    Element.explain


padding : Int -> Attribute msg
padding =
    Element.padding


paddingXY : Int -> Int -> Attribute msg
paddingXY =
    Element.paddingXY


paddingEach : Int -> Int -> Int -> Int -> Attribute msg
paddingEach a b c d =
    Element.paddingEach { left = a, right = b, top = c, bottom = d }


spacing : Int -> Attribute msg
spacing =
    Element.spacing


spacingXY : Int -> Int -> Attribute msg
spacingXY =
    Element.spacingXY


spaceEvenly : Attribute msg
spaceEvenly =
    Element.spaceEvenly


centerX : Attribute msg
centerX =
    Element.centerX


centerY : Attribute msg
centerY =
    Element.centerY


alignLeft : Attribute msg
alignLeft =
    Element.alignLeft


alignRight : Attribute msg
alignRight =
    Element.alignRight


alignTop : Attribute msg
alignTop =
    Element.alignTop


alignBottom : Attribute msg
alignBottom =
    Element.alignBottom


transparent : Bool -> Attr decorative msg
transparent =
    Element.transparent


alpha : Float -> Attr decorative msg
alpha =
    Element.alpha


pointer : Attribute msg
pointer =
    Element.pointer


moveUp : Float -> Attr decorative msg
moveUp =
    Element.moveUp


moveDown : Float -> Attr decorative msg
moveDown =
    Element.moveDown


moveRight : Float -> Attr decorative msg
moveRight =
    Element.moveRight


moveLeft : Float -> Attr decorative msg
moveLeft =
    Element.moveLeft


rotate : Float -> Attr decorative msg
rotate =
    Element.rotate


scale : Float -> Attr decorative msg
scale =
    Element.scale


clip : Attribute msg
clip =
    Element.clip


clipX : Attribute msg
clipX =
    Element.clipX


clipY : Attribute msg
clipY =
    Element.clipY


scrollbars : Attribute msg
scrollbars =
    Element.scrollbars


scrollbarX : Attribute msg
scrollbarX =
    Element.scrollbarX


scrollbarY : Attribute msg
scrollbarY =
    Element.scrollbarY


layout : List (Attribute msg) -> Element msg -> Html.Html msg
layout =
    Element.layout


layoutWith : { options : List Option } -> List (Attribute msg) -> Element msg -> Html.Html msg
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
