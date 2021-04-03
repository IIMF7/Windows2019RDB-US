module PackageBrowser.Ui.Alias exposing (..)

import Element
import Element.Background
import Element.Border
import Element.Events
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



-- ELEMENT MODULE


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


noStaticStyleSheet : Option
noStaticStyleSheet =
    Element.noStaticStyleSheet


forceHover : Option
forceHover =
    Element.forceHover


noHover : Option
noHover =
    Element.noHover


focusStyle : FocusStyle -> Option
focusStyle =
    Element.focusStyle


type alias FocusStyle =
    Element.FocusStyle


link : List (Attribute msg) -> { label : Element msg, url : String } -> Element msg
link =
    Element.link


newTabLink : List (Attribute msg) -> { label : Element msg, url : String } -> Element msg
newTabLink =
    Element.newTabLink


download : List (Attribute msg) -> { label : Element msg, url : String } -> Element msg
download =
    Element.download


downloadAs : List (Attribute msg) -> { label : Element msg, url : String, filename : String } -> Element msg
downloadAs =
    Element.downloadAs


image : List (Attribute msg) -> { description : String, src : String } -> Element msg
image =
    Element.image


type alias Color =
    Element.Color


rgba : Float -> Float -> Float -> Float -> Color
rgba =
    Element.rgba


rgb : Float -> Float -> Float -> Color
rgb =
    Element.rgb


rgb255 : Int -> Int -> Int -> Color
rgb255 =
    Element.rgb255


rgba255 : Int -> Int -> Int -> Float -> Color
rgba255 =
    Element.rgba255


fromRgb : { red : Float, green : Float, blue : Float, alpha : Float } -> Color
fromRgb =
    Element.fromRgb


fromRgb255 : { red : Int, green : Int, blue : Int, alpha : Float } -> Color
fromRgb255 =
    Element.fromRgb255


toRgb : Color -> { red : Float, green : Float, blue : Float, alpha : Float }
toRgb =
    Element.toRgb


above : Element msg -> Attribute msg
above =
    Element.above


below : Element msg -> Attribute msg
below =
    Element.below


onRight : Element msg -> Attribute msg
onRight =
    Element.onRight


onLeft : Element msg -> Attribute msg
onLeft =
    Element.onLeft


inFront : Element msg -> Attribute msg
inFront =
    Element.inFront


behindContent : Element msg -> Attribute msg
behindContent =
    Element.behindContent


type alias Attr decorative msg =
    Element.Attr decorative msg


type alias Decoration =
    Element.Decoration


mouseOver : List Decoration -> Attribute msg
mouseOver =
    Element.mouseOver


mouseDown : List Decoration -> Attribute msg
mouseDown =
    Element.mouseDown


focused : List Decoration -> Attribute msg
focused =
    Element.focused


type alias Device =
    Element.Device


type alias DeviceClass =
    Element.DeviceClass


type alias Orientation =
    Element.Orientation


classifyDevice : { window | height : Int, width : Int } -> Device
classifyDevice =
    Element.classifyDevice


modular : Float -> Float -> Int -> Float
modular =
    Element.modular


map : (msg -> msg1) -> Element msg -> Element msg1
map =
    Element.map


mapAttribute : (a -> b) -> Attribute a -> Attribute b
mapAttribute =
    Element.mapAttribute


html : Html.Html msg -> Element msg
html =
    Element.html


htmlAttribute : Html.Attribute msg -> Attribute msg
htmlAttribute =
    Element.htmlAttribute



-- BACKGROUND MODULE


bgColor : Color -> Attr decorative msg
bgColor =
    Element.Background.color


bgGradient : { angle : Float, steps : List Color } -> Attr decorative msg
bgGradient =
    Element.Background.gradient


bgImage : String -> Attribute msg
bgImage =
    Element.Background.image


bgUncropped : String -> Attribute msg
bgUncropped =
    Element.Background.uncropped


bgTiled : String -> Attribute msg
bgTiled =
    Element.Background.tiled


bgTiledX : String -> Attribute msg
bgTiledX =
    Element.Background.tiledX


bgTiledY : String -> Attribute msg
bgTiledY =
    Element.Background.tiledY



-- BORDER MODULE


borderColor : Color -> Attr decorative msg
borderColor =
    Element.Border.color


borderWidth : Int -> Attribute msg
borderWidth =
    Element.Border.width


borderWidthXY : Int -> Int -> Attribute msg
borderWidthXY =
    Element.Border.widthXY


borderWidthEach : Int -> Int -> Int -> Int -> Attribute msg
borderWidthEach a b c d =
    Element.Border.widthEach { left = a, right = b, top = c, bottom = d }


borderSolid : Attribute msg
borderSolid =
    Element.Border.solid


borderDashed : Attribute msg
borderDashed =
    Element.Border.dashed


borderDotted : Attribute msg
borderDotted =
    Element.Border.dotted


borderRounded : Int -> Attribute msg
borderRounded =
    Element.Border.rounded


borderRoundEach : Int -> Int -> Int -> Int -> Attribute msg
borderRoundEach a b c d =
    Element.Border.roundEach { topLeft = a, topRight = b, bottomLeft = c, bottomRight = d }


borderGlow : Color -> Float -> Attr decorative msg
borderGlow =
    Element.Border.glow


borderInnerGlow : Color -> Float -> Attr decorative msg
borderInnerGlow =
    Element.Border.innerGlow


borderShadow : { offset : ( Float, Float ), size : Float, blur : Float, color : Color } -> Attr decorative msg
borderShadow =
    Element.Border.shadow


borderInnerShadow : { offset : ( Float, Float ), size : Float, blur : Float, color : Color } -> Attr decorative msg
borderInnerShadow =
    Element.Border.innerShadow



-- EVENTS MODULE


onClick : msg -> Attribute msg
onClick =
    Element.Events.onClick


onDoubleClick : msg -> Attribute msg
onDoubleClick =
    Element.Events.onDoubleClick


onMouseDown : msg -> Attribute msg
onMouseDown =
    Element.Events.onMouseDown


onMouseUp : msg -> Attribute msg
onMouseUp =
    Element.Events.onMouseUp


onMouseEnter : msg -> Attribute msg
onMouseEnter =
    Element.Events.onMouseEnter


onMouseLeave : msg -> Attribute msg
onMouseLeave =
    Element.Events.onMouseLeave


onMouseMove : msg -> Attribute msg
onMouseMove =
    Element.Events.onMouseMove


onFocus : msg -> Attribute msg
onFocus =
    Element.Events.onFocus


onLoseFocus : msg -> Attribute msg
onLoseFocus =
    Element.Events.onLoseFocus
