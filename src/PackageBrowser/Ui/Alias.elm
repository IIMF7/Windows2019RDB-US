module PackageBrowser.Ui.Alias exposing (..)

import Element
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Element.Keyed
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



-- FONT MODULE


fontColor : Color -> Attr decorative msg
fontColor =
    Element.Font.color


fontSize : Int -> Attr decorative msg
fontSize =
    Element.Font.size


fontFamily : List Font -> Attribute msg
fontFamily =
    Element.Font.family


type alias Font =
    Element.Font.Font


fontTypeface : String -> Font
fontTypeface =
    Element.Font.typeface


fontSerif : Font
fontSerif =
    Element.Font.serif


fontSansSerif : Font
fontSansSerif =
    Element.Font.sansSerif


fontMonospace : Font
fontMonospace =
    Element.Font.monospace


fontExternal : { name : String, url : String } -> Font
fontExternal =
    Element.Font.external


fontAlignLeft : Attribute msg
fontAlignLeft =
    Element.Font.alignLeft


fontAlignRight : Attribute msg
fontAlignRight =
    Element.Font.alignRight


fontCenter : Attribute msg
fontCenter =
    Element.Font.center


fontJustify : Attribute msg
fontJustify =
    Element.Font.justify


fontLetterSpacing : Float -> Attribute msg
fontLetterSpacing =
    Element.Font.letterSpacing


fontWordSpacing : Float -> Attribute msg
fontWordSpacing =
    Element.Font.wordSpacing


fontUnderline : Attribute msg
fontUnderline =
    Element.Font.underline


fontStrike : Attribute msg
fontStrike =
    Element.Font.strike


fontItalic : Attribute msg
fontItalic =
    Element.Font.italic


fontUnitalicized : Attribute msg
fontUnitalicized =
    Element.Font.unitalicized


fontHeavy : Attribute msg
fontHeavy =
    Element.Font.heavy


fontExtraBold : Attribute msg
fontExtraBold =
    Element.Font.extraBold


fontBold : Attribute msg
fontBold =
    Element.Font.bold


fontSemiBold : Attribute msg
fontSemiBold =
    Element.Font.semiBold


fontMedium : Attribute msg
fontMedium =
    Element.Font.medium


fontRegular : Attribute msg
fontRegular =
    Element.Font.regular


fontLight : Attribute msg
fontLight =
    Element.Font.light


fontExtraLight : Attribute msg
fontExtraLight =
    Element.Font.extraLight


fontHairline : Attribute msg
fontHairline =
    Element.Font.hairline


type alias FontVariant =
    Element.Font.Variant


fontVariant : FontVariant -> Attribute msg
fontVariant =
    Element.Font.variant


fontVariantList : List FontVariant -> Attribute msg
fontVariantList =
    Element.Font.variantList


fontSmallCaps : FontVariant
fontSmallCaps =
    Element.Font.smallCaps


fontSlashedZero : FontVariant
fontSlashedZero =
    Element.Font.slashedZero


fontLigatures : FontVariant
fontLigatures =
    Element.Font.ligatures


fontOrdinal : FontVariant
fontOrdinal =
    Element.Font.ordinal


fontTabularNumbers : FontVariant
fontTabularNumbers =
    Element.Font.tabularNumbers


fontStackedFractions : FontVariant
fontStackedFractions =
    Element.Font.stackedFractions


fontDiagonalFractions : FontVariant
fontDiagonalFractions =
    Element.Font.diagonalFractions


fontSwash : Int -> FontVariant
fontSwash =
    Element.Font.swash


fontFeature : String -> Bool -> FontVariant
fontFeature =
    Element.Font.feature


fontIndexed : String -> Int -> FontVariant
fontIndexed =
    Element.Font.indexed


fontGlow : Color -> Float -> Attr decorative msg
fontGlow =
    Element.Font.glow


fontShadow : { offset : ( Float, Float ), blur : Float, color : Color } -> Attr decorative msg
fontShadow =
    Element.Font.shadow



-- INPUT MODULE


inputFocusedOnLoad : Attribute msg
inputFocusedOnLoad =
    Element.Input.focusedOnLoad


inputButton : List (Attribute msg) -> { label : Element msg, onPress : Maybe msg } -> Element msg
inputButton =
    Element.Input.button


inputCheckbox :
    List (Attribute msg)
    ->
        { icon : Bool -> Element msg
        , label : InputLabel msg
        , checked : Bool
        , onChange : Bool -> msg
        }
    -> Element msg
inputCheckbox =
    Element.Input.checkbox


inputDefaultCheckbox : Bool -> Element msg
inputDefaultCheckbox =
    Element.Input.defaultCheckbox


inputText :
    List (Attribute msg)
    ->
        { label : InputLabel msg
        , placeholder : Maybe (InputPlaceholder msg)
        , text : String
        , onChange : String -> msg
        }
    -> Element msg
inputText =
    Element.Input.text


inputMultiline :
    List (Attribute msg)
    ->
        { label : InputLabel msg
        , placeholder : Maybe (InputPlaceholder msg)
        , text : String
        , spellcheck : Bool
        , onChange : String -> msg
        }
    -> Element msg
inputMultiline =
    Element.Input.multiline


type alias InputPlaceholder msg =
    Element.Input.Placeholder msg


inputPlaceholder : List (Attribute msg) -> Element msg -> InputPlaceholder msg
inputPlaceholder =
    Element.Input.placeholder


inputUsername :
    List (Attribute msg)
    ->
        { label : InputLabel msg
        , placeholder : Maybe (InputPlaceholder msg)
        , text : String
        , onChange : String -> msg
        }
    -> Element msg
inputUsername =
    Element.Input.username


inputNewPassword :
    List (Attribute msg)
    ->
        { label : InputLabel msg
        , placeholder : Maybe (InputPlaceholder msg)
        , text : String
        , show : Bool
        , onChange : String -> msg
        }
    -> Element msg
inputNewPassword =
    Element.Input.newPassword


inputCurrentPassword :
    List (Attribute msg)
    ->
        { label : InputLabel msg
        , placeholder : Maybe (InputPlaceholder msg)
        , text : String
        , onChange : String -> msg
        , show : Bool
        }
    -> Element msg
inputCurrentPassword =
    Element.Input.currentPassword


inputEmail :
    List (Attribute msg)
    ->
        { label : InputLabel msg
        , placeholder : Maybe (InputPlaceholder msg)
        , text : String
        , onChange : String -> msg
        }
    -> Element msg
inputEmail =
    Element.Input.email


inputSearch :
    List (Attribute msg)
    ->
        { label : InputLabel msg
        , placeholder : Maybe (InputPlaceholder msg)
        , text : String
        , onChange : String -> msg
        }
    -> Element msg
inputSearch =
    Element.Input.search


inputSpellChecked :
    List (Attribute msg)
    ->
        { label : InputLabel msg
        , placeholder : Maybe (InputPlaceholder msg)
        , text : String
        , onChange : String -> msg
        }
    -> Element msg
inputSpellChecked =
    Element.Input.spellChecked


inputSlider :
    List (Attribute msg)
    ->
        { label : InputLabel msg
        , thumb : InputThumb
        , min : Float
        , max : Float
        , step : Maybe Float
        , value : Float
        , onChange : Float -> msg
        }
    -> Element msg
inputSlider =
    Element.Input.slider


type alias InputThumb =
    Element.Input.Thumb


inputThumb : List (Attribute Never) -> InputThumb
inputThumb =
    Element.Input.thumb


inputDefaultThumb : InputThumb
inputDefaultThumb =
    Element.Input.defaultThumb


inputRadio :
    List (Attribute msg)
    ->
        { label : InputLabel msg
        , options : List (InputOption a msg)
        , selected : Maybe a
        , onChange : a -> msg
        }
    -> Element msg
inputRadio =
    Element.Input.radio


inputRadioRow :
    List (Attribute msg)
    ->
        { label : InputLabel msg
        , options : List (InputOption a msg)
        , selected : Maybe a
        , onChange : a -> msg
        }
    -> Element msg
inputRadioRow =
    Element.Input.radioRow


type alias InputOption a msg =
    Element.Input.Option a msg


inputOption : a -> Element msg -> InputOption a msg
inputOption =
    Element.Input.option


inputOptionWith : a -> (InputOptionState -> Element msg) -> InputOption a msg
inputOptionWith =
    Element.Input.optionWith


type alias InputOptionState =
    Element.Input.OptionState


type alias InputLabel msg =
    Element.Input.Label msg


inputLabelAbove : List (Attribute msg) -> Element msg -> InputLabel msg
inputLabelAbove =
    Element.Input.labelAbove


inputLabelBelow : List (Attribute msg) -> Element msg -> InputLabel msg
inputLabelBelow =
    Element.Input.labelBelow


inputLabelLeft : List (Attribute msg) -> Element msg -> InputLabel msg
inputLabelLeft =
    Element.Input.labelLeft


inputLabelRight : List (Attribute msg) -> Element msg -> InputLabel msg
inputLabelRight =
    Element.Input.labelRight


inputLabelHidden : String -> InputLabel msg
inputLabelHidden =
    Element.Input.labelHidden



-- KEYED MODULE


keyedEl : List (Attribute msg) -> ( String, Element msg ) -> Element msg
keyedEl =
    Element.Keyed.el


keyedColumn : List (Attribute msg) -> List ( String, Element msg ) -> Element msg
keyedColumn =
    Element.Keyed.column


keyedRow : List (Attribute msg) -> List ( String, Element msg ) -> Element msg
keyedRow =
    Element.Keyed.row
