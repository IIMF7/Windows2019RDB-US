module PackageBrowser.Ui.Base exposing (..)

import Element
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Element.Keyed
import Element.Lazy
import Element.Region
import Html
import Html.Attributes
import PackageBrowser.Ui.Style as A


layout a =
    Element.layout (bgColor A.baseBgColor :: fontColor A.baseColor :: fontSize A.baseFontSize :: A.baseFontFamily :: a)


layoutWith opt a =
    Element.layoutWith opt (bgColor A.baseBgColor :: fontColor A.baseColor :: fontSize A.baseFontSize :: A.baseFontFamily :: a)


p a =
    Element.paragraph (spacing A.baseLineSpacing :: a)


textColumn a =
    Element.textColumn (spacing A.baseLineSpacing :: width fill :: a)


link a =
    Element.link (fontColor A.linkColor :: a)


link_ a =
    Element.Input.button (fontColor A.linkColor :: focused [ borderColor (rgba 0 0 0 1) ] :: a)


newTabLink a =
    Element.newTabLink (fontColor A.linkColor :: a)


download a =
    Element.download (fontColor A.linkColor :: a)


downloadAs a =
    Element.downloadAs (fontColor A.linkColor :: a)


h1 a =
    p (regionHeading 1 :: fontSize A.h1FontSize :: a)


h2 a =
    p (regionHeading 2 :: fontSize A.h2FontSize :: a)


h3 a =
    p (regionHeading 3 :: fontSize A.h3FontSize :: a)


h4 a =
    p (regionHeading 4 :: fontSize A.h4FontSize :: a)


h5 a =
    p (regionHeading 5 :: fontSize A.h5FontSize :: a)


h6 a =
    p (regionHeading 6 :: fontSize A.h6FontSize :: a)


br =
    html (Html.br [] [])


hr =
    el [ width fill, paddingXY 0 (rem 1) ] (el [ width fill, borderWidthEach 0 0 0 1, borderColor A.hrBorderColor ] none)


id a =
    htmlAttribute (Html.Attributes.id a)


noneAttribute =
    htmlAttribute (Html.Attributes.classList [])


labelLeft a =
    Element.Input.labelLeft (fontColor A.labelColor :: fontSize A.labelFontSize :: a)


labelRight a =
    Element.Input.labelRight (fontColor A.labelColor :: fontSize A.labelFontSize :: a)


labelAbove a =
    Element.Input.labelAbove (fontColor A.labelColor :: fontSize A.labelFontSize :: a)


labelBelow a =
    Element.Input.labelBelow (fontColor A.labelColor :: fontSize A.labelFontSize :: a)


inputText a =
    Element.Input.text (spacing A.inputSpacing :: padding A.inputPadding :: bgColor A.inputBgColor :: fontColor A.inputColor :: borderColor A.inputBorderColor :: borderWidth A.inputBorderWidth :: borderRounded A.inputBorderRounded :: a)


inputMultiline a =
    Element.Input.multiline (spacing A.inputSpacing :: padding A.inputPadding :: bgColor A.inputBgColor :: fontColor A.inputColor :: borderColor A.inputBorderColor :: borderWidth A.inputBorderWidth :: borderRounded A.inputBorderRounded :: a)


inputSearch a =
    Element.Input.search (spacing A.inputSpacing :: padding A.inputPadding :: bgColor A.inputBgColor :: fontColor A.inputColor :: borderColor A.inputBorderColor :: borderWidth A.inputBorderWidth :: borderRounded A.inputBorderRounded :: a)


inputPlaceholder a =
    Element.Input.placeholder (fontColor A.placeholderColor :: fontSize A.placeholderFontSize :: a)



--


shadow1 =
    A.shadow1


shadow2 =
    A.shadow2


shadow3 =
    A.shadow3


grey0 =
    A.grey0


grey1 =
    A.grey1


grey2 =
    A.grey2


grey3 =
    A.grey3


grey4 =
    A.grey4


grey5 =
    A.grey5


grey6 =
    A.grey6


grey7 =
    A.grey7


grey8 =
    A.grey8


grey9 =
    A.grey9


grey10 =
    A.grey10


primary =
    A.primary


secondary =
    A.secondary


success =
    A.success


info =
    A.info


warning =
    A.warning


danger =
    A.danger



--


type alias Attr decorative msg =
    Element.Attr decorative msg


type alias Attribute msg =
    Element.Attribute msg


type alias Color =
    Element.Color


type alias Column record msg =
    Element.Column record msg


type alias Decoration =
    Element.Decoration


type alias Device =
    Element.Device


type alias Element msg =
    Element.Element msg


type alias FocusStyle =
    Element.FocusStyle


type alias IndexedColumn record msg =
    Element.IndexedColumn record msg


type alias Length =
    Element.Length


type alias Option =
    Element.Option


above =
    Element.above


alignBottom =
    Element.alignBottom


alignLeft =
    Element.alignLeft


alignRight =
    Element.alignRight


alignTop =
    Element.alignTop


alpha =
    Element.alpha


behindContent =
    Element.behindContent


below =
    Element.below


centerX =
    Element.centerX


centerY =
    Element.centerY


classifyDevice =
    Element.classifyDevice


clip =
    Element.clip


clipX =
    Element.clipX


clipY =
    Element.clipY


column =
    Element.column


el =
    Element.el


explain =
    Element.explain


fill =
    Element.fill


fillPortion =
    Element.fillPortion


focusStyle =
    Element.focusStyle


focused =
    Element.focused


forceHover =
    Element.forceHover


fromRgb =
    Element.fromRgb


fromRgb255 =
    Element.fromRgb255


height =
    Element.height


html =
    Element.html


htmlAttribute =
    Element.htmlAttribute


image =
    Element.image


inFront =
    Element.inFront


indexedTable =
    Element.indexedTable


map =
    Element.map


mapAttribute =
    Element.mapAttribute


maximum =
    Element.maximum


minimum =
    Element.minimum


modular =
    Element.modular


mouseDown =
    Element.mouseDown


mouseOver =
    Element.mouseOver


moveDown =
    Element.moveDown


moveLeft =
    Element.moveLeft


moveRight =
    Element.moveRight


moveUp =
    Element.moveUp


noHover =
    Element.noHover


noStaticStyleSheet =
    Element.noStaticStyleSheet


none =
    Element.none


onLeft =
    Element.onLeft


onRight =
    Element.onRight


padding =
    Element.padding


paddingEach minX maxX minY maxY =
    Element.paddingEach { left = minX, right = maxX, top = minY, bottom = maxY }


paddingXY =
    Element.paddingXY


pointer =
    Element.pointer


px =
    Element.px


rgb =
    Element.rgb


rgb255 =
    Element.rgb255


rgba =
    Element.rgba


rgba255 =
    Element.rgba255


rotate =
    Element.rotate


row =
    Element.row


scale =
    Element.scale


scrollbarX =
    Element.scrollbarX


scrollbarY =
    Element.scrollbarY


scrollbars =
    Element.scrollbars


shrink =
    Element.shrink


spaceEvenly =
    Element.spaceEvenly


spacing =
    Element.spacing


spacingXY =
    Element.spacingXY


table =
    Element.table


text =
    Element.text


toRgb =
    Element.toRgb


transparent =
    Element.transparent


width =
    Element.width


wrappedRow =
    Element.wrappedRow



--


bgColor =
    Element.Background.color


bgGradient =
    Element.Background.gradient


bgImage =
    Element.Background.image


bgTiled =
    Element.Background.tiled


bgTiledX =
    Element.Background.tiledX


bgTiledY =
    Element.Background.tiledY


bgUncropped =
    Element.Background.uncropped



--


borderColor =
    Element.Border.color


borderDashed =
    Element.Border.dashed


borderDotted =
    Element.Border.dotted


borderGlow =
    Element.Border.glow


borderInnerGlow =
    Element.Border.innerGlow


borderInnerShadow =
    Element.Border.innerShadow


borderRoundEach topLeft topRight bottomLeft bottomRight =
    Element.Border.roundEach { topLeft = topLeft, topRight = topRight, bottomLeft = bottomLeft, bottomRight = bottomRight }


borderRounded =
    Element.Border.rounded


borderShadow =
    Element.Border.shadow


borderSolid =
    Element.Border.solid


borderWidth =
    Element.Border.width


borderWidthEach minX maxX minY maxY =
    Element.Border.widthEach { left = minX, right = maxX, top = minY, bottom = maxY }


borderWidthXY =
    Element.Border.widthXY



--


onClick =
    Element.Events.onClick


onDoubleClick =
    Element.Events.onDoubleClick


onFocus =
    Element.Events.onFocus


onLoseFocus =
    Element.Events.onLoseFocus


onMouseDown =
    Element.Events.onMouseDown


onMouseEnter =
    Element.Events.onMouseEnter


onMouseLeave =
    Element.Events.onMouseLeave


onMouseMove =
    Element.Events.onMouseMove


onMouseUp =
    Element.Events.onMouseUp



--


type alias Font =
    Element.Font.Font


type alias Variant =
    Element.Font.Variant


fontAlignLeft =
    Element.Font.alignLeft


fontAlignRight =
    Element.Font.alignRight


fontBold =
    Element.Font.bold


fontCenter =
    Element.Font.center


fontColor =
    Element.Font.color


fontDiagonalFractions =
    Element.Font.diagonalFractions


fontExternal =
    Element.Font.external


fontExtraBold =
    Element.Font.extraBold


fontExtraLight =
    Element.Font.extraLight


fontFamily =
    Element.Font.family


fontFeature =
    Element.Font.feature


fontGlow =
    Element.Font.glow


fontHairline =
    Element.Font.hairline


fontHeavy =
    Element.Font.heavy


fontIndexed =
    Element.Font.indexed


fontItalic =
    Element.Font.italic


fontJustify =
    Element.Font.justify


fontLetterSpacing =
    Element.Font.letterSpacing


fontLigatures =
    Element.Font.ligatures


fontLight =
    Element.Font.light


fontMedium =
    Element.Font.medium


fontMonospace =
    Element.Font.monospace


fontOrdinal =
    Element.Font.ordinal


fontRegular =
    Element.Font.regular


fontSansSerif =
    Element.Font.sansSerif


fontSemiBold =
    Element.Font.semiBold


fontSerif =
    Element.Font.serif


fontShadow =
    Element.Font.shadow


fontSize =
    Element.Font.size


fontSlashedZero =
    Element.Font.slashedZero


fontSmallCaps =
    Element.Font.smallCaps


fontStackedFractions =
    Element.Font.stackedFractions


fontStrike =
    Element.Font.strike


fontSwash =
    Element.Font.swash


fontTabularNumbers =
    Element.Font.tabularNumbers


fontTypeface =
    Element.Font.typeface


fontUnderline =
    Element.Font.underline


fontUnitalicized =
    Element.Font.unitalicized


fontVariant =
    Element.Font.variant


fontVariantList =
    Element.Font.variantList


fontWordSpacing =
    Element.Font.wordSpacing



--


inputButton =
    Element.Input.button


inputCheckbox =
    Element.Input.checkbox


inputCurrentPassword =
    Element.Input.currentPassword


inputDefaultCheckbox =
    Element.Input.defaultCheckbox


inputDefaultThumb =
    Element.Input.defaultThumb


inputEmail =
    Element.Input.email


inputFocusedOnLoad =
    Element.Input.focusedOnLoad


inputLabelHidden =
    Element.Input.labelHidden


inputNewPassword =
    Element.Input.newPassword


inputOption =
    Element.Input.option


inputOptionWith =
    Element.Input.optionWith


inputRadio =
    Element.Input.radio


inputRadioRow =
    Element.Input.radioRow


inputSlider =
    Element.Input.slider


inputSpellChecked =
    Element.Input.spellChecked


inputThumb =
    Element.Input.thumb


inputUsername =
    Element.Input.username



--


keyedColumn =
    Element.Keyed.column


keyedEl =
    Element.Keyed.el


keyedRow =
    Element.Keyed.row



--


lazy =
    Element.Lazy.lazy


lazy2 =
    Element.Lazy.lazy2


lazy3 =
    Element.Lazy.lazy3


lazy4 =
    Element.Lazy.lazy4


lazy5 =
    Element.Lazy.lazy5



--


regionAnnounce =
    Element.Region.announce


regionAnnounceUrgently =
    Element.Region.announceUrgently


regionAside =
    Element.Region.aside


regionDescription =
    Element.Region.description


regionFooter =
    Element.Region.footer


regionHeading =
    Element.Region.heading


regionMainContent =
    Element.Region.mainContent


regionNavigation =
    Element.Region.navigation
