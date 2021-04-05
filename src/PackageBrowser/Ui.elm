module PackageBrowser.Ui exposing (..)

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html
import Html.Attributes



-- Basic Elements


type alias Element msg =
    Element.Element msg


none =
    Element.none


text =
    Element.text


el =
    Element.el



-- Rows and Columns


row =
    Element.row


wrappedRow =
    Element.wrappedRow


column =
    Element.column



-- Text Layout


p a =
    Element.paragraph (spacing1 :: a)


textColumn a =
    Element.textColumn (Element.width Element.fill :: a)


status a =
    p (padding2 :: fontCenter :: Font.color gray6 :: a)


h1 a =
    p (Region.heading 1 :: Font.size 40 :: a)


h2 a =
    p (Region.heading 2 :: Font.size 32 :: a)


h3 a =
    p (Region.heading 3 :: Font.size 28 :: a)


h4 a =
    p (Region.heading 4 :: Font.size 24 :: a)


h5 a =
    p (Region.heading 5 :: Font.size 20 :: a)


h6 a =
    p (Region.heading 6 :: Font.size 16 :: a)



-- Data Table


table : List (Element.Attribute msg) -> { data : List a, columns : List (Element.Column a msg) } -> Element msg
table =
    Element.table


indexedTable : List (Element.Attribute msg) -> { data : List a, columns : List (Element.IndexedColumn a msg) } -> Element msg
indexedTable =
    Element.indexedTable



-- Size


width =
    Element.width


height =
    Element.height


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



-- Padding and Spacing


padding a =
    Element.padding (a * 8)


paddingXY a b =
    Element.paddingXY (a * 8) (b * 8)


paddingEach a b c d =
    Element.paddingEach { top = a * 8, right = b * 8, bottom = c * 8, left = d * 8 }


spacing a =
    Element.spacing (a * 8)


spacingXY a b =
    Element.spacingXY (a * 8) (b * 8)


spaceEvenly =
    Element.spaceEvenly



-- Alignment


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



-- Root


rootStyle a =
    fontSize
        :: fontFamily
        :: Font.color gray9
        :: Background.color gray1
        :: a



-- Colors


gray0 =
    Element.rgb 1 1 1


gray1 =
    Element.rgb 0.97 0.98 0.98


gray2 =
    Element.rgb 0.91 0.93 0.94


gray3 =
    Element.rgb 0.87 0.89 0.9


gray4 =
    Element.rgb 0.81 0.83 0.85


gray5 =
    Element.rgb 0.68 0.71 0.74


gray6 =
    Element.rgb 0.42 0.46 0.49


gray7 =
    Element.rgb 0.29 0.31 0.34


gray8 =
    Element.rgb 0.2 0.23 0.25


gray9 =
    Element.rgb 0.13 0.15 0.16


gray10 =
    Element.rgb 0 0 0


primary =
    Element.rgb 0.05 0.43 0.99


secondary =
    gray6


success =
    Element.rgb 0.1 0.53 0.33


info =
    Element.rgb 0.05 0.79 0.94


warning =
    Element.rgb 1 0.76 0.03


danger =
    Element.rgb 0.86 0.21 0.27



-- FONT


fontSize =
    Font.size 16


fontFamily =
    Font.family
        [ Font.typeface "system-ui"
        , Font.typeface "-apple-system"
        , Font.typeface "Segoe UI"
        , Font.typeface "Roboto"
        , Font.typeface "Helvetica Neue"
        , Font.typeface "Arial"
        , Font.typeface "Noto Sans"
        , Font.typeface "Liberation Sans"
        , Font.sansSerif
        , Font.typeface "Apple Color Emoji"
        , Font.typeface "Segoe UI Emoji"
        , Font.typeface "Segoe UI Symbol"
        , Font.typeface "Noto Color Emoji"
        ]


fontFamilyMonospace =
    Font.family
        [ Font.typeface "SFMono-Regular"
        , Font.typeface "Menlo"
        , Font.typeface "Monaco"
        , Font.typeface "Consolas"
        , Font.typeface "Liberation Mono"
        , Font.typeface "Courier New"
        , Font.monospace
        ]


fontLeft =
    Font.alignLeft


fontRight =
    Font.alignRight


fontCenter =
    Font.center



-- BORDER


edges =
    { left = 0, right = 0, top = 0, bottom = 0 }


border =
    Border.width 1


borderLeft =
    Border.widthEach { edges | left = 1 }


borderRight =
    Border.widthEach { edges | right = 1 }


borderTop =
    Border.widthEach { edges | top = 1 }


borderBottom =
    Border.widthEach { edges | bottom = 1 }


borderRounded05 =
    Border.rounded 4


borderRounded1 =
    Border.rounded 8


borderRounded2 =
    Border.rounded 16


borderShadow =
    Border.shadow
        { offset = ( 0, 16 )
        , size = 0
        , blur = 48
        , color = gray10 |> Element.toRgb |> (\v -> { v | alpha = 0.2 }) |> Element.fromRgb
        }



-- SPACING


spacing05 =
    Element.spacing 4


spacing1 =
    Element.spacing 8


spacing2 =
    Element.spacing 16


spacing3 =
    Element.spacing 24


spacing4 =
    Element.spacing 32



-- PADDING


padding05 =
    Element.padding 4


padding1 =
    Element.padding 8


padding2 =
    Element.padding 16


padding3 =
    Element.padding 24


padding4 =
    Element.padding 32



-- ELEMENTS THAT SHRINK WIDTH


br =
    Element.html (Html.br [] [])


link : List (Element.Attr () msg) -> { label : Element.Element msg, url : String } -> Element.Element msg
link a =
    Element.link (Font.color primary :: a)


image : List (Element.Attribute msg) -> { description : String, src : String } -> Element.Element msg
image =
    Element.image


newTabLink : List (Element.Attr () msg) -> { label : Element.Element msg, url : String } -> Element.Element msg
newTabLink a =
    Element.newTabLink (Font.color primary :: a)


buttonLink : List (Element.Attribute msg) -> { label : Element.Element msg, onPress : Maybe msg } -> Element.Element msg
buttonLink a =
    Input.button
        (Font.color primary
            :: borderRounded05
            :: a
        )



-- ATTRIBUTES


noneAttribute =
    Element.htmlAttribute (Html.Attributes.classList [])


id a =
    Element.htmlAttribute (Html.Attributes.id a)



-- INPUTS


inputStyle a =
    padding1
        :: spacing1
        :: Background.color gray0
        :: Border.color gray4
        :: border
        :: borderRounded05
        :: a


searchInput a =
    Input.search (inputStyle a)


placeholder a =
    Input.placeholder (Font.size 14 :: a)


labelHidden =
    Input.labelHidden


labelAbove a =
    Input.labelAbove (Font.size 12 :: a)
