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
    Element.paragraph (spacing 1 :: a)


textColumn a =
    Element.textColumn (Element.width Element.fill :: a)


status a =
    p (padding 2 :: fontCenter :: Font.color gray6 :: a)


h1 a =
    p (Region.heading 1 :: fontSize 1.5 :: a)


h2 a =
    p (Region.heading 2 :: fontSize 2 :: a)


h3 a =
    p (Region.heading 3 :: fontSize 1.75 :: a)


h4 a =
    p (Region.heading 4 :: fontSize 1.5 :: a)


h5 a =
    p (Region.heading 5 :: fontSize 1.25 :: a)


h6 a =
    p (Region.heading 6 :: fontSize 1 :: a)



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
    Element.padding (mul8 a)


paddingXY a b =
    Element.paddingXY (mul8 a) (mul8 b)


paddingEach a b c d =
    Element.paddingEach { left = mul8 a, right = mul8 b, top = mul8 c, bottom = mul8 d }


spacing a =
    Element.spacing (mul8 a)


spacingXY a b =
    Element.spacingXY (mul8 a) (mul8 b)


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



-- Transparency


transparent =
    Element.transparent


alpha =
    Element.alpha



-- Links


link : List (Element.Attribute msg) -> { label : Element msg, url : String } -> Element msg
link =
    Element.link


newTabLink : List (Element.Attribute msg) -> { label : Element msg, url : String } -> Element msg
newTabLink =
    Element.newTabLink


download : List (Element.Attribute msg) -> { label : Element msg, url : String } -> Element msg
download =
    Element.download


downloadAs : List (Element.Attribute msg) -> { label : Element msg, filename : String, url : String } -> Element msg
downloadAs =
    Element.downloadAs



-- Images


image : List (Element.Attribute msg) -> { description : String, src : String } -> Element msg
image =
    Element.image



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



-- Fonts


fontSize a =
    Font.size (mul16 a)


fontFamilyDefault =
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



-- Borders


borderWidth a =
    Border.width a


borderWidthEach a b c d =
    Border.widthEach { left = a, right = b, top = c, bottom = d }


borderRounded a =
    Border.rounded (mul8 a)


borderShadow a =
    Border.shadow
        { offset = ( 0, 16 )
        , size = 0
        , blur = a * 16
        , color = gray10 |> Element.toRgb |> (\v -> { v | alpha = 0.2 }) |> Element.fromRgb
        }



-- Extras


br =
    Element.html (Html.br [] [])


noneAttribute =
    Element.htmlAttribute (Html.Attributes.classList [])


id a =
    Element.htmlAttribute (Html.Attributes.id a)



-- Inputs


inputStyle a =
    padding 1
        :: spacing 1
        :: Background.color gray0
        :: Border.color gray4
        :: borderWidth 1
        :: borderRounded 0.5
        :: a


buttonLink : List (Element.Attribute msg) -> { label : Element msg, onPress : Maybe msg } -> Element msg
buttonLink a =
    Input.button
        (Font.color primary
            :: borderRounded 0.5
            :: a
        )


searchInput a =
    Input.search (inputStyle a)


placeholder a =
    Input.placeholder (Font.size 14 :: a)


labelHidden =
    Input.labelHidden


labelAbove a =
    Input.labelAbove (Font.size 12 :: a)



-- Root


rootStyle a =
    fontSize 1
        :: fontFamilyDefault
        :: Font.color gray9
        :: Background.color gray1
        :: a



-- Helpers


mul8 : Float -> Int
mul8 a =
    a * 8 |> round


mul16 : Float -> Int
mul16 a =
    a * 16 |> round
