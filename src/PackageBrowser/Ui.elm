module PackageBrowser.Ui exposing (..)

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html
import Html.Attributes
import PackageBrowser.Ui.Color as Color



-- ROOT


rootStyle a =
    fontSize
        :: fontFamily
        :: Color.fontGray9
        :: Color.bgGray1
        :: a



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
        , color = Color.gray10 |> Element.toRgb |> (\v -> { v | alpha = 0.2 }) |> Element.fromRgb
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



-- ELEMENTS


type alias Element msg =
    Element.Element msg


row =
    Element.row


column =
    Element.column



-- ELEMENTS THAT FILL WIDTH


p a =
    Element.paragraph (spacing1 :: a)


status a =
    p (padding2 :: fontCenter :: Color.fontGray6 :: a)


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



-- ELEMENTS THAT SHRINK WIDTH


none =
    Element.none


text =
    Element.text


el =
    Element.el


br =
    Element.html (Html.br [] [])


link : List (Element.Attr () msg) -> { label : Element.Element msg, url : String } -> Element.Element msg
link a =
    Element.link (Color.fontPrimary :: a)


image : List (Element.Attribute msg) -> { description : String, src : String } -> Element.Element msg
image =
    Element.image


newTabLink : List (Element.Attr () msg) -> { label : Element.Element msg, url : String } -> Element.Element msg
newTabLink a =
    Element.newTabLink (Color.fontPrimary :: a)


buttonLink : List (Element.Attribute msg) -> { label : Element.Element msg, onPress : Maybe msg } -> Element.Element msg
buttonLink a =
    Input.button
        (Color.fontPrimary
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
        :: Color.bgGray0
        :: Color.borderGray4
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
