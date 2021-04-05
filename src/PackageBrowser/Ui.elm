module PackageBrowser.Ui exposing (..)

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html
import Html.Attributes
import PackageBrowser.Ui.Colors as Colors



-- ROOT


rootStyle a =
    fontSize
        :: fontFamily
        :: fontColor
        :: bgColor
        :: a



-- FONT


fontColor =
    Font.color Colors.gray9


fontColorMuted =
    Font.color Colors.gray6


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



-- BACKGROUND


bgColor =
    Background.color Colors.gray1



-- BORDER


borderColor =
    Border.color Colors.gray3


borderColorInput =
    Border.color Colors.gray4


borderRounded1 =
    Border.rounded 4


borderRounded2 =
    Border.rounded 8


borderRounded3 =
    Border.rounded 16


borderShadow =
    Border.shadow
        { offset = ( 0, 16 )
        , size = 0
        , blur = 48
        , color = Colors.black |> Element.toRgb |> (\v -> { v | alpha = 0.2 }) |> Element.fromRgb
        }



-- SPACING


spacing1 =
    Element.spacing 4


spacing2 =
    Element.spacing 8


spacing3 =
    Element.spacing 16


padding1 =
    Element.padding 4


padding2 =
    Element.padding 8


padding3 =
    Element.padding 16



-- ELEMENTS


type alias Element msg =
    Element.Element msg


row =
    Element.row


column =
    Element.column



-- ELEMENTS THAT FILL WIDTH


p a =
    Element.paragraph (spacing2 :: a)


status a =
    p (padding3 :: Font.center :: fontColorMuted :: a)


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
    Element.link (Font.color Colors.primary :: a)


image : List (Element.Attribute msg) -> { description : String, src : String } -> Element.Element msg
image =
    Element.image


newTabLink : List (Element.Attr () msg) -> { label : Element.Element msg, url : String } -> Element.Element msg
newTabLink a =
    Element.newTabLink (Font.color Colors.primary :: a)


buttonLink : List (Element.Attribute msg) -> { label : Element.Element msg, onPress : Maybe msg } -> Element.Element msg
buttonLink a =
    Input.button
        (Font.color Colors.primary
            :: borderRounded1
            :: a
        )



-- ATTRIBUTES


noneAttribute =
    Element.htmlAttribute (Html.Attributes.classList [])


id a =
    Element.htmlAttribute (Html.Attributes.id a)



--


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


edges =
    { left = 0, right = 0, top = 0, bottom = 0 }



--


inputStyle a =
    Element.padding 8
        :: Element.spacing 8
        :: Background.color Colors.white
        :: borderColorInput
        :: border
        :: borderRounded1
        :: a


searchInput a =
    Input.search (inputStyle a)


placeholder a =
    Input.placeholder (Font.size 14 :: a)


labelHidden =
    Input.labelHidden


labelAbove a =
    Input.labelAbove (Font.size 12 :: a)



--


modal a =
    column
        (Element.spacing 32
            :: Element.width Element.fill
            :: Element.width (Element.shrink |> Element.maximum 512)
            :: Background.color Colors.white
            :: borderShadow
            :: border
            :: borderColor
            :: Border.rounded 16
            :: a
        )
