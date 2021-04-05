module PackageBrowser.Ui exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html
import Html.Attributes



-- ROOT


rootStyle a =
    fontSize
        :: fontFamily
        :: fontColor
        :: bgColor
        :: a



-- COLORS


white =
    rgb 1 1 1


gray1 =
    rgb 0.97 0.98 0.98


gray2 =
    rgb 0.91 0.93 0.94


gray3 =
    rgb 0.87 0.89 0.9


gray4 =
    rgb 0.81 0.83 0.85


gray5 =
    rgb 0.68 0.71 0.74


gray6 =
    rgb 0.42 0.46 0.49


gray7 =
    rgb 0.29 0.31 0.34


gray8 =
    rgb 0.2 0.23 0.25


gray9 =
    rgb 0.13 0.15 0.16


black =
    rgb 0 0 0



-- COLORS


blue =
    rgb 0.05 0.43 0.99


indigo =
    rgb 0.4 0.06 0.95


purple =
    rgb 0.44 0.26 0.76


pink =
    rgb 0.84 0.2 0.52


red =
    rgb 0.86 0.21 0.27


orange =
    rgb 0.99 0.49 0.08


yellow =
    rgb 1 0.76 0.03


green =
    rgb 0.1 0.53 0.33


teal =
    rgb 0.13 0.79 0.59


cyan =
    rgb 0.05 0.79 0.94



-- COLORS


primary =
    blue


secondary =
    gray6


success =
    green


info =
    cyan


warning =
    yellow


danger =
    red


light =
    gray1


dark =
    gray9



-- FONT


fontColor =
    Font.color gray9


fontColorMuted =
    Font.color gray6


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



-- BACKGROUND


bgColor =
    Background.color gray1



-- BORDER


borderColor =
    Border.color gray3


borderColorInput =
    Border.color gray4


borderRounded =
    Border.rounded 4


borderShadow =
    Border.shadow
        { offset = ( 0, 16 )
        , size = 0
        , blur = 48
        , color = black |> toRgb |> (\v -> { v | alpha = 0.2 }) |> fromRgb
        }



-- ELEMENTS


type alias Element msg =
    Element.Element msg


row =
    Element.row


column =
    Element.column



--


p a =
    paragraph (spacing 8 :: a)


status a =
    p (Element.padding 16 :: Font.center :: fontColorMuted :: a)


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



--


none =
    Element.none


noneAttribute =
    Element.htmlAttribute (Html.Attributes.classList [])


text =
    Element.text


el =
    Element.el


br =
    html (Html.br [] [])


image =
    Element.image


link a =
    Element.link (Font.color primary :: a)


newTabLink a =
    Element.newTabLink (Font.color primary :: a)


buttonLink a =
    Input.button
        (Font.color primary
            :: Border.rounded 2
            :: a
        )



--


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
    padding 8
        :: spacing 8
        :: Background.color white
        :: borderColorInput
        :: border
        :: borderRounded
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
            :: width (shrink |> Element.maximum 512)
            :: Background.color white
            :: borderShadow
            :: border
            :: borderColor
            :: Border.rounded 16
            :: a
        )
