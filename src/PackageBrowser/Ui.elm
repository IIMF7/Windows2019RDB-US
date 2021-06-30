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
    Element.paragraph (spacing 0.5 :: a)


textColumn a =
    Element.textColumn (width fill :: a)


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


padding =
    Element.padding


paddingXY =
    Element.paddingXY


paddingEach a b c d =
    Element.paddingEach { left = a, right = b, top = c, bottom = d }


spacing a =
    Element.spacing (step a)


spacingXY a b =
    Element.spacingXY (step a) (step b)


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
link a =
    Element.link (fontColor primary :: a)


newTabLink : List (Element.Attribute msg) -> { label : Element msg, url : String } -> Element msg
newTabLink a =
    Element.newTabLink (fontColor primary :: a)


download : List (Element.Attribute msg) -> { label : Element msg, url : String } -> Element msg
download a =
    Element.download (fontColor primary :: a)


downloadAs : List (Element.Attribute msg) -> { label : Element msg, filename : String, url : String } -> Element msg
downloadAs a =
    Element.downloadAs (fontColor primary :: a)



-- Images


image : List (Element.Attribute msg) -> { description : String, src : String } -> Element msg
image =
    Element.image



-- Colors


grey10 =
    Element.rgb 1 1 1


grey9 =
    Element.rgb 0.96 0.97 0.97


grey8 =
    Element.rgb 0.9 0.92 0.93


grey7 =
    Element.rgb 0.86 0.88 0.89


grey6 =
    Element.rgb 0.8 0.82 0.84


grey5 =
    Element.rgb 0.67 0.7 0.73


grey4 =
    Element.rgb 0.41 0.45 0.48


grey3 =
    Element.rgb 0.28 0.3 0.33


grey2 =
    Element.rgb 0.19 0.22 0.24


grey1 =
    Element.rgb 0.12 0.14 0.15


grey0 =
    Element.rgb 0 0 0


primary =
    Element.rgb 0.05 0.43 0.99


secondary =
    grey4


success =
    Element.rgb 0.1 0.53 0.33


info =
    Element.rgb 0.05 0.79 0.94


warning =
    Element.rgb 1 0.76 0.03


danger =
    Element.rgb 0.86 0.21 0.27



-- Backgrounds


bgColor =
    Background.color



-- Font


fontColor =
    Font.color


fontSize a =
    Font.size (step a)



-- Font - Typefaces


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



-- Font - Alignment and Spacing


fontLeft =
    Font.alignLeft


fontRight =
    Font.alignRight


fontCenter =
    Font.center



-- Font - Font Styles


fontUnderline =
    Font.underline


fontStrike =
    Font.strike


fontItalic =
    Font.italic


fontUnitalicized =
    Font.unitalicized



-- Font - Font Weight


fontWeight a =
    case a of
        1 ->
            Font.hairline

        2 ->
            Font.extraLight

        3 ->
            Font.light

        4 ->
            Font.regular

        5 ->
            Font.medium

        6 ->
            Font.semiBold

        7 ->
            Font.bold

        8 ->
            Font.extraBold

        9 ->
            Font.heavy

        _ ->
            Font.regular



-- Borders


borderColor =
    Border.color


borderWidth =
    Border.width


borderWidthEach a b c d =
    Border.widthEach { left = a, right = b, top = c, bottom = d }


borderRounded a =
    Border.rounded (step a)


borderShadow a =
    Border.shadow
        { offset = ( 0, step_ 1 )
        , size = 0
        , blur = step_ a
        , color = grey0 |> Element.toRgb |> (\v -> { v | alpha = 0.2 }) |> Element.fromRgb
        }



-- Extras


br =
    Element.html (Html.br [] [])


hr =
    row [ width fill, paddingXY 0 16 ]
        [ el [ width fill, borderWidthEach 0 0 0 1, borderColor grey7 ] none
        ]


noneAttribute =
    Element.htmlAttribute (Html.Attributes.classList [])


id a =
    Element.htmlAttribute (Html.Attributes.id a)



-- Inputs


buttonLink : List (Element.Attribute msg) -> { label : Element msg, onPress : Maybe msg } -> Element msg
buttonLink a =
    Input.button
        (fontColor primary
            :: borderRounded 0.25
            :: Element.focused [ borderColor (Element.rgba 0 0 0 1) ]
            :: a
        )



--


inputStyle a =
    padding 8
        :: spacing 0.5
        :: bgColor grey10
        :: borderColor grey6
        :: borderWidth 1
        :: borderRounded 0.25
        :: a


searchInput a =
    Input.search (inputStyle a)


placeholder a =
    Input.placeholder (fontSize 0.875 :: a)



--


inputCheckbox :
    List (Element.Attribute msg)
    ->
        { icon : Bool -> Element msg
        , label : Input.Label msg
        , checked : Bool
        , onChange : Bool -> msg
        }
    -> Element msg
inputCheckbox a =
    Input.checkbox (spacing 0.5 :: a)


inputDefaultCheckbox =
    Input.defaultCheckbox



--


inputRadioRow :
    List (Element.Attribute msg)
    ->
        { label : Input.Label msg
        , options : List (Input.Option a msg)
        , selected : Maybe a
        , onChange : a -> msg
        }
    -> Element msg
inputRadioRow a =
    Input.radioRow (fontSize 0.875 :: a)


inputOption : a -> Element msg -> Input.Option a msg
inputOption a b =
    Input.optionWith a
        (\v ->
            case v of
                Input.Idle ->
                    el [] b

                Input.Focused ->
                    el [] b

                Input.Selected ->
                    el [ fontColor primary ] b
        )



--


labelStyle a =
    fontSize 0.875 :: fontColor grey4 :: a


labelRight a =
    Input.labelRight (labelStyle a)


labelLeft a =
    Input.labelLeft (labelStyle a)


labelAbove a =
    Input.labelAbove (labelStyle a)


labelBelow a =
    Input.labelBelow (labelStyle a)


labelHidden =
    Input.labelHidden



-- Root


rootStyle a =
    fontSize 1
        :: fontFamilyDefault
        :: fontColor grey1
        :: bgColor grey9
        :: a
