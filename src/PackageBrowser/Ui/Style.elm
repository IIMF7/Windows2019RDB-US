module PackageBrowser.Ui.Style exposing (..)

import Element
import Element.Font as Font


rootEm =
    16


baseBgColor =
    grey9


baseColor =
    grey1


baseFontSize =
    rootEm


baseLineSpacing =
    round (rootEm * 0.5)


linkColor =
    primary


hrBorderColor =
    grey7



--


h1FontSize =
    round (rootEm * 1.5)


h2FontSize =
    round (rootEm * 2)


h3FontSize =
    round (rootEm * 1.75)


h4FontSize =
    round (rootEm * 1.5)


h5FontSize =
    round (rootEm * 1.25)


h6FontSize =
    round (rootEm * 1)



--


baseFontFamily =
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


monospaceFontFamily =
    Font.family
        [ Font.typeface "SFMono-Regular"
        , Font.typeface "Menlo"
        , Font.typeface "Monaco"
        , Font.typeface "Consolas"
        , Font.typeface "Liberation Mono"
        , Font.typeface "Courier New"
        , Font.monospace
        ]



--


shadow1 a =
    { offset = ( 0, rootEm * 16 )
    , size = 0
    , blur = rootEm * a
    , color = grey0 |> Element.toRgb |> (\v -> { v | alpha = 0.2 }) |> Element.fromRgb
    }


shadow2 =
    shadow1


shadow3 =
    shadow1



--


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



--


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
