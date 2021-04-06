module PackageBrowser.Ui.Markdown exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html
import Html.Attributes
import Markdown.Block as Block
import Markdown.Html
import Markdown.Renderer


renderer : Markdown.Renderer.Renderer (Element msg)
renderer =
    { heading =
        \{ level, rawText, children } ->
            paragraph
                [ Font.size
                    (case level of
                        Block.H1 ->
                            32

                        Block.H2 ->
                            24

                        _ ->
                            16
                    )
                , Font.bold
                , spacing 8
                , paddingEach { left = 0, right = 0, top = 16, bottom = 0 }
                , Region.heading (Block.headingLevelToInt level)
                , htmlAttribute (Html.Attributes.attribute "name" (idFromString rawText))
                , htmlAttribute (Html.Attributes.id (idFromString rawText))
                ]
                children
    , paragraph =
        \a ->
            paragraph [ spacing 8 ] a
    , blockQuote =
        \a ->
            column
                [ spacing 16
                , paddingEach { left = 32, right = 0, top = 0, bottom = 0 }
                ]
                a
    , html = Markdown.Html.oneOf []
    , text =
        \a ->
            text a
    , codeSpan =
        \a ->
            el
                [ Background.color (rgb255 233 236 239)
                , Border.rounded 4
                , padding 2
                , Font.family
                    [ Font.typeface "SFMono-Regular"
                    , Font.typeface "Menlo"
                    , Font.typeface "Monaco"
                    , Font.typeface "Consolas"
                    , Font.typeface "Liberation Mono"
                    , Font.typeface "Courier New"
                    , Font.monospace
                    ]
                ]
                (text a)
    , strong =
        \a ->
            row [ Font.bold ] a
    , emphasis =
        \a ->
            row [ Font.italic ] a
    , strikethrough =
        \a ->
            row [ Font.strike ] a
    , hardLineBreak =
        html (Html.br [] [])
    , link =
        \a b ->
            newTabLink []
                { label = paragraph [ Font.color (rgb255 13 110 253) ] b
                , url = a.destination
                }
    , image =
        \a ->
            image [ width (shrink |> maximum 512) ]
                { description = a.alt
                , src = a.src
                }
    , unorderedList =
        \a ->
            column [ spacing 16, paddingEach { left = 16, right = 0, top = 0, bottom = 0 } ]
                (a
                    |> List.map
                        (\(Block.ListItem b c) ->
                            row [ spacing 8 ]
                                [ paragraph [ alignTop ]
                                    ((case b of
                                        Block.IncompleteTask ->
                                            Input.defaultCheckbox False

                                        Block.CompletedTask ->
                                            Input.defaultCheckbox True

                                        Block.NoTask ->
                                            text "•"
                                     )
                                        :: text " "
                                        :: c
                                    )
                                ]
                        )
                )
    , orderedList =
        \startIndex a ->
            column [ spacing 16 ]
                (a
                    |> List.indexedMap
                        (\index b ->
                            row [ spacing 8 ]
                                [ row [ alignTop ]
                                    (text (String.fromInt (index + startIndex) ++ " ") :: b)
                                ]
                        )
                )
    , codeBlock =
        \{ body } ->
            el
                [ Background.color (rgb255 233 236 239)
                , padding 8
                , width fill
                , Border.rounded 4
                , Font.family
                    [ Font.typeface "SFMono-Regular"
                    , Font.typeface "Menlo"
                    , Font.typeface "Monaco"
                    , Font.typeface "Consolas"
                    , Font.typeface "Liberation Mono"
                    , Font.typeface "Courier New"
                    , Font.monospace
                    ]
                ]
                (text body)
    , thematicBreak =
        none
    , table = column []
    , tableHeader = column []
    , tableBody = column []
    , tableRow = row []
    , tableHeaderCell =
        \_ a ->
            paragraph [] a
    , tableCell =
        \_ a ->
            paragraph [] a
    }


idFromString : String -> String
idFromString a =
    a |> String.replace " " "-"
