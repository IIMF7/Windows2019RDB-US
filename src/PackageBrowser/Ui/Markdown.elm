module PackageBrowser.Ui.Markdown exposing (..)

import Element.Input as Input
import Element.Region as Region
import Markdown.Block as Block
import Markdown.Html
import Markdown.Renderer
import PackageBrowser.Ui exposing (..)
import Url.Builder


renderer : Markdown.Renderer.Renderer (Element msg)
renderer =
    { heading =
        \{ level, rawText, children } ->
            p
                [ fontSize
                    (case level of
                        Block.H1 ->
                            2

                        Block.H2 ->
                            1.5

                        _ ->
                            1
                    )
                , fontWeight 7
                , paddingEach 0 0 16 0
                , Region.heading (Block.headingLevelToInt level)
                , id (idFromString rawText)
                ]
                children
    , paragraph = p []
    , blockQuote = column [ spacing 1, paddingEach 32 0 0 0 ]
    , html = Markdown.Html.oneOf []
    , text = text
    , codeSpan =
        \a ->
            el
                [ padding 2
                , borderRounded 0.25
                , bgColor grey8
                , fontFamilyMonospace
                ]
                (text a)
    , strong = p [ fontWeight 7 ]
    , emphasis = p [ fontItalic ]
    , strikethrough = p [ fontStrike ]
    , hardLineBreak = br
    , link =
        \a b ->
            newTabLink []
                { label = p [] b
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
            column [ spacing 1, paddingEach 4 0 0 0 ]
                (a
                    |> List.map
                        (\(Block.ListItem b c) ->
                            row [ spacing 0.5 ]
                                [ el [ alignTop ]
                                    (case b of
                                        Block.IncompleteTask ->
                                            Input.defaultCheckbox False

                                        Block.CompletedTask ->
                                            Input.defaultCheckbox True

                                        Block.NoTask ->
                                            text "•"
                                    )
                                , p [] c
                                ]
                        )
                )
    , orderedList =
        \startIndex a ->
            column [ spacing 1, paddingEach 4 0 0 0 ]
                (a
                    |> List.indexedMap
                        (\i b ->
                            row [ spacing 0.5 ]
                                [ el [ alignTop ]
                                    (text (String.fromInt (i + startIndex) ++ "."))
                                , p [] b
                                ]
                        )
                )
    , codeBlock =
        \{ body } ->
            el
                [ width fill
                , padding 8
                , borderRounded 0.25
                , bgColor grey8
                , fontFamilyMonospace
                ]
                (text body)
    , thematicBreak = hr
    , table = column []
    , tableHeader = column []
    , tableBody = column []
    , tableRow = row []
    , tableHeaderCell = \_ a -> p [] a
    , tableCell = \_ a -> p [] a
    }


idFromString : String -> String
idFromString a =
    a |> String.replace " " "-"


idToUrl : String -> String
idToUrl a =
    Url.Builder.custom
        Url.Builder.Relative
        []
        []
        (Just (idFromString a))
