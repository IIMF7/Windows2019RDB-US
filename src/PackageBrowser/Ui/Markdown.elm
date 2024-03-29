module PackageBrowser.Ui.Markdown exposing (..)

import Element.Input as Input
import Markdown.Block as Block
import Markdown.Html
import Markdown.Renderer
import PackageBrowser.Ui.Base exposing (..)
import Url.Builder


renderer : Markdown.Renderer.Renderer (Element msg)
renderer =
    { heading =
        \{ level, rawText, children } ->
            p
                [ fontSize
                    (case level of
                        Block.H1 ->
                            32

                        Block.H2 ->
                            24

                        _ ->
                            16
                    )
                , fontBold
                , paddingEach 0 0 16 0
                , regionHeading (Block.headingLevelToInt level)
                , id (idFromString rawText)
                ]
                children
    , paragraph = p []
    , blockQuote = column [ spacing 16, paddingEach 32 0 0 0 ]
    , html = Markdown.Html.oneOf []
    , text = text
    , codeSpan =
        \a ->
            el
                [ padding 2
                , borderRounded 4
                , bgColor grey8
                , monospaceFontFamily
                ]
                (text a)
    , strong = p [ fontBold ]
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
            column [ spacing 16, paddingEach 4 0 0 0 ]
                (a
                    |> List.map
                        (\(Block.ListItem b c) ->
                            row [ spacing 8 ]
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
            column [ spacing 16, paddingEach 4 0 0 0 ]
                (a
                    |> List.indexedMap
                        (\i b ->
                            row [ spacing 8 ]
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
                , borderRounded 4
                , bgColor grey8
                , monospaceFontFamily
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
