module Element.Virtualized exposing (..)

import Element
import Element.Keyed
import Html.Attributes
import Html.Events
import Json.Decode as Decode


column :
    List (Element.Attribute msg)
    ->
        { data : List a
        , getKey : a -> String
        , getSize : a -> Int
        , scrollOffset : Float
        , view : a -> Element.Element msg
        , onScroll : Float -> msg
        }
    -> Element.Element msg
column attrs { data, getKey, getSize, scrollOffset, view, onScroll } =
    let
        { size, items } =
            compute getSize scrollOffset data

        spacer : ( String, Element.Element msg )
        spacer =
            ( ""
            , Element.el [ Element.height (Element.px size) ] Element.none
            )
    in
    Element.Keyed.column
        (Element.width Element.fill
            :: Element.height Element.fill
            :: Element.scrollbars
            :: onScroll_ onScroll
            :: attrs
        )
        (spacer
            :: (items
                    |> List.map
                        (\v ->
                            ( getKey v.value
                            , Element.el
                                [ Element.width Element.fill
                                , Element.height (Element.px v.size)
                                , Element.htmlAttribute (Html.Attributes.style "position" "absolute")
                                , Element.htmlAttribute (Html.Attributes.style "top" (String.fromInt v.offset ++ "px"))
                                ]
                                (view v.value)
                            )
                        )
               )
        )



--


type alias VirtualList a =
    { size : Int
    , items :
        List
            { offset : Int
            , size : Int
            , value : a
            }
    }


onScroll_ : (Float -> msg) -> Element.Attribute msg
onScroll_ toMsg =
    let
        decoder : Decode.Decoder msg
        decoder =
            Decode.at [ "target", "scrollTop" ] Decode.float
                |> Decode.map toMsg
    in
    Element.htmlAttribute (Html.Events.on "scroll" decoder)
