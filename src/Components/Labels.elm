module Components.Labels exposing (..)

import Html.Styled as Html
import Html.Styled.Attributes as Attr
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw


labels : Html.Html msg
labels =
    Html.div [ Attr.css [ Tw.flex, Tw.flex_row, Tw.py_4, Tw.items_center, Tw.text_color Tw.white ] ]
        [ Html.div
            [ Attr.css
                [ Tw.rounded_full
                , Tw.bg_color Tw.sky_400
                , Tw.w_4
                , Tw.h_4
                ]
            ]
            []
        , Html.span [ Attr.css [ Tw.ml_3 ] ] [ Html.text "Open" ]
        , Html.div
            [ Attr.css
                [ Tw.rounded_full
                , Tw.bg_color Tw.gray_400
                , Tw.w_4
                , Tw.h_4
                , Tw.ml_4
                ]
            ]
            []
        , Html.span [ Attr.css [ Tw.ml_2 ] ] [ Html.text "Closed" ]
        , Html.div
            [ Attr.css
                [ Tw.rounded_full
                , Tw.bg_color Tw.green_400
                , Tw.w_4
                , Tw.h_4
                , Tw.ml_4
                ]
            ]
            []
        , Html.span [ Attr.css [ Tw.ml_2 ] ] [ Html.text "Goal" ]
        , Html.div
            [ Attr.css
                [ Tw.rounded_full
                , Tw.bg_color Tw.indigo_400
                , Tw.w_4
                , Tw.h_4
                , Tw.ml_4
                ]
            ]
            []
        , Html.span [ Attr.css [ Tw.ml_2 ] ] [ Html.text "X" ]
        ]
