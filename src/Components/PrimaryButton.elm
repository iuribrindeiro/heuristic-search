module Components.PrimaryButton exposing (..)

import Css
import Html.Styled as Html
import Html.Styled.Attributes as Attr
import Html.Styled.Events
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw


primaryButton : Bool -> msg -> Html.Html msg
primaryButton disabled onClick =
    let
        hoverStyles =
            if not disabled then
                [ Tw.scale_110
                , Tw.duration_300
                , Tw.neg_translate_y_1
                , Tw.bg_color Tw.indigo_400
                , Tw.shadow_lg
                , Tw.shadow_color Tw.indigo_500
                ]

            else
                []

        color =
            if disabled then
                Tw.gray_400

            else
                Tw.sky_400

        cursor =
            if disabled then
                Tw.cursor_not_allowed

            else
                Tw.cursor_pointer
    in
    Html.button
        [ Attr.css
            [ Tw.rounded_xl
            , Tw.bg_color color
            , Tw.px_8
            , Tw.py_2
            , Tw.border_none
            , cursor
            , Tw.transition
            , Tw.ease_in_out
            , Tw.transform
            , Tw.mb_5
            , Tw.text_sm
            , Css.hover hoverStyles
            , Tw.w_fit
            ]
        , Html.Styled.Events.onClick onClick
        , Attr.disabled disabled
        ]
        [ Html.text "Start" ]
