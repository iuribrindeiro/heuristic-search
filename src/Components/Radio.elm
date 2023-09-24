module Components.Radio exposing (..)

import Html.Styled as Html
import Html.Styled.Attributes as Attr
import Html.Styled.Events
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw


radioInput : String -> String -> String -> msg -> Bool -> Html.Html msg
radioInput id name labelTxt onClick checked =
    Html.div [ Attr.css [ Tw.flex, Tw.flex_row ] ]
        [ Html.input
            [ Attr.type_ "radio"
            , Attr.id id
            , Attr.name name
            , Attr.checked checked
            , Html.Styled.Events.onClick onClick
            , Attr.css [ Tw.mr_3, Tw.cursor_pointer ]
            ]
            []
        , label id labelTxt
        ]


label : String -> String -> Html.Html msg
label name text =
    Html.label
        [ Attr.for name
        , Attr.css [ Tw.text_color Tw.sky_400, Tw.cursor_pointer ]
        ]
        [ Html.text text
        ]
