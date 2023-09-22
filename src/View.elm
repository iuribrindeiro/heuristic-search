module View exposing (View, map, none, placeholder, toBrowserDocument)

import Browser
import Css.Global
import Html.Styled as Html exposing (Html, toUnstyled)
import Html.Styled.Attributes as Attr
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw


type alias View msg =
    { title : String
    , body : List (Html msg)
    }


placeholder : String -> View msg
placeholder str =
    { title = str
    , body = [ Html.text str ]
    }


none : View msg
none =
    placeholder ""


map : (a -> b) -> View a -> View b
map fn view =
    { title = view.title
    , body = List.map (Html.map fn) view.body
    }


toBrowserDocument : View msg -> Browser.Document msg
toBrowserDocument view =
    { title = view.title
    , body =
        [ Css.Global.global [ Css.Global.selector "body" [ Tw.bg_color Tw.neutral_800 ] ]
        , Html.div
            [ Attr.css
                [ Tw.flex
                , Tw.items_start
                ]
            ]
            [ Html.div
                [ Attr.css [ Tw.mx_auto, Tw.mt_40 ] ]
                view.body
            ]
        ]
            |> List.map toUnstyled
    }
