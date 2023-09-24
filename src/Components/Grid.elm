module Components.Grid exposing (..)

import Css
import Html.Styled as Html
import Html.Styled.Attributes as Attr
import Html.Styled.Events
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw
import Utils exposing (Grid, Line, Slot)


type GridState
    = Success
    | Loading
    | LoadingX
    | Disabled
    | Start


liBefore : List Css.Style
liBefore =
    [ Tw.right_1over2 ]


liAfter : List Css.Style
liAfter =
    [ Tw.left_1over2
    , Tw.border_l_2
    , Css.property "animation" "fadeIn 0.3s ease"
    , Tw.origin_left
    ]


liOnlyChild : List Css.Style
liOnlyChild =
    [ Tw.pt_0 ]


liOnlyChildBeforeAfter : List Css.Style
liOnlyChildBeforeAfter =
    [ Tw.hidden ]


liFirstChildBeforeLastChildAfter : List Css.Style
liFirstChildBeforeLastChildAfter =
    [ Tw.border_0, Tw.border_none ]


liLastChildBefore : List Css.Style
liLastChildBefore =
    [ Tw.rounded_tr_lg
    , Tw.border_r_2
    , Css.property "animation" "fadeIn 0.3s ease"
    , Tw.origin_right
    ]


liFirstChildAfter : List Css.Style
liFirstChildAfter =
    [ Tw.rounded_tl_lg ]


liBeforeAfterStyles : List Css.Style
liBeforeAfterStyles =
    [ Css.property "content" "''"
    , Tw.absolute
    , Tw.top_0
    , Tw.w_1over2
    , Tw.h_9
    , Tw.border_t_2
    , Tw.border_color Tw.sky_400
    , Css.property "animation" "fadeIn 0.3s ease"
    , Tw.origin_top
    ]


ulOlBeforeStyles : List Css.Style
ulOlBeforeStyles =
    [ Css.property "content" "''"
    , Tw.absolute
    , Tw.top_0
    , Tw.left_1over2
    , Tw.border_l_2
    , Tw.h_5
    , Tw.border_color Tw.sky_400
    , Css.property "animation" "fadeIn 0.3s ease"
    , Tw.origin_top
    ]


ulOlFirstChild : List Css.Style
ulOlFirstChild =
    [ Tw.pt_0
    ]


ulOlBeforeFirstChild : List Css.Style
ulOlBeforeFirstChild =
    [ Tw.hidden ]


ulOlStyles : List Css.Style
ulOlStyles =
    [ Tw.flex
    , Tw.items_start
    , Tw.flex_nowrap
    , Tw.list_none
    , Tw.relative
    , Tw.pt_5
    , Tw.pl_0
    ]


styledLstOfChildren : List (Html.Html msg) -> Html.Html msg
styledLstOfChildren =
    Html.ul
        [ ulOlStyles
            |> List.append [ Css.before ulOlBeforeStyles ]
            |> List.append [ Css.firstChild ulOlFirstChild ]
            |> List.append [ Css.property "animation" "fadeIn 0.3s ease-in-out" ]
            |> Attr.css
        ]


singleItemStyledNodeWrapper : List (Html.Html msg) -> Html.Html msg
singleItemStyledNodeWrapper =
    Html.ol
        [ ulOlStyles
            |> List.append [ Css.before ulOlBeforeStyles ]
            |> List.append [ Css.firstChild ulOlFirstChild ]
            |> List.append [ Css.before [ Css.firstChild ulOlBeforeFirstChild ] ]
            |> List.append [ Css.property "animation" "fadeIn 0.3s ease-in-out" ]
            |> Attr.css
        ]


styledNode : List (Html.Html msg) -> Html.Html msg
styledNode lst =
    Html.li
        [ [ Css.after liAfter ]
            |> List.append [ Css.before liBefore ]
            |> List.append [ Css.onlyChild liOnlyChild ]
            |> List.append
                [ Css.onlyChild
                    [ Css.before liOnlyChildBeforeAfter
                    , Css.after liOnlyChildBeforeAfter
                    ]
                ]
            |> List.append
                [ Css.firstChild [ Css.before liFirstChildBeforeLastChildAfter ]
                , Css.lastChild [ Css.after liFirstChildBeforeLastChildAfter ]
                ]
            |> List.append [ Css.before liBeforeAfterStyles, Css.after liBeforeAfterStyles ]
            |> List.append [ Css.lastChild [ Css.before liLastChildBefore ] ]
            |> List.append [ Css.firstChild [ Css.after liFirstChildAfter ] ]
            |> List.append
                [ Tw.flex
                , Tw.items_center
                , Tw.flex_col
                , Tw.basis_full
                , Tw.relative
                , Tw.pt_10
                , Tw.pl_5
                , Tw.pr_5
                ]
            |> Attr.css
        ]
        lst


renderHeuristicBadge : Bool -> Maybe Int -> Html.Html msg
renderHeuristicBadge renderHeuristic maybeVal =
    let
        value =
            maybeVal |> Maybe.withDefault 0

        textVal =
            if value > 0 then
                Html.text <| String.fromInt value

            else
                Html.text ""
    in
    if renderHeuristic && value > 0 then
        Html.div
            [ Attr.css
                [ Tw.rounded_full
                , Tw.bg_color Tw.amber_600
                , Tw.w_5
                , Tw.h_5
                , Tw.text_center
                , Tw.text_color Tw.neutral_950
                , Tw.text_sm
                , Tw.absolute
                , Tw.bottom_32
                , Tw.mb_1
                , Tw.left_28
                ]
            ]
            [ textVal ]

    else
        Html.text ""


renderStyledGrid : Bool -> Maybe Int -> Css.Style -> Tw.Color -> msg -> msg -> List (Html.Html msg) -> Html.Html msg
renderStyledGrid renderHeuristic heuristic animation color onMouseOver onMouseOut lstLines =
    let
        heuristicBadge =
            renderHeuristicBadge renderHeuristic heuristic
    in
    Html.div
        [ Attr.css
            [ Tw.text_color color
            , animation
            , Tw.cursor_pointer
            , Tw.relative
            , Css.hover [ Tw.shadow, Tw.shadow_color color ]
            ]
        , Html.Styled.Events.onMouseOver onMouseOver
        , Html.Styled.Events.onMouseOut onMouseOut
        ]
        [ heuristicBadge
        , Html.div [ Attr.css [ Tw.flex, Tw.flex_col ] ]
            lstLines
        ]


renderSmallGrid : GridState -> Bool -> (Grid -> msg) -> msg -> Grid -> Html.Html msg
renderSmallGrid gridState selected onMouseOver onMouseOut grid =
    renderGrid selected True gridState (onMouseOver grid) onMouseOut grid


renderMdGrid : GridState -> Bool -> (Grid -> msg) -> msg -> Grid -> Html.Html msg
renderMdGrid gridState selected onMouseOver onMouseOut grid =
    renderGrid selected False gridState (onMouseOver grid) onMouseOut grid


renderGrid : Bool -> Bool -> GridState -> msg -> msg -> Grid -> Html.Html msg
renderGrid selected small gridState onMouseOver onMouseOut grid =
    let
        gridAnimation =
            case gridState of
                Success ->
                    if selected then
                        Tw.animate_infiniteSmPing

                    else
                        Tw.animate_longCountSmPing

                LoadingX ->
                    Tw.animate_finiteSmPing

                _ ->
                    if selected then
                        Tw.animate_infiniteSmPing

                    else
                        Tw.animate_none

        gridColor =
            case gridState of
                Success ->
                    Tw.green_400

                Loading ->
                    Tw.sky_400

                LoadingX ->
                    Tw.indigo_400

                Disabled ->
                    Tw.gray_400

                Start ->
                    Tw.sky_200
    in
    grid.lines
        |> List.map (renderLine small gridColor)
        |> renderStyledGrid (not small) grid.heuristic gridAnimation gridColor onMouseOver onMouseOut


renderLine : Bool -> Tw.Color -> Line -> Html.Html msg
renderLine small slotColor line =
    line
        |> List.map (renderSlot small slotColor)
        |> Html.div [ Attr.css [ Tw.flex, Tw.flex_row ] ]


renderSlot : Bool -> Tw.Color -> Slot -> Html.Html msg
renderSlot small slotColor slot =
    let
        py =
            if small then
                Tw.py_0

            else
                Tw.py_2

        px =
            if small then
                Css.property "padding" "1px 5px 1px 5px"

            else
                Tw.px_4

        border =
            if small then
                Css.property "border-width" "0.5px"

            else
                Tw.border
    in
    Html.div
        [ Attr.css
            [ py
            , px
            , Tw.border_solid
            , border
            , Tw.border_color slotColor
            , Tw.transition_colors
            ]
        ]
        (slot.value
            |> Maybe.map (\z -> [ Html.span [] [ Html.text <| String.fromInt z ] ])
            |> Maybe.withDefault [ Html.span [ Attr.css [ Tw.invisible ] ] [ Html.text <| String.fromInt 0 ] ]
        )
