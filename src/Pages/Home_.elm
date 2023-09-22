module Pages.Home_ exposing (Model, Msg, page)

import Css
import Css.Global
import Dict
import Html.Attributes exposing (style)
import Html.Styled as Html
import Html.Styled.Attributes as Attr
import Html.Styled.Events
import List exposing (singleton)
import Page
import Process
import Request exposing (Request)
import Shared
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw
import Task
import Utils exposing (GenerateChildrenFailures, Grid, Line, NestedGrid(..), Slot, generateChildrenOf, notIn, toGrid)
import View exposing (View)


goal : Grid
goal =
    [ [ 1, 2, 3 ]
    , [ 4, 5, 6 ]
    , [ 7, 8, 0 ]
    ]
        |> toGrid


start : Grid
start =
    [ [ 1, 2, 3 ]
    , [ 4, 5, 6 ]
    , [ 7, 0, 8 ]
    ]
        |> toGrid


type GridState
    = Success
    | Loading
    | LoadingX
    | Disabled
    | Start


type alias SearchingModel =
    { root : NestedGrid, x : Grid, open : List Grid, closed : List Grid }


type FailureMsgType
    = NoMoreElementsInList
    | FailureGeneratingChildren GenerateChildrenFailures


type alias FailureModel =
    { failure : FailureMsgType, searchModel : SearchingModel }


type Model
    = Idle
    | Searching SearchingModel
    | GoalFound SearchingModel
    | Failure FailureModel


type Msg
    = StartSearchingMsg
    | XFoundMsg { x : Grid, open : List Grid }
    | KeepSearchingMsg { children : List Grid, open : List Grid, closed : List Grid }
    | GoalFoundMsg { goalValue : Grid, closed : List Grid }
    | FailureMsg FailureMsgType


page : Shared.Model -> Request -> Page.With Model Msg
page _ _ =
    Page.element { init = init, update = update, view = view, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions =
    \_ -> Sub.none


init : ( Model, Cmd Msg )
init =
    ( Idle, Cmd.none )


firstNestedGrid : NestedGrid
firstNestedGrid =
    NestedGridModel { current = start, parent = Nothing, children = [] }


getCurrentSearchingModel : Model -> SearchingModel
getCurrentSearchingModel model =
    case model of
        Idle ->
            { x = start, root = firstNestedGrid, open = [ start ], closed = [] }

        Searching searchingMmodel ->
            searchingMmodel

        GoalFound searchingMmodel ->
            searchingMmodel

        Failure { searchModel } ->
            searchModel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        currentSearchingModel =
            getCurrentSearchingModel model

        root =
            currentSearchingModel.root
    in
    case msg of
        StartSearchingMsg ->
            ( currentSearchingModel |> Searching, findX currentSearchingModel.open )

        XFoundMsg { x, open } ->
            ( { currentSearchingModel | x = x } |> Searching
            , findGoal x open currentSearchingModel.closed
            )

        KeepSearchingMsg { children, open, closed } ->
            ( { currentSearchingModel
                | x = currentSearchingModel.x
                , open = open
                , closed = closed
                , root = addChildrenToTarget currentSearchingModel.x children root
              }
                |> Searching
            , findX open
            )

        GoalFoundMsg { goalValue, closed } ->
            ( { currentSearchingModel | x = goalValue, closed = closed } |> GoalFound, Cmd.none )

        FailureMsg failure ->
            ( { failure = failure, searchModel = currentSearchingModel } |> Failure, Cmd.none )


addChildrenToTarget : Grid -> List Grid -> NestedGrid -> NestedGrid
addChildrenToTarget x children (NestedGridModel root) =
    if x == root.current then
        NestedGridModel root |> addChildren children

    else if root.children == [] then
        NestedGridModel root

    else
        NestedGridModel { root | children = root.children |> List.map (addChildrenToTarget x children) }


addChildren : List Grid -> NestedGrid -> NestedGrid
addChildren children (NestedGridModel current) =
    NestedGridModel
        { current
            | children =
                children
                    |> List.map
                        (\x ->
                            NestedGridModel
                                { current = x
                                , children = []
                                , parent = Just (NestedGridModel current)
                                }
                        )
        }


view : Model -> View Msg
view model =
    { title = "Homepage"
    , body =
        [ Css.Global.global Tw.globalStyles
        , Html.div
            [ Attr.css
                [ Tw.flex
                , Tw.flex_col
                , Tw.items_center
                , Tw.relative
                ]
            ]
            ([ Html.div [ Attr.css [ Tw.flex, Tw.flex_row ] ]
                [ renderNestedGrid model
                , goal
                    |> List.map (renderLine Tw.green_400)
                    |> renderStyledGrid Tw.green_400
                ]
             ]
                |> List.append [ startButtom ]
            )
        ]
    }


startButtom : Html.Html Msg
startButtom =
    Html.button
        [ Attr.css
            [ Tw.rounded_lg
            , Tw.bg_color Tw.sky_400
            , Tw.px_8
            , Tw.py_2
            , Tw.border_none
            , Tw.cursor_pointer
            , Tw.shadow_lg
            , Tw.transition
            , Tw.ease_in_out
            , Tw.delay_150
            , Tw.transform
            , Tw.mb_5
            , Tw.text_sm
            , Css.hover
                [ Tw.scale_110
                , Tw.duration_300
                , Tw.neg_translate_y_1
                , Tw.bg_color Tw.indigo_400
                ]
            ]
        , Html.Styled.Events.onClick StartSearchingMsg
        ]
        [ Html.text "Start" ]


renderNestedGrid : Model -> Html.Html msg
renderNestedGrid model =
    let
        searchingModel =
            getCurrentSearchingModel model

        (NestedGridModel nestedGridModel) =
            searchingModel.root
    in
    singleItemStyledNodeWrapper
        [ styledNode
            [ renderGrid searchingModel model nestedGridModel.current
            , renderChildren searchingModel model <| nestedGridModel.children
            ]
        ]


liBefore : List Css.Style
liBefore =
    [ Tw.right_1over2 ]


liAfter : List Css.Style
liAfter =
    [ Tw.left_1over2, Tw.border_l_2 ]


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
    [ Tw.rounded_tr_lg, Tw.border_r_2 ]


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
    ]


ulOlBeforeStyles : List Css.Style
ulOlBeforeStyles =
    [ Css.property "content" "''"
    , Tw.absolute
    , Tw.top_0
    , Tw.left_1over2
    , Tw.border_l_2
    , Tw.h_5
    , Tw.transition
    , Tw.ease_in_out
    , Tw.border_color Tw.sky_400
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


renderChildren : SearchingModel -> Model -> List NestedGrid -> Html.Html msg
renderChildren searchingMmodel model children =
    let
        lstEl =
            if List.length children == 1 then
                singleItemStyledNodeWrapper

            else
                Html.ul
                    [ ulOlStyles
                        |> List.append [ Css.before ulOlBeforeStyles ]
                        |> List.append [ Css.firstChild ulOlFirstChild ]
                        |> Attr.css
                    ]
    in
    if children == [] then
        Html.div [] []

    else
        children
            |> List.map (nodeWithChildren searchingMmodel model)
            |> lstEl


singleItemStyledNodeWrapper : List (Html.Html msg) -> Html.Html msg
singleItemStyledNodeWrapper =
    Html.ol
        [ ulOlStyles
            |> List.append [ Css.before ulOlBeforeStyles ]
            |> List.append [ Css.firstChild ulOlFirstChild ]
            |> List.append [ Css.before [ Css.firstChild ulOlBeforeFirstChild ] ]
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


nodeWithChildren : SearchingModel -> Model -> NestedGrid -> Html.Html msg
nodeWithChildren searchingModel model (NestedGridModel nestedGrid) =
    styledNode
        [ renderGrid searchingModel model nestedGrid.current
        , renderChildren searchingModel model nestedGrid.children
        ]


renderStyledGrid : Tw.Color -> List (Html.Html msg) -> Html.Html msg
renderStyledGrid color lstLines =
    Html.div [ Attr.css [ Tw.transition, Tw.ease_in_out, Tw.text_color color ] ]
        [ Html.div [ Attr.css [ Tw.flex, Tw.flex_col ] ]
            lstLines
        ]


renderGrid : SearchingModel -> Model -> Grid -> Html.Html msg
renderGrid searchingModel model grid =
    let
        gridColor =
            case getGridState searchingModel model grid of
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
    grid
        |> List.map (renderLine gridColor)
        |> renderStyledGrid gridColor


renderLine : Tw.Color -> Line -> Html.Html msg
renderLine slotColor line =
    line
        |> List.map (renderSlot slotColor)
        |> Html.div [ Attr.css [ Tw.flex, Tw.flex_row ] ]


renderSlot : Tw.Color -> Slot -> Html.Html msg
renderSlot slotColor slot =
    Html.div
        [ Attr.css
            [ Tw.py_2
            , Tw.px_4
            , Tw.border_solid
            , Tw.border
            , Tw.border_color slotColor
            ]
        ]
        (slot.value
            |> Maybe.map (\z -> [ Html.span [] [ Html.text <| String.fromInt z ] ])
            |> Maybe.withDefault [ Html.span [ Attr.css [ Tw.invisible ] ] [ Html.text <| String.fromInt 0 ] ]
        )


getGridState : SearchingModel -> Model -> Grid -> GridState
getGridState searchingModel model grid =
    let
        goalFound =
            case model of
                GoalFound _ ->
                    True

                _ ->
                    False

        isIdle =
            case model of
                Idle ->
                    True

                _ ->
                    False
    in
    if grid == goal && goalFound then
        Success

    else if (grid |> List.member) <| searchingModel.closed then
        Disabled

    else if grid == searchingModel.x && not isIdle then
        LoadingX

    else if ((grid |> List.member) <| searchingModel.open) && not goalFound then
        Loading

    else
        Start


findGoal : Grid -> List Grid -> List Grid -> Cmd Msg
findGoal x open closed =
    if x == goal then
        GoalFoundMsg { goalValue = x, closed = open ++ closed } |> delayMessage

    else
        x |> generateChildrenAndSearch closed open


generateChildrenAndSearch : List Grid -> List Grid -> Grid -> Cmd Msg
generateChildrenAndSearch closed open x =
    case generateChildrenOf x of
        Ok childrenOfX ->
            let
                nonClosedChildrenOfX =
                    childrenOfX
                        |> notIn closed

                newOpen =
                    List.append
                        open
                        nonClosedChildrenOfX

                newClosed =
                    x :: closed
            in
            KeepSearchingMsg { children = childrenOfX, open = newOpen, closed = newClosed }
                |> delayMessage

        Err failures ->
            FailureGeneratingChildren failures
                |> FailureMsg
                |> delayMessage


findX : List Grid -> Cmd Msg
findX open =
    case open of
        [] ->
            FailureMsg NoMoreElementsInList |> delayMessage

        x :: xs ->
            { x = x, open = xs } |> XFoundMsg |> delayMessage


delayMessage : Msg -> Cmd Msg
delayMessage msg =
    Process.sleep 200
        |> Task.perform (\_ -> msg)
