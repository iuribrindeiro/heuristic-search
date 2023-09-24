module Pages.Home_ exposing (Model, Msg, page)

import Components.Grid exposing (GridState(..), renderMdGrid, renderSmallGrid, singleItemStyledNodeWrapper, styledLstOfChildren, styledNode)
import Components.Labels exposing (labels)
import Components.PrimaryButton exposing (primaryButton)
import Components.Radio exposing (radioInput)
import Core exposing (FindGoalResult(..), FindXResult(..), SearchMode(..), findGoal, findX)
import Css
import Html.Styled as Html
import Html.Styled.Attributes as Attr
import List
import Page
import Process
import Request exposing (Request)
import Shared
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw
import Task
import Utils exposing (GenerateChildrenFailures, Grid, NestedGrid(..), toGrid)
import View exposing (View)


goal : Grid
goal =
    [ [ 1, 2, 3 ]
    , [ 5, 8, 6 ]
    , [ 4, 7, 0 ]
    ]
        |> toGrid


start : Grid
start =
    [ [ 1, 2, 3 ]
    , [ 4, 5, 6 ]
    , [ 0, 8, 7 ]
    ]
        |> toGrid


type alias SearchingModel =
    { root : NestedGrid, x : Grid, open : List Grid, closed : List Grid }


type FailureMsgType
    = NoMoreElementsInList
    | FailureGeneratingChildren GenerateChildrenFailures


type alias FailureModel =
    { failure : FailureMsgType, searchingModel : SearchingModel }


type alias Model =
    { searchMode : SearchMode
    , selectedGrid : Maybe Grid
    , state : State
    }


type State
    = Idle
    | Searching SearchingModel
    | GoalFound SearchingModel
    | Failure FailureModel


type Msg
    = StartSearchingMsg
    | XFoundMsg { x : Grid, open : List Grid }
    | KeepSearchingMsg { children : List Grid, open : List Grid, closed : List Grid }
    | GoalFoundMsg
    | FailureMsg FailureMsgType
    | UpdateSearchMode SearchMode
    | SelectGrid Grid
    | DeselectGrid


page : Shared.Model -> Request -> Page.With Model Msg
page _ _ =
    Page.element { init = init, update = update, view = view, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions =
    \_ -> Sub.none


init : ( Model, Cmd Msg )
init =
    ( { searchMode = BreadthFirst, selectedGrid = Nothing, state = Idle }, Cmd.none )


firstNestedGrid : NestedGrid
firstNestedGrid =
    NestedGridModel { current = start, parent = Nothing, children = [], level = 0 }


getCurrentSearchingModel : Model -> SearchingModel
getCurrentSearchingModel model =
    case model.state of
        Idle ->
            { x = start, root = firstNestedGrid, open = [ start ], closed = [] }

        Searching searchingModel ->
            searchingModel

        GoalFound searchingModel ->
            searchingModel

        Failure { searchingModel } ->
            searchingModel


findXWithMsg : List Grid -> Cmd Msg
findXWithMsg open =
    case findX open of
        LstIsEmpty ->
            FailureMsg NoMoreElementsInList
                |> delayMessage200ms

        XFound rs ->
            XFoundMsg rs
                |> delayMessage200ms


findGoalWithMsg : Grid -> List Grid -> List Grid -> SearchMode -> NestedGrid -> Cmd Msg
findGoalWithMsg x open closed searchMode root =
    case findGoal x goal open closed searchMode root of
        FailRs generatingChildFail ->
            FailureGeneratingChildren generatingChildFail
                |> FailureMsg
                |> delayMessage200ms

        GoalFoundRs ->
            GoalFoundMsg
                |> delayMessage200ms

        KeepSearchingRs rs ->
            KeepSearchingMsg rs
                |> delayMessage2s


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        currentSearchingModel =
            getCurrentSearchingModel model

        handleStartSearchingMsg =
            ( { model | state = Searching currentSearchingModel }, findXWithMsg currentSearchingModel.open )

        handleUpdateSearchMsg newSearchMode =
            ( { model | searchMode = newSearchMode, state = Idle }, Cmd.none )

        handleSelectGrid grid =
            ( { model | selectedGrid = Just grid }, Cmd.none )

        handleDeselectGrid =
            ( { model | selectedGrid = Nothing }, Cmd.none )
    in
    case model.state of
        Idle ->
            case msg of
                StartSearchingMsg ->
                    handleStartSearchingMsg

                UpdateSearchMode searchMode ->
                    handleUpdateSearchMsg searchMode

                SelectGrid grid ->
                    handleSelectGrid grid

                DeselectGrid ->
                    handleDeselectGrid

                _ ->
                    ( model, Cmd.none )

        _ ->
            case msg of
                StartSearchingMsg ->
                    handleStartSearchingMsg

                XFoundMsg { x, open } ->
                    ( { model | state = Searching { currentSearchingModel | x = x, open = open } }
                    , findGoalWithMsg x open currentSearchingModel.closed model.searchMode currentSearchingModel.root
                    )

                KeepSearchingMsg { children, open, closed } ->
                    ( { model
                        | state =
                            Searching
                                { currentSearchingModel
                                    | open = open
                                    , closed = closed
                                    , root = addChildrenToTarget currentSearchingModel.x children currentSearchingModel.root
                                }
                      }
                    , findXWithMsg open
                    )

                GoalFoundMsg ->
                    ( { model | state = GoalFound { currentSearchingModel | open = List.filter (\z -> z /= currentSearchingModel.x) currentSearchingModel.open } }, Cmd.none )

                FailureMsg failure ->
                    ( { model | state = Failure { failure = failure, searchingModel = currentSearchingModel } }, Cmd.none )

                UpdateSearchMode newSearchMode ->
                    handleUpdateSearchMsg newSearchMode

                SelectGrid grid ->
                    handleSelectGrid grid

                DeselectGrid ->
                    handleDeselectGrid


addChildrenToTarget : Grid -> List Grid -> NestedGrid -> NestedGrid
addChildrenToTarget x children (NestedGridModel root) =
    if x.lines == root.current.lines then
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
                                , level = current.level + 1
                                }
                        )
        }


setHeuristic : Msg
setHeuristic =
    UpdateSearchMode Heuristic


setDepthFirst : Msg
setDepthFirst =
    UpdateSearchMode DepthFirst


setBreadthFirst : Msg
setBreadthFirst =
    UpdateSearchMode BreadthFirst


isGridSelected : Model -> Grid -> Bool
isGridSelected model grid =
    Maybe.map (\z -> z.lines == grid.lines) model.selectedGrid
        |> Maybe.withDefault False


view : Model -> View Msg
view model =
    let
        searchingModel =
            getCurrentSearchingModel model
    in
    { title = "Homepage"
    , body =
        [ Html.div
            [ Attr.css
                [ Tw.flex
                , Tw.flex_col
                , Tw.justify_between
                , Css.height (Css.rem 70)
                ]
            ]
            [ Html.div
                [ Attr.css
                    [ Tw.flex
                    , Tw.flex_col
                    , Tw.pl_2
                    , Tw.pb_4
                    ]
                ]
                [ model.searchMode
                    == BreadthFirst
                    |> radioInput "breadth-first" "searchType" "Breadth First" setBreadthFirst
                , model.searchMode
                    == DepthFirst
                    |> radioInput "depth-first" "searchType" "Depth First" setDepthFirst
                , model.searchMode
                    == Heuristic
                    |> radioInput "heuristic" "searchType" "Heuristic" setHeuristic
                , labels
                , startButton model
                , renderGridRow <|
                    List.map (renderSmallGrid Disabled False SelectGrid DeselectGrid) searchingModel.closed
                , renderGridRow <|
                    List.map (renderSmallGrid Loading False SelectGrid DeselectGrid) searchingModel.open
                ]
            , Html.div
                [ Attr.css
                    [ Tw.flex
                    , Tw.flex_row
                    , Tw.items_start
                    , Tw.justify_center
                    , Tw.relative
                    , Tw.basis_full
                    , Tw.overflow_auto
                    , Tw.bg_scroll
                    , Tw.py_4
                    , Tw.h_full
                    , Tw.border
                    , Tw.border_color Tw.neutral_700
                    , Tw.rounded_2xl
                    , Tw.shadow_lg
                    , Tw.shadow_color Tw.neutral_900
                    ]
                ]
                [ Html.div
                    [ Attr.css
                        [ Tw.flex
                        , Tw.flex_row
                        , Tw.max_w_full
                        ]
                    ]
                    [ renderNestedGrid model ]
                ]
            ]
        ]
    }


renderGridRow : List (Html.Html Msg) -> Html.Html Msg
renderGridRow =
    Html.div
        [ Attr.css
            [ Tw.max_w_lg
            , Tw.left_16
            , Tw.my_2
            , Tw.p_3
            , Tw.flex
            , Tw.flex_row
            , Tw.gap_4
            , Tw.justify_start
            , Tw.overflow_x_auto
            , Tw.border
            , Tw.border_color Tw.neutral_700
            , Tw.rounded_lg
            , Tw.shadow_lg
            , Tw.shadow_color Tw.neutral_900
            , Css.fontSize (Css.px 10)
            , Tw.h_20
            ]
        ]


startButton : Model -> Html.Html Msg
startButton model =
    let
        disabled =
            case model.state of
                Idle ->
                    False

                Failure _ ->
                    False

                _ ->
                    True
    in
    primaryButton disabled StartSearchingMsg


renderNestedGrid : Model -> Html.Html Msg
renderNestedGrid model =
    let
        searchingModel =
            getCurrentSearchingModel model

        (NestedGridModel nestedGridModel) =
            searchingModel.root

        rootGridState =
            getGridState searchingModel model nestedGridModel.current

        isSelected =
            isGridSelected model nestedGridModel.current
    in
    singleItemStyledNodeWrapper
        [ styledNode
            [ renderMdGrid rootGridState isSelected SelectGrid DeselectGrid nestedGridModel.current
            , renderChildren searchingModel model <| nestedGridModel.children
            ]
        ]


renderChildren : SearchingModel -> Model -> List NestedGrid -> Html.Html Msg
renderChildren searchingMmodel model children =
    let
        lstEl =
            if List.length children == 1 then
                singleItemStyledNodeWrapper

            else
                styledLstOfChildren
    in
    if children == [] then
        Html.div [] []

    else
        children
            |> List.map (nodeWithChildren searchingMmodel model)
            |> lstEl


nodeWithChildren : SearchingModel -> Model -> NestedGrid -> Html.Html Msg
nodeWithChildren searchingModel model (NestedGridModel nestedGrid) =
    let
        childGridState =
            getGridState searchingModel model nestedGrid.current

        isSelected =
            isGridSelected model nestedGrid.current
    in
    styledNode
        [ renderMdGrid childGridState isSelected SelectGrid DeselectGrid nestedGrid.current
        , renderChildren searchingModel model nestedGrid.children
        ]


getGridState : SearchingModel -> Model -> Grid -> GridState
getGridState searchingModel model grid =
    let
        goalFound =
            case model.state of
                GoalFound _ ->
                    True

                _ ->
                    False

        isIdle =
            case model.state of
                Idle ->
                    True

                _ ->
                    False
    in
    if grid.lines == goal.lines && goalFound then
        Success

    else if (grid.lines |> List.member) <| (searchingModel.closed |> List.map .lines) then
        Disabled

    else if grid.lines == searchingModel.x.lines && not isIdle then
        LoadingX

    else if (grid.lines |> List.member) <| (searchingModel.open |> List.map .lines) then
        Loading

    else
        Start


delayMessage200ms : Msg -> Cmd Msg
delayMessage200ms msg =
    Process.sleep 300
        |> Task.perform (\_ -> msg)


delayMessage2s : Msg -> Cmd Msg
delayMessage2s msg =
    Process.sleep 2000
        |> Task.perform (\_ -> msg)
