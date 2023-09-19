module Pages.Home_ exposing (Model, Msg, page)

import Css
import Css.Global
import Html.Attributes exposing (style)
import Html.Events
import Html.Styled as Html
import Html.Styled.Attributes as Attr
import Page
import Process
import Request exposing (Request)
import Shared
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw
import Task
import Utils exposing (GenerateChildrenFailures, Grid, NestedGrid(..), generateChildrenOf, notIn, toGrid, toString)
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
    , [ 4, 8, 5 ]
    , [ 6, 7, 0 ]
    ]
        |> toGrid


type GridState
    = GridGoalFound
    | SearchingGoal
    | Closed
    | Start


type alias SearchingModel =
    { root : NestedGrid, x : Grid, open : List Grid, closed : List Grid }


type FailureMsgType
    = NoMoreElementsInList
    | FailureGeneratingChildren GenerateChildrenFailures


type alias FailureModel =
    { failure : FailureMsgType, current : SearchingModel }


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

        Failure { current } ->
            current


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
            ( { failure = failure, current = currentSearchingModel } |> Failure, Cmd.none )


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


bla : List (Html.Html msg)
bla =
    [ Html.div [] [ Html.text "bla" ]
    ]


view : Model -> View msg
view _ =
    { title = "Homepage"
    , body =
        [ Html.div [] [ Html.text "bla" ]
        ]

    -- Html.toUnstyled <|
    --     [ Html.div [ Attr.css [ Tw.bg_color Tw.gray_50 ] ]
    --         [ -- This will give us the standard tailwind style-reset as well as the fonts
    --           Css.Global.global Tw.globalStyles
    --         , Html.div
    --             [ Attr.css
    --                 [ Tw.mt_8
    --                 , Tw.flex
    --                 -- We use breakpoints like this
    --                 -- However, you need to order your breakpoints from high to low :/
    --                 , Breakpoints.lg [ Tw.mt_0, Tw.flex_shrink_0 ]
    --                 ]
    --             ]
    --             [ Html.div [ Attr.css [ Tw.inline_flex, Tw.rounded_md, Tw.shadow ] ]
    --                 [ Html.a
    --                     [ Attr.css
    --                         [ Tw.inline_flex
    --                         , Tw.items_center
    --                         , Tw.justify_center
    --                         , Tw.px_5
    --                         , Tw.py_3
    --                         , Tw.border
    --                         , Tw.border_color Tw.transparent
    --                         , Tw.text_base
    --                         , Tw.font_medium
    --                         , Tw.rounded_md
    --                         , Tw.text_color Tw.white
    --                         , Tw.bg_color Tw.indigo_600
    --                         -- We can use hover styles via elm-css :)
    --                         , Css.hover [ Tw.bg_color Tw.indigo_700 ]
    --                         ]
    --                     , Attr.href "#"
    --                     ]
    --                     [ Html.text "Get started" ]
    --                 ]
    --             ]
    --         ]
    --     ]
    }


getGridState : List Grid -> List Grid -> Bool -> Grid -> GridState
getGridState open closed goalFound x =
    if x == goal && goalFound then
        GridGoalFound

    else if (x |> List.member) <| open then
        SearchingGoal

    else if (x |> List.member) <| closed then
        Closed

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
                newOpen =
                    childrenOfX
                        |> notIn closed
                        |> List.append open

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
