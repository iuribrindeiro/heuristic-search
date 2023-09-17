module Pages.Home_ exposing (Model, Msg, page)

import Html exposing (button, div, text)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (onClick)
import Page exposing (Page)
import Process
import Request exposing (Request)
import Task
import Utils exposing (GenerateChildrenFailures, Grid, fromList, generateChildrenOf, notIn, toString)
import View exposing (View)


goal : Grid
goal =
    [ [ 1, 2, 3 ]
    , [ 4, 5, 6 ]
    , [ 7, 8, 0 ]
    ]
        |> fromList


start : Grid
start =
    [ [ 1, 2, 3 ]
    , [ 4, 5, 0 ]
    , [ 7, 8, 6 ]
    ]
        |> fromList


type Grids
    = Grids { children : List Grids, parent : Maybe Grids, current : Grid }


type alias SearchingModel =
    { x : Grid, open : List Grid, closed : List Grid }


type Failure
    = NoMoreElementsInList
    | FailureGeneratingChildren GenerateChildrenFailures


type alias GoalFoundModel =
    { goalFound : Grid, closed : List Grid }


type Model
    = Idle { open : List Grid, closed : List Grid }
    | Seaching SearchingModel
    | GoalFound GoalFoundModel
    | Failure Failure


type Msg
    = GoalFoundMsg GoalFoundModel
    | FailureMsg Failure
    | SearchingMsg SearchingModel
    | StartSearchingMsg


page shared req =
    Page.element { init = init, update = update, view = view, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions =
    \_ -> Sub.none


init : ( Model, Cmd Msg )
init =
    ( Idle { open = [ start ], closed = [] }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GoalFoundMsg x ->
            ( GoalFound x, Cmd.none )

        FailureMsg failure ->
            ( Failure failure, Cmd.none )

        SearchingMsg { x, open, closed } ->
            ( Seaching { x = x, open = open, closed = closed }, findGoal open closed )

        StartSearchingMsg ->
            ( Seaching { x = start, open = [], closed = [] }, findGoal [ start ] [] )


view : Model -> View Msg
view model =
    { title = "Homepage"
    , body =
        [ div
            [ style "display" "flex"
            , style "flex-direction" "column"
            , style "margin" "auto"
            , style "margin-top" "20vh"
            ]
            [ div
                [ style "display" "flex"
                , style "flex-direction" "column"
                , style "align-items" "center"
                ]
                [ findGoalButton model
                , case model of
                    Idle e ->
                        renderGrid start e.open e.closed

                    Seaching { x, open, closed } ->
                        div
                            [ style "display" "flex", style "flex-direction" "column" ]
                            (List.append
                                [ renderGrid x open closed
                                ]
                                [ div
                                    [ style "display" "flex"
                                    , style "flex-direction" "row"
                                    , style "justify-content" "center"
                                    ]
                                    (List.map
                                        (\g -> div [ style "margin" "5px" ] [ renderGrid g open closed ])
                                        open
                                    )
                                ]
                            )

                    GoalFound x ->
                        renderGrid x.goalFound [] x.closed

                    Failure _ ->
                        text "Failure"
                ]
            ]
        ]
    }


findGoalButton : Model -> Html.Html Msg
findGoalButton model =
    let
        disab =
            case model of
                Idle _ ->
                    False

                Seaching _ ->
                    True

                GoalFound _ ->
                    False

                Failure _ ->
                    False

        txtOnModel =
            case model of
                Idle _ ->
                    "Click here to find the goal!"

                Seaching _ ->
                    "Searching..."

                GoalFound _ ->
                    "Goal found! Try again :)"

                Failure _ ->
                    ":( Click here to try again"
    in
    button
        [ onClick StartSearchingMsg
        , disabled disab
        , style "margin-bottom" "20px"
        ]
        [ text txtOnModel
        ]


renderGrid : Grid -> List Grid -> List Grid -> Html.Html msg
renderGrid grid open closed =
    let
        color =
            if List.member grid closed then
                "gray"

            else if List.member grid open then
                "blue"

            else if grid == start then
                "blue"

            else
                "green"
    in
    div
        []
        (List.map
            (\row ->
                div
                    [ style "display" "flex"
                    , style "justify-content" "center"
                    , style "color" color
                    ]
                    (List.map
                        (\slot ->
                            div
                                [ style "width" "50px"
                                , style "height" "50px"
                                , style "border" ("1px solid " ++ color)
                                , style "justify-content" "center"
                                , style "align-items" "center"
                                , style "display" "flex"
                                ]
                                [ text (toString slot) ]
                        )
                        row
                    )
            )
            grid
        )


findGoal : List Grid -> List Grid -> Cmd Msg
findGoal open closed =
    case findX open of
        Just ( x, openWithoutX ) ->
            if x == goal then
                GoalFoundMsg { goalFound = x, closed = open ++ closed } |> delayMessage

            else
                x |> generateChildrenAndSearch closed openWithoutX

        Nothing ->
            FailureMsg NoMoreElementsInList
                |> delayMessage


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
            SearchingMsg { x = x, open = newOpen, closed = newClosed }
                |> delayMessage

        Err failures ->
            FailureGeneratingChildren failures
                |> FailureMsg
                |> delayMessage


findX : List Grid -> Maybe ( Grid, List Grid )
findX list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            ( x, xs ) |> Just


delayMessage : Msg -> Cmd Msg
delayMessage msg =
    Process.sleep 1000
        |> Task.perform (\_ -> msg)
