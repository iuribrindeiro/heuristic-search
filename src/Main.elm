module Main exposing (..)

import Basics exposing (..)
import Browser
import Html exposing (Html, button, h1, table, text)
import Html.Events exposing (onClick)
import List exposing (head)
import Maybe exposing (andThen, map, withDefault)
import Process
import Result exposing (fromMaybe)
import String exposing (fromInt)
import Task
import Tuple exposing (pair, second)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type alias Table =
    List (List Int)


type alias EmptySlot =
    { line : Int
    , column : Int
    }


type alias LookingForGoal =
    { open : List Table
    , closed : List Table
    }


type Model
    = Idle
    | LookingForGoalModel LookingForGoal
    | GoalFound Table
    | Failure FailureType


type HorizontalMovement
    = MoveLeft
    | MoveRight


type VerticalMovement
    = MoveUp
    | MoveDown


type FailureType
    = EmptyX
    | FailToFindEmptySlot
    | FailToFindAdjacentSlotMovingUp
    | FailToFindAdjacentSlotMovingDown EmptySlot
    | FailToFindAdjacentSlotMovingLeft
    | FailToFindAdjacentSlotMovingRight


type Msg
    = StartLookingForGoal
    | KeepLookingForGoal LookingForGoal
    | FailureWhileLookingForGoal FailureType
    | GoalFoundMsg Table


start : Table
start =
    [ [ 1, 2, 3 ], [ 4, 5, 0 ], [ 6, 7, 8 ] ]


goal : Table
goal =
    [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 0 ] ]


init : a -> ( Model, Cmd Msg )
init _ =
    ( Idle, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        StartLookingForGoal ->
            { open = [ start ], closed = [] }
                |> lookForGoal

        KeepLookingForGoal lg ->
            lookForGoal lg

        FailureWhileLookingForGoal f ->
            ( Failure f, Cmd.none )

        GoalFoundMsg table ->
            ( GoalFound table, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Idle ->
            button [ onClick StartLookingForGoal ]
                [ text "Start looking for goal" ]

        LookingForGoalModel lg ->
            h1 [] [ text <| (toTexts lg.open |> String.join "\n") ]

        Failure f ->
            h1 [] [ text ("this is odd: " ++ failureToStr f) ]

        _ ->
            h1 [] [ text "hey" ]


failureToStr : FailureType -> String
failureToStr f =
    case f of
        EmptyX ->
            "EmptyX"

        FailToFindEmptySlot ->
            "FailToFindEmptySlot"

        FailToFindAdjacentSlotMovingUp ->
            "FailToFindAdjacentSlotMovingUp"

        FailToFindAdjacentSlotMovingDown empty ->
            "FailToFindAdjacentSlotMovingDown" ++ fromInt empty.line ++ fromInt empty.column

        FailToFindAdjacentSlotMovingLeft ->
            "FailToFindAdjacentSlotMovingLeft"

        FailToFindAdjacentSlotMovingRight ->
            "FailToFindAdjacentSlotMovingRight"


toTexts : List Table -> List String
toTexts tables =
    List.map mapToRawList tables
        |> List.map (\l -> List.map fromInt l)
        |> List.map (\l -> String.join ", " l)


lookForGoal : LookingForGoal -> ( Model, Cmd Msg )
lookForGoal lg =
    ( LookingForGoalModel lg, deepSearch lg )


deepSearch : LookingForGoal -> Cmd Msg
deepSearch model =
    findXOrFail model
        (\x ->
            if x == goal then
                GoalFoundMsg x |> toCmdMsg

            else
                model
                    |> addXToClosed x
                    |> generateChildrenOfX x
                    |> addNonClosedChildrenToOpen model
                    |> mapResultToMsg
                    |> toCmdMsg
        )


findXOrFail : LookingForGoal -> (Table -> Cmd Msg) -> Cmd Msg
findXOrFail lg onFound =
    case findX lg.open of
        Just x ->
            onFound x

        Nothing ->
            FailureWhileLookingForGoal EmptyX |> toCmdMsg


findX : List Table -> Maybe Table
findX open =
    head open


toCmdMsg : Msg -> Cmd Msg
toCmdMsg msg =
    Process.sleep 2000
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity


mapResultToMsg : Result FailureType LookingForGoal -> Msg
mapResultToMsg maybeModel =
    case maybeModel of
        Ok model ->
            KeepLookingForGoal model

        Err e ->
            FailureWhileLookingForGoal e


addNonClosedChildrenToOpen : LookingForGoal -> Result FailureType (List Table) -> Result FailureType LookingForGoal
addNonClosedChildrenToOpen model =
    Result.map (\c -> { model | open = c |> notClosed model })


notClosed : LookingForGoal -> List Table -> List Table
notClosed model children =
    List.filter
        (\c -> List.member c model.closed |> not)
        children


addXToClosed : Table -> LookingForGoal -> LookingForGoal
addXToClosed x model =
    { model | closed = x :: model.closed }


generateChildrenOfX : Table -> LookingForGoal -> Result FailureType (List Table)
generateChildrenOfX x _ =
    findEmptySlot x
        |> Result.andThen (tryMoveToEveryDirection x)
        |> Result.map (List.filter (List.isEmpty >> not))


tryMoveToEveryDirection : Table -> EmptySlot -> Result FailureType (List Table)
tryMoveToEveryDirection x emptySlot =
    Result.map4
        (\a b c d ->
            [ a, b, c, d ]
        )
        (tryMoveUp x emptySlot)
        (tryMoveRight x emptySlot)
        (tryMoveDown x emptySlot)
        (tryMoveLeft x emptySlot)


tryMoveLeft : Table -> EmptySlot -> Result FailureType Table
tryMoveLeft currentTable emptySlot =
    let
        columnsSize =
            List.head currentTable
                |> map lengthMinus1
                |> withDefault 0
    in
    if emptySlot.column < columnsSize then
        moveHorizontaly MoveLeft currentTable emptySlot

    else
        Ok []


tryMoveRight : Table -> EmptySlot -> Result FailureType Table
tryMoveRight currentTable emptySlot =
    if emptySlot.column > 0 then
        moveHorizontaly MoveRight currentTable emptySlot

    else
        Ok []


tryMoveUp : Table -> EmptySlot -> Result FailureType Table
tryMoveUp emptyTable emptySlot =
    if emptySlot.line < List.length emptyTable then
        moveVertically MoveUp emptyTable emptySlot

    else
        Ok []


tryMoveDown : Table -> EmptySlot -> Result FailureType Table
tryMoveDown currentTable emptySlot =
    if emptySlot.line > 0 then
        moveVertically MoveDown currentTable emptySlot

    else
        Ok []


moveVertically : VerticalMovement -> Table -> EmptySlot -> Result FailureType Table
moveVertically movement x emptySlot =
    let
        adjacentVal =
            movement
                |> getSwappedVerticalDirection
                |> getVerticalAdjacentVal x emptySlot
    in
    adjacentVal
        |> Result.map (swapValues x)


moveHorizontaly : HorizontalMovement -> Table -> EmptySlot -> Result FailureType Table
moveHorizontaly movement currentTable emptySlot =
    let
        adjacentVal =
            movement
                |> getSwappedHorizontalDirection
                |> getHorizontalAdjacentVal currentTable emptySlot
    in
    adjacentVal
        |> Result.map (swapValues currentTable)


swapValues : Table -> (Int -> Table)
swapValues x val =
    x
        |> mapToRawList
        |> List.map (swapEmptyVal val)
        |> toTable x


toTable : Table -> List Int -> Table
toTable x tableRaw =
    let
        itemsToDrop ind =
            ind * List.length x

        itemsToTake =
            List.length x
    in
    tableRaw
        |> List.indexedMap
            (\i _ -> tableRaw |> (List.drop (itemsToDrop i) >> List.take itemsToTake))
        |> List.filter (List.isEmpty >> not)


mapToRawList : Table -> List Int
mapToRawList =
    List.concat


swapEmptyVal : Int -> Int -> Int
swapEmptyVal adjacentVal val =
    if adjacentVal == val then
        0

    else if val == 0 then
        adjacentVal

    else
        val


getSwappedVerticalDirection : VerticalMovement -> VerticalMovement
getSwappedVerticalDirection movement =
    case movement of
        MoveUp ->
            MoveDown

        MoveDown ->
            MoveUp


getSwappedHorizontalDirection : HorizontalMovement -> HorizontalMovement
getSwappedHorizontalDirection movement =
    case movement of
        MoveLeft ->
            MoveRight

        MoveRight ->
            MoveLeft


getHorizontalAdjacentVal : Table -> EmptySlot -> HorizontalMovement -> Result FailureType Int
getHorizontalAdjacentVal currentTable emptySlot movement =
    case movement of
        MoveRight ->
            currentTable
                |> findLineWithEmptySlot emptySlot
                |> findAdjacentColVal emptySlot 1
                |> Result.fromMaybe FailToFindAdjacentSlotMovingRight

        MoveLeft ->
            currentTable
                |> findLineWithEmptySlot emptySlot
                |> findAdjacentColVal emptySlot -1
                |> Result.fromMaybe FailToFindAdjacentSlotMovingLeft


getVerticalAdjacentVal : Table -> EmptySlot -> VerticalMovement -> Result FailureType Int
getVerticalAdjacentVal currentTable emptySlot movement =
    case movement of
        MoveUp ->
            currentTable
                |> findAdjacentLineVal emptySlot -1
                |> findColWithEmptySlot emptySlot
                |> Result.fromMaybe FailToFindAdjacentSlotMovingUp

        MoveDown ->
            currentTable
                |> findAdjacentLineVal emptySlot 1
                |> findColWithEmptySlot emptySlot
                |> Result.fromMaybe (FailToFindAdjacentSlotMovingDown emptySlot)


findAdjacentColVal : EmptySlot -> Int -> Maybe (List b) -> Maybe b
findAdjacentColVal emptySlot diff =
    emptySlot.column
        + diff
        |> findByIndex
        |> andThen


findAdjacentLineVal : EmptySlot -> Int -> List b -> Maybe b
findAdjacentLineVal emptySlot diff =
    emptySlot.line
        + diff
        |> findByIndex


findLineWithEmptySlot : EmptySlot -> Table -> Maybe (List Int)
findLineWithEmptySlot emptySlot =
    findByIndex emptySlot.line


findColWithEmptySlot : EmptySlot -> Maybe (List b) -> Maybe b
findColWithEmptySlot emptySlot =
    findByIndex emptySlot.column
        |> andThen


findByIndex : Int -> List b -> Maybe b
findByIndex index =
    List.indexedMap pair
        >> List.filter (\( i, _ ) -> i == index)
        >> List.map second
        >> List.head


findEmptySlot : Table -> Result FailureType EmptySlot
findEmptySlot =
    List.indexedMap pair
        >> List.filter (\( _, r ) -> List.member 0 r)
        >> List.map (\( i, v ) -> ( i, v |> List.filter (\x -> x == 0) |> List.head ))
        >> List.head
        >> andThen (\( line, maybeCol ) -> map (\c -> { line = c, column = line }) maybeCol)
        >> Result.fromMaybe EmptyX


lengthMinus1 : List Int -> Int
lengthMinus1 =
    List.length >> (-) 1


traverseMaybe : List (Maybe a) -> Maybe (List a)
traverseMaybe listOfMaybeItems =
    let
        listOfItemsWithVal =
            listOfMaybeItems
                |> List.filterMap identity
    in
    if List.length listOfItemsWithVal < List.length listOfMaybeItems then
        Nothing

    else
        Just listOfItemsWithVal


isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False
