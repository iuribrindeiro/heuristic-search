module Core exposing (..)

import Json.Decode exposing (Error(..))
import Utils exposing (GenerateChildrenFailures, Grid, NestedGrid(..), Slot, findNested, generateChildrenOf, notIn, notMember)


type SearchMode
    = BreadthFirst
    | DepthFirst
    | Heuristic


type FindGoalResult
    = GoalFoundRs
    | KeepSearchingRs { children : List Grid, open : List Grid, closed : List Grid }
    | FailRs GenerateChildrenFailures


type FindXResult
    = XFound { x : Grid, open : List Grid }
    | LstIsEmpty


notGoalSlot : List Slot -> Slot -> Bool
notGoalSlot goalSlots z =
    (not << List.member z) goalSlots


getCountSlotsOutOfPlace : List Slot -> List Slot -> Int
getCountSlotsOutOfPlace goalSlots =
    List.filter (notGoalSlot goalSlots) >> List.length


calculateHeuristic : Grid -> Grid -> Int -> Int
calculateHeuristic rootGrid grid distanceFromRoot =
    let
        goalSlots =
            List.concat rootGrid.lines

        gridSlots =
            List.concat grid.lines

        countSlotsOutOfPlace =
            getCountSlotsOutOfPlace goalSlots gridSlots
    in
    countSlotsOutOfPlace + distanceFromRoot


findGoalIn : Grid -> Grid -> List Grid -> List Grid -> SearchMode -> NestedGrid -> FindGoalResult
findGoalIn x goal open closed searchMode root =
    if x.lines == goal.lines then
        GoalFoundRs

    else
        case generateChildrenOf x of
            Ok childrenOfX ->
                let
                    distanceFromRoot =
                        x |> calculateDistanceFromRoot root

                    nonClosedChildrenOfX =
                        childrenOfX
                            |> notIn closed

                    newChildren =
                        case searchMode of
                            Heuristic ->
                                childrenOfX
                                    |> List.map (getGridWithHeuristicOrId closed open goal distanceFromRoot)

                            _ ->
                                childrenOfX

                    newOpen =
                        case searchMode of
                            BreadthFirst ->
                                open ++ nonClosedChildrenOfX

                            DepthFirst ->
                                nonClosedChildrenOfX ++ open

                            Heuristic ->
                                open
                                    ++ newChildren
                                    |> filterWithHeuristic
                                    |> sortByHeuristic

                    newClosed =
                        x :: closed
                in
                KeepSearchingRs { children = newChildren, open = newOpen, closed = newClosed }

            Err failures ->
                FailRs failures


sortByHeuristic : List { lines : List Utils.Line, heuristic : Maybe Int } -> List { lines : List Utils.Line, heuristic : Maybe Int }
sortByHeuristic =
    List.sortBy (\z -> z.heuristic |> Maybe.withDefault 0)


filterWithHeuristic : List { lines : List Utils.Line, heuristic : Maybe Int } -> List { lines : List Utils.Line, heuristic : Maybe Int }
filterWithHeuristic =
    List.filter (\z -> (z.heuristic |> Maybe.withDefault 0) > 0)


getGridWithHeuristicOrId : List Grid -> List Grid -> Grid -> Int -> Grid -> Grid
getGridWithHeuristicOrId closed open goal distanceFromRoot g =
    if (g |> notMember closed) && (g |> notMember open) then
        gridWithHeuristic goal distanceFromRoot g

    else
        g


calculateDistanceFromRoot : NestedGrid -> Grid -> Int
calculateDistanceFromRoot root x =
    findNested x root
        |> (\(NestedGridModel z) -> z.level + 1)


gridWithHeuristic : Grid -> Int -> Grid -> Grid
gridWithHeuristic goal distanceFromRoot grid =
    { lines = grid.lines, heuristic = Just <| calculateHeuristic goal grid distanceFromRoot }


findX : List Grid -> FindXResult
findX open =
    case open of
        [] ->
            LstIsEmpty

        x :: xs ->
            XFound { x = x, open = xs }
