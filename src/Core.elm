module Core exposing (..)

import Json.Decode exposing (Error(..))
import Utils exposing (GenerateChildrenFailures, Grid, NestedGrid(..), Slot, findNested, generateChildrenOf, mapIf, notIn, notMember)


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
notGoalSlot goalSlots slot =
    (not << List.member slot) goalSlots


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
                    notMemberOfClosedOrOpen =
                        notMember (closed ++ open)

                    distanceFromRoot =
                        x |> calculateDistanceFromRoot root

                    toGridWithHeuristic =
                        gridWithHeuristic goal distanceFromRoot

                    nonClosedChildrenOfX =
                        childrenOfX
                            |> notIn closed

                    newChildren =
                        case searchMode of
                            Heuristic ->
                                childrenOfX
                                    |> mapIf notMemberOfClosedOrOpen toGridWithHeuristic

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
                                    |> onlyWithHeuristic
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


onlyWithHeuristic : List { lines : List Utils.Line, heuristic : Maybe Int } -> List { lines : List Utils.Line, heuristic : Maybe Int }
onlyWithHeuristic =
    List.filter (\z -> (z.heuristic |> Maybe.withDefault 0) > 0)


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
