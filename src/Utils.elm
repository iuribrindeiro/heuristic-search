module Utils exposing (..)

import Html exposing (b)
import List exposing (singleton)


type alias Slot =
    { lineIndex : Int, colIndex : Int, value : Maybe Int }


type alias Line =
    List Slot


type alias Grid =
    List Line


type alias EmptySlot =
    Slot


type GenerateChildrenFailures
    = NoEmptySlot Grid
    | NoSlotAboveEmptySlot EmptySlot
    | NoSlotUnderEmptySlot EmptySlot
    | NoSlotOverLeftEmptySlot EmptySlot
    | NoSlotOverRightEmptySlot EmptySlot


type NestedGrid
    = NestedGridModel { children : List NestedGrid, parent : Maybe NestedGrid, current : Grid }



-- UTIL FUNC


getCurrent : NestedGrid -> Grid
getCurrent (NestedGridModel nested) =
    nested.current


apply : a -> a -> (a -> b) -> ( b, b )
apply a1 a2 f =
    ( f a1, f a2 )


toArgs : (a -> a -> b) -> ( a, a ) -> b
toArgs f ( a, a1 ) =
    f a a1


findRoot : NestedGrid -> NestedGrid
findRoot (NestedGridModel nested) =
    case nested.parent of
        Just parent ->
            findRoot parent

        Nothing ->
            NestedGridModel nested


findNested : Grid -> NestedGrid -> NestedGrid
findNested target (NestedGridModel root) =
    if target == root.current then
        NestedGridModel root

    else if root.children == [] then
        NestedGridModel root

    else
        root.children
            |> List.map (findNested target)
            |> List.filter (\(NestedGridModel x) -> x.current == target)
            |> List.head
            |> Maybe.withDefault (NestedGridModel root)


nestedToGrid : NestedGrid -> List Grid
nestedToGrid (NestedGridModel nested) =
    [ [ nested.current ]
    , maybeNestedToLstGrid nested.parent
    ]
        |> List.concat


maybeNestedToLstGrid : Maybe NestedGrid -> List Grid
maybeNestedToLstGrid nested =
    Maybe.map nestedToGrid nested
        |> Maybe.withDefault []


toString : Slot -> String
toString slot =
    slot.value
        |> Maybe.withDefault 0
        |> (\x ->
                if x == 0 then
                    ""

                else
                    String.fromInt x
           )


notIn : List Grid -> List Grid -> List Grid
notIn grid2 grid1 =
    List.filter (\child -> List.member child grid2 |> not) grid1


toGrid : List (List Int) -> Grid
toGrid =
    List.indexedMap
        (\lineIndex line ->
            line
                |> List.indexedMap
                    (\colIndex value ->
                        { lineIndex = lineIndex
                        , colIndex = colIndex
                        , value =
                            if value == 0 then
                                Nothing

                            else
                                Just value
                        }
                    )
        )



-- MOVE FUNCTIONS


generateChildrenOf : Grid -> Result GenerateChildrenFailures (List Grid)
generateChildrenOf grid =
    findEmptySlot grid
        |> Result.andThen (tryMoveToEveryDirection grid)
        |> Result.map allNonEmptyLists


allNonEmptyLists : List Grid -> List Grid
allNonEmptyLists =
    List.isEmpty >> not |> List.filter


tryMoveToEveryDirection : Grid -> EmptySlot -> Result GenerateChildrenFailures (List Grid)
tryMoveToEveryDirection grid emptySlot =
    Result.map4
        combineMovementResults
        (tryMoveUp emptySlot grid)
        (tryMoveRight emptySlot grid)
        (tryMoveDown emptySlot grid)
        (tryMoveLeft emptySlot grid)


combineMovementResults : Grid -> Grid -> Grid -> Grid -> List Grid
combineMovementResults r1 r2 r3 r4 =
    [ r1
    , r2
    , r3
    , r4
    ]


tryMoveLeft : EmptySlot -> Grid -> Result GenerateChildrenFailures Grid
tryMoveLeft emptySlot grid =
    if emptySlot |> isInLastCol grid then
        Ok []

    else
        grid
            |> List.concat
            |> findSlotOnRightSideEmptySlot emptySlot
            |> rearangeGrid emptySlot grid


tryMoveRight : EmptySlot -> Grid -> Result GenerateChildrenFailures Grid
tryMoveRight emptySlot grid =
    if emptySlot |> isInFirstCol then
        Ok []

    else
        grid
            |> List.concat
            |> findSlotOnLeftSideEmptySlot emptySlot
            |> rearangeGrid emptySlot grid


tryMoveUp : EmptySlot -> Grid -> Result GenerateChildrenFailures Grid
tryMoveUp emptySlot grid =
    if emptySlot |> isInLastLine grid then
        Ok []

    else
        grid
            |> List.concat
            |> findSlotUnderEmptySlot emptySlot
            |> rearangeGrid emptySlot grid


tryMoveDown : EmptySlot -> Grid -> Result GenerateChildrenFailures Grid
tryMoveDown emptySlot grid =
    if emptySlot |> isInFirstLine then
        Ok []

    else
        grid
            |> List.concat
            |> findSlotAboveEmptySlot emptySlot
            |> rearangeGrid emptySlot grid


isInFirstLine : EmptySlot -> Bool
isInFirstLine emptySlot =
    emptySlot.lineIndex == 0


isInLastLine : Grid -> EmptySlot -> Bool
isInLastLine grid emptySlot =
    emptySlot.lineIndex == (List.length grid - 1)


isInLastCol : Grid -> EmptySlot -> Bool
isInLastCol grid emptySlot =
    emptySlot.colIndex == (List.length grid - 1)


isInFirstCol : EmptySlot -> Bool
isInFirstCol emptySlot =
    emptySlot.colIndex == 0


rearangeGrid : EmptySlot -> Grid -> Result GenerateChildrenFailures Slot -> Result GenerateChildrenFailures Grid
rearangeGrid emptySlot grid =
    Result.map
        (swapSlotValues emptySlot >> replaceSlotsInGrid grid)


replaceSlotsInGrid : Grid -> ( Slot, Slot ) -> Grid
replaceSlotsInGrid grid slots =
    grid
        |> List.map
            (List.map
                (\slot ->
                    if sameSlotIndex slot (Tuple.first slots) then
                        Tuple.first slots

                    else if sameSlotIndex slot (Tuple.second slots) then
                        Tuple.second slots

                    else
                        slot
                )
            )


sameSlotIndex : Slot -> Slot -> Bool
sameSlotIndex slot slot1 =
    slot.lineIndex == slot1.lineIndex && slot.colIndex == slot1.colIndex


swapSlotValues : Slot -> Slot -> ( Slot, Slot )
swapSlotValues slot1 slot2 =
    ( { slot1 | value = slot2.value }, { slot2 | value = slot1.value } )



-- FIND ADJAVENT SLOT FUNCTIONS


findSlotOnLeftSideEmptySlot : EmptySlot -> List Slot -> Result GenerateChildrenFailures Slot
findSlotOnLeftSideEmptySlot emptySlot =
    List.filter (byLineIndex emptySlot.lineIndex)
        >> List.filter (bySlotIndex (emptySlot.colIndex - 1))
        >> List.head
        >> noSlotOverLeftErr emptySlot


findSlotOnRightSideEmptySlot : EmptySlot -> List Slot -> Result GenerateChildrenFailures Slot
findSlotOnRightSideEmptySlot emptySlot =
    List.filter (byLineIndex emptySlot.lineIndex)
        >> List.filter (bySlotIndex (emptySlot.colIndex + 1))
        >> List.head
        >> noSlotOverRightErr emptySlot


findSlotUnderEmptySlot : EmptySlot -> List Slot -> Result GenerateChildrenFailures Slot
findSlotUnderEmptySlot emptySlot =
    List.filter (byLineIndex (emptySlot.lineIndex + 1))
        >> List.filter (bySlotIndex emptySlot.colIndex)
        >> List.head
        >> noSlotUnderErr emptySlot


findSlotAboveEmptySlot : EmptySlot -> List Slot -> Result GenerateChildrenFailures Slot
findSlotAboveEmptySlot emptySlot =
    List.filter (byLineIndex (emptySlot.lineIndex - 1))
        >> List.filter (bySlotIndex emptySlot.colIndex)
        >> List.head
        >> noSlotAboveErr emptySlot


noSlotAboveErr : EmptySlot -> Maybe a -> Result GenerateChildrenFailures a
noSlotAboveErr =
    Result.fromMaybe << NoSlotAboveEmptySlot


noSlotUnderErr : EmptySlot -> Maybe a -> Result GenerateChildrenFailures a
noSlotUnderErr =
    Result.fromMaybe << NoSlotUnderEmptySlot


noSlotOverLeftErr : EmptySlot -> Maybe a -> Result GenerateChildrenFailures a
noSlotOverLeftErr =
    Result.fromMaybe << NoSlotOverLeftEmptySlot


noSlotOverRightErr : EmptySlot -> Maybe a -> Result GenerateChildrenFailures a
noSlotOverRightErr =
    Result.fromMaybe << NoSlotOverRightEmptySlot


bySlotIndex : Int -> Slot -> Bool
bySlotIndex ind =
    \{ colIndex } -> colIndex == ind


byLineIndex : Int -> Slot -> Bool
byLineIndex ind =
    \{ lineIndex } -> lineIndex == ind



-- FIND EMPTY SLOT FUNCTIONS


findEmptySlot : Grid -> Result GenerateChildrenFailures EmptySlot
findEmptySlot grid =
    grid
        |> List.concat
        |> List.filter isNone
        |> List.head
        |> noEmptySlotErr grid


noEmptySlotErr : Grid -> Maybe a -> Result GenerateChildrenFailures a
noEmptySlotErr =
    Result.fromMaybe << NoEmptySlot


isNone : Slot -> Bool
isNone slot =
    Maybe.map (\_ -> False) slot.value |> Maybe.withDefault True
