app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    util: "../util/util.roc",
    answers: "../answers/answers.roc",
}
import "./input.txt" as inputData : Str
import answers.A exposing [answers]
import util.ListUtil

parse : Str -> Grid
parse = \str ->
    Str.toUtf8 str
    |> List.splitOn '\n'
    |> List.walkWithIndex (Dict.empty {}) \dict, row, y ->
        List.walkWithIndex row dict \rowDict, cell, x ->
            Dict.insert rowDict { x: Num.toI16 x, y: Num.toI16 y } cell

Id : U8
Pos : { x : I16, y : I16 }
Grid : Dict Pos Id
Heading : [N, E, S, W]
Fence : [Northern Pos, Eastern Pos]

anyFromDict = \dict ->
    Dict.walkUntil dict (Err Empty) \_, k, v -> Break (Ok (k, v))

takeSet = \set ->
    when Set.walkUntil set (Err Empty) \_, a -> Break (Ok a) is
        Ok a -> Ok (a, Set.remove set a)
        _ -> Err Empty

nextPos = \{ x, y }, heading ->
    when heading is
        N -> { x, y: y - 1 }
        E -> { x: x + 1, y }
        S -> { x, y: y + 1 }
        W -> { x: x - 1, y }

potentialUnvisitedPositions = \pos, heading ->
    when heading is
        N | S -> [nextPos pos W, nextPos pos E]
        E | W -> [nextPos pos N, nextPos pos S]

unvisitedNeighbours : Grid, Heading, Pos, Id, Set Pos -> Set Pos
unvisitedNeighbours = \grid, heading, pos, id, visited ->
    potentialUnvisitedPositions pos heading
    |> List.keepIf \neighbourPos ->
        Set.contains visited neighbourPos
        |> Bool.not
    |> List.keepOks \neighbourPos ->
        when Dict.get grid neighbourPos is
            Ok neighbourId if neighbourId == id -> Ok neighbourPos
            _ -> Err None
    |> Set.fromList

searchDirectionally : Grid, Heading, Pos, Id, Set Pos, Set Pos -> { visited : Set Pos, unvisited : Set Pos }
searchDirectionally = \grid, heading, pos, id, visited, unvisited ->
    target = nextPos pos heading
    nextVisited = Set.insert visited pos
    nextUnvisited =
        unvisitedNeighbours grid heading pos id visited
        |> Set.union unvisited

    if Set.contains visited target then
        { visited: nextVisited, unvisited: nextUnvisited }
    else
        when Dict.get grid target is
            Ok nextId if nextId == id ->
                searchDirectionally
                    grid
                    heading
                    target
                    id
                    nextVisited
                    nextUnvisited

            _ -> { visited: nextVisited, unvisited: nextUnvisited }

search : Grid, Pos, Id, Set Pos, Set Pos -> Set Pos
search = \grid, pos, id, visited, unvisited ->
    { visited: visitedS, unvisited: unvisitedS } =
        searchDirectionally grid S pos id visited unvisited

    { visited: nextVisited, unvisited: finalUnvisited } =
        searchDirectionally grid E pos id (Set.union visited visitedS) (Set.union unvisited unvisitedS)

    when takeSet finalUnvisited is
        Ok (nextUnvisitedPos, nextUnvisited) ->
            search grid nextUnvisitedPos id nextVisited nextUnvisited

        Err Empty -> nextVisited

perimeter : Set Pos -> Set Fence
perimeter = \region ->
    Set.walk region (Set.empty {}) \fences, { x, y } ->
        List.keepIf
            [
                (N, { x, y: y - 1 }),
                (E, { x: x + 1, y }),
                (S, { x, y: y + 1 }),
                (W, { x: x - 1, y }),
            ]
            \(_, pos) -> Set.contains region pos |> Bool.not
        |> List.map \(neighbourHeading, _) ->
            when neighbourHeading is
                N -> Northern { x, y }
                E -> Eastern { x, y }
                S -> Northern { x, y: y + 1 }
                W -> Eastern { x: x - 1, y }
        |> Set.fromList
        |> Set.union fences

getRegions : Grid -> List (Id, Set Pos)
getRegions = \input ->
    loop = \gridState, regions ->
        when anyFromDict gridState is
            Ok (pos, id) ->
                region = search gridState pos id (Set.empty {}) (Set.empty {})
                nextGrid = Dict.dropIf gridState \(k, _) -> Set.contains region k
                nextRegions = List.append regions (id, region)

                loop nextGrid nextRegions

            Err _ -> regions

    loop input []

solve1 : Grid -> _
solve1 = \input ->
    getRegions input
    |> ListUtil.sumBy \(_, region) ->
        perimeterLength = perimeter region |> Set.len
        Set.len region * perimeterLength

perimeterSides = \fences ->
    searchFenceDirectionally = \fs, fencePos, fenceType, heading, visited ->
        nextVisited = Set.insert visited (fenceType fencePos)
        next = nextPos fencePos heading

        if Set.contains fs (fenceType next) then
            searchFenceDirectionally fs next fenceType heading nextVisited
        else
            nextVisited

    searchFence = \fs, fence ->
        when fence is
            Northern northOf ->
                Set.single fence
                |> Set.union (searchFenceDirectionally fs northOf Northern W (Set.empty {}))
                |> Set.union (searchFenceDirectionally fs northOf Northern E (Set.empty {}))

            Eastern eastOf ->
                Set.single fence
                |> Set.union (searchFenceDirectionally fs eastOf Eastern N (Set.empty {}))
                |> Set.union (searchFenceDirectionally fs eastOf Eastern S (Set.empty {}))

    getSides = \fencesState, sides ->
        when takeSet fencesState is
            Ok (fence, others) ->
                side = searchFence others fence
                getSides
                    (Set.difference fencesState side)
                    (List.append sides side)

            _ -> sides

    getSides fences []

solve2 : Grid -> _
solve2 = \input ->
    getRegions input
    |> ListUtil.sumBy \(_, region) ->
        sideCount = perimeter region |> perimeterSides |> dbg |> List.len
        Set.len region * sideCount

part1 = \input -> parse input |> solve1 |> Num.toStr |> Ok
part2 = \input -> parse input |> solve2 |> Num.toStr |> Ok

smallExample =
    """
    AAAA
    BBCD
    BBCC
    EEEC
    """

largeExample =
    """
    RRRRIICCFF
    RRRRIICCCF
    VVRRRCCFFF
    VVRCCCJFFF
    VVVVCJJCFE
    VVIVCCJJEE
    VVIIICJJEE
    MIIIIIJJEE
    MIIISIJEEE
    MMMISSJEEE
    """

eShapeExample =
    """
    EEEEE
    EXXXX
    EEEEE
    EXXXX
    EEEEE
    """

abExample =
    """
    AAAAAA
    AAABBA
    AAABBA
    ABBAAA
    ABBAAA
    AAAAAA
    """

xoExample =
    """
    OOOOO
    OXOXO
    OOOOO
    OXOXO
    OOOOO
    """

# expect
#     actual = part1 largeExample
#     actual == Ok "1930"

# expect
#     actual = part1 inputData
#     actual == Ok answers.day12.part1

# expect
#     actual = part2 largeExample
#     actual == Ok "1206"

# expect
#     actual = part2 eShapeExample
#     actual == Ok "236"

# expect
#     actual = part2 smallExample
#     actual == Ok "80"

# expect
#     actual = part2 xoExample
#     actual == Ok "436"

expect
    actual = part2 abExample
    actual == Ok "368"
# 312

# expect
#     actual = part2 inputData
#     actual == Ok answers.day12.part2
