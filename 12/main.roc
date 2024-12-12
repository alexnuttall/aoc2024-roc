app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    util: "../util/util.roc",
    # answers: "../answers/answers.roc",
}
# import "./input.txt" as inputData : Str
# import answers.A exposing [answers]
import util.ListUtil

parse : Str -> Grid
parse = \str ->
    Str.toUtf8 str
    |> List.splitOn '\n'
    |> List.walkWithIndex (Dict.empty {}) \dict, row, y ->
        List.walkWithIndex row dict \rowDict, cell, x ->
            Dict.insert rowDict { x: Num.toI16 x, y: Num.toI16 y } cell

Pos : { x : I16, y : I16 }
Grid : Dict Pos U8
Heading : [N, E, S, W]
Fence : (Heading, Pos)

solve1 : Grid -> U64
solve1 = \input ->
    getRegions input
    |> ListUtil.sumBy \region ->
        perimeter region |> Set.len |> Num.mul (Set.len region)

getRegions : Grid -> List (Set Pos)
getRegions = \input ->
    getRegion = \gridState, regions ->
        when dictFirst gridState is
            Ok (pos, id) ->
                region = flood gridState pos id (Set.empty {}) (Set.empty {})

                getRegion
                    (Dict.dropIf gridState \(position, _id) -> Set.contains region position)
                    (List.append regions region)

            Err Empty -> regions

    getRegion input []

flood : Grid, Pos, U8, Set Pos, Set Pos -> Set Pos
flood = \grid, pos, id, visited, unvisited ->
    south = traverseDirection grid S pos id visited unvisited

    mergedVisited = Set.union visited south.visited
    mergedUnvisited = Set.union unvisited south.unvisited

    final = traverseDirection grid E pos id mergedVisited mergedUnvisited

    when takeSet final.unvisited is
        Ok (nextUnvisitedPos, nextUnvisited) ->
            flood grid nextUnvisitedPos id final.visited nextUnvisited

        Err Empty -> final.visited

traverseDirection : Grid, Heading, Pos, U8, Set Pos, Set Pos -> { visited : Set Pos, unvisited : Set Pos }
traverseDirection = \grid, heading, pos, id, visited, unvisited ->
    target = move pos heading
    nextVisited = Set.insert visited pos
    nextUnvisited = unvisitedNeighbours grid heading pos id visited |> Set.union unvisited

    if Set.contains nextVisited target then
        { visited: nextVisited, unvisited: nextUnvisited }
    else
        when Dict.get grid target is
            Ok nextId if nextId == id ->
                traverseDirection grid heading target id nextVisited nextUnvisited

            _ -> { visited: nextVisited, unvisited: nextUnvisited }

unvisitedNeighbours : Grid, Heading, Pos, U8, Set Pos -> Set Pos
unvisitedNeighbours = \grid, heading, pos, id, visited ->
    lateralHeadings heading
    |> \(left, right) -> [move pos left, move pos right]
    |> List.dropIf \lateral -> Set.contains visited lateral
    |> List.keepOks \lateral ->
        when Dict.get grid lateral is
            Ok neighbourId if neighbourId == id -> Ok lateral
            _ -> Err None
    |> Set.fromList

perimeter : Set Pos -> Set Fence
perimeter = \region ->
    Set.walk region (Set.empty {}) \fences, pos ->
        [(N, move pos N), (E, move pos E), (S, move pos S), (W, move pos W)]
        |> List.dropIf \(_, position) -> Set.contains region position
        |> Set.fromList
        |> Set.union fences

solve2 : Grid -> U64
solve2 = \input ->
    getRegions input
    |> ListUtil.sumBy \region ->
        perimeter region |> groupSides |> List.len |> Num.mul (Set.len region)

groupSides : Set Fence -> List (Set Fence)
groupSides = \fences ->
    getSides = \fencesState, sides ->
        when takeSet fencesState is
            Ok (fence, others) ->
                (orientation, _) = fence
                searchDirections = lateralHeadings orientation
                side = traverseFence others fence searchDirections

                getSides
                    (Set.difference fencesState side)
                    (List.append sides side)

            Err Empty -> sides

    getSides fences []

traverseFence : Set Fence, Fence, (Heading, Heading) -> Set Fence
traverseFence = \fences, fence, (firstDir, secondDir) ->
    direction = \f, heading, visited ->
        nextVisited = Set.insert visited f

        (fenceType, orientation) = f
        nextFence = (fenceType, move orientation heading)

        if Set.contains fences nextFence then
            direction nextFence heading nextVisited
        else
            nextVisited

    Set.single fence
    |> Set.union (direction fence firstDir (Set.empty {}))
    |> Set.union (direction fence secondDir (Set.empty {}))

dictFirst = \dict -> Dict.walkUntil dict (Err Empty) \_, k, v -> Break (Ok (k, v))

takeSet = \set ->
    when Set.walkUntil set (Err Empty) \_, a -> Break (Ok a) is
        Ok a -> Ok (a, Set.remove set a)
        _ -> Err Empty

move = \{ x, y }, heading ->
    when heading is
        N -> { x, y: y - 1 }
        E -> { x: x + 1, y }
        S -> { x, y: y + 1 }
        W -> { x: x - 1, y }

lateralHeadings = \heading ->
    when heading is
        N | S -> (W, E)
        E | W -> (N, S)

part1 = \input -> parse input |> solve1 |> Num.toStr |> Ok
part2 = \input -> parse input |> solve2 |> Num.toStr |> Ok

example =
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

expect
    actual = part1 example
    actual == Ok "1930"

# expect
#     actual = part1 inputData
#     actual == Ok answers.day12.part1

expect
    actual = part2 example
    actual == Ok "1206"

# expect
#     actual = part2 inputData
#     actual == Ok answers.day12.part2
