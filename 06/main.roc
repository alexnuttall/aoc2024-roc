app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    util: "../util/util.roc",
    answers: "../answers/answers.roc",
}

import "./input.txt" as inputData : Str
import answers.A exposing [answers]

Grid : Dict Pos U8
Pos : (I64, I64)
Orientation : [N, E, S, W]

parse : Str -> Grid
parse = \str -> Str.splitOn str "\n"
    |> List.walkWithIndex (Dict.empty {}) \dict, row, y ->
        Str.toUtf8 row
        |> List.walkWithIndex (Dict.empty {}) \rowDict, cell, x ->
            Dict.insert rowDict (Num.toI64 x, Num.toI64 y) cell
        |> Dict.insertAll dict

findGuard : Grid -> Result Pos [NotFound]
findGuard = \grid ->
    Dict.walkUntil grid (Err NotFound) \state, pos, cell ->
        if cell == '^' then
            Break (Ok pos)
        else
            Continue state

nextPos : Pos, Orientation -> Pos
nextPos = \(x, y), orientation ->
    when orientation is
        N -> (x, y - 1)
        E -> (x + 1, y)
        S -> (x, y + 1)
        W -> (x - 1, y)

turn : Orientation -> Orientation
turn = \orientation ->
    when orientation is
        N -> E
        E -> S
        S -> W
        W -> N

patrol : Grid, Pos, Orientation, Set (Pos, Orientation) -> Result (Set (Pos, Orientation)) _
patrol = \grid, current, orientation, seen ->
    target = nextPos current orientation
    seenCount = Set.len seen

    when Dict.get grid target is
        Ok '#' ->
            patrol grid current (turn orientation) seen

        Ok _ ->
            nextSeen = Set.insert seen (target, orientation)
            if Set.len nextSeen == seenCount then
                Err Looped
            else
                patrol grid target orientation nextSeen

        Err _ -> Ok seen

part1 : Str -> Result Str _
part1 = \str ->
    grid = parse str
    start = try findGuard grid
    seen = Set.single (start, N)

    try patrol grid start N seen
    |> Set.map .0
    |> Set.len
    |> Num.toStr
    |> Ok

part2 : Str -> Result Str _
part2 = \str ->
    grid = parse str
    start = try findGuard grid

    try patrol grid start N (Set.empty {})
    |> Set.map .0
    |> Set.walk 0 \count, pos ->
        when
            Dict.insert grid pos '#'
            |> patrol start N (Set.empty {})
        is
            Ok _ -> count
            Err Looped -> count + 1
    |> Num.toStr
    |> Ok

exampleData =
    """
    ....#.....
    .........#
    ..........
    ..#.......
    .......#..
    ..........
    .#..^.....
    ........#.
    #.........
    ......#...
    """

expect
    actual = part1 exampleData
    actual == Ok "41"

expect
    actual = part1 inputData
    actual == Ok answers.day06.part1

expect
    actual = part2 exampleData
    actual == Ok "6"

expect
    actual = part2 inputData
    actual == Ok answers.day06.part2
