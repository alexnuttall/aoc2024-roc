app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    util: "../util/util.roc",
    answers: "../answers/answers.roc",
}

import "./input.txt" as inputData : List U8
import answers.A exposing [answers]

main = Task.ok {}

Grid : Dict Pos U8
Pos : (I64, I64)
Orientation : [N, E, S, W]
Matrix : List (List U8)

parse : List U8 -> Matrix
parse = \bytes -> List.splitOn bytes '\n'

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

        Err _ ->
            seen |> Ok

toDict = \matrix ->
    List.walkWithIndex matrix (Dict.empty {}) \dict, row, y ->
        List.walkWithIndex row (Dict.empty {}) \rowDict, cell, x ->
            Dict.insert rowDict (Num.toI64 x, Num.toI64 y) cell
        |> Dict.insertAll dict

solve1 : Matrix -> Result U64 _
solve1 = \matrix ->
    grid = toDict matrix
    start = try findGuard grid
    orientation = N
    seen = Set.single (start, orientation)

    path = try patrol grid start orientation seen

    Set.map path .0
    |> Set.len
    |> Ok

solve2 : Matrix -> Result U64 _
solve2 = \matrix ->
    grid = toDict matrix
    start = try findGuard grid
    orientation = N
    seen = Set.single (start, orientation)

    Dict.walk grid 0 \count, pos, cell ->
        if cell == '#' || cell == '^' then
            count
        else
            loopResult =
                Dict.insert grid pos '#'
                |> patrol start orientation seen

            when loopResult is
                Ok _ -> count
                Err Looped -> count + 1
    |> Ok

exampleData = Str.toUtf8
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
    actual = parse exampleData |> solve1
    actual == Ok 41

expect
    actual = parse inputData |> solve1
    actual == Ok answers.day06.part1

expect
    actual = parse exampleData |> solve2
    actual == Ok 6

expect
    actual = parse inputData |> solve2
    actual == Ok answers.day06.part2
