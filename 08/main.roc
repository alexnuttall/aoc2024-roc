app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    util: "../util/util.roc",
    answers: "../answers/answers.roc",
}

import "./input.txt" as inputData : Str
import util.DictUtil
import util.ListUtil
import answers.A exposing [answers]

Frequency : U8
Matrix : List (List Frequency)
Pos : (I16, I16)
NodeGroups : List (List Pos)
Bounds : { height : I16, width : I16 }
Resonance : (Pos, Pos) -> List Pos

parse : Str -> Matrix
parse = \str -> Str.toUtf8 str |> List.splitOn '\n'

findNodes : Matrix -> NodeGroups
findNodes = \matrix ->
    List.walkWithIndex matrix (Dict.empty {}) \dict, row, y ->
        List.walkWithIndex row (Dict.empty {}) \rowDict, cell, x ->
            when cell is
                '.' -> rowDict
                f -> Dict.insert rowDict (Num.toI16 x, Num.toI16 y) f
        |> Dict.insertAll dict
    |> DictUtil.invert
    |> Dict.values

getBounds : Matrix -> Bounds
getBounds = \matrix -> {
    height: List.len matrix |> Num.toI16,
    width: List.get matrix 0 |> Result.withDefault [] |> List.len |> Num.toI16,
}

inBounds : Bounds -> (Pos -> Bool)
inBounds = \{ height, width } -> \(x, y) -> x >= 0 && x < width && y >= 0 && y < height

antinodesOnce : Bounds -> Resonance
antinodesOnce = \bounds ->
    \((ax, ay), (bx, by)) ->
        [
            (ax - (ax - bx) * 2, ay - (ay - by) * 2),
            (bx - (bx - ax) * 2, by - (by - ay) * 2),
        ]
        |> List.keepIf (inBounds bounds)

countUniqueAntinodes : Matrix, (Bounds -> Resonance) -> U64
countUniqueAntinodes = \matrix, resonateWithBounds ->
    findNodes matrix
    |> List.map \group ->
        ListUtil.pairs group
        |> List.joinMap (getBounds matrix |> resonateWithBounds)
        |> Set.fromList
    |> ListUtil.joinSets
    |> Set.len

part1 : Str -> Result Str _
part1 = \input -> parse input |> countUniqueAntinodes antinodesOnce |> Num.toStr |> Ok

antinodesRecurring : Bounds -> Resonance
antinodesRecurring = \bounds ->
    \((ax, ay), (bx, by)) ->
        deltaA = (ax - bx, ay - by)
        deltaB = (bx - ax, by - ay)

        resonateDirection = \acc, (x, y), (xDelta, yDelta) ->
            next = (x - xDelta, y - yDelta)
            if (inBounds bounds) next then
                List.append acc next
                |> resonateDirection next (xDelta, yDelta)
            else
                acc

        resonateDirection [] (ax, ay) deltaA
        |> List.concat (resonateDirection [] (bx, by) deltaB)

part2 : Str -> Result Str _
part2 = \input -> parse input |> countUniqueAntinodes antinodesRecurring |> Num.toStr |> Ok

exampleData =
    """
    ............
    ........0...
    .....0......
    .......0....
    ....0.......
    ......A.....
    ............
    ............
    ........A...
    .........A..
    ............
    ............
    """

expect
    actual = part1 exampleData
    actual == Ok "14"

expect
    actual = part1 inputData
    actual == Ok answers.day08.part1

expect
    actual = part2 exampleData
    actual == Ok "34"

expect
    actual = part2 inputData
    actual == Ok answers.day08.part2
