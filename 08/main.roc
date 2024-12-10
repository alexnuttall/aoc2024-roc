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
Pos : { x : I16, y : I16 }
NodeGroups : List (List Pos)
Bounds : { height : I16, width : I16 }
Resonance : { a : Pos, b : Pos } -> List Pos

parse : Str -> Matrix
parse = \str -> Str.toUtf8 str |> List.splitOn '\n'

findNodes : Matrix -> NodeGroups
findNodes = \matrix ->
    List.walkWithIndex matrix (Dict.empty {}) \dict, row, y ->
        List.walkWithIndex row dict \rowDict, cell, x ->
            when cell is
                '.' -> rowDict
                f -> Dict.insert rowDict { x: Num.toI16 x, y: Num.toI16 y } f
    |> DictUtil.invert
    |> Dict.values

getBounds : Matrix -> Bounds
getBounds = \matrix -> {
    height: List.len matrix |> Num.toI16,
    width: List.get matrix 0 |> Result.withDefault [] |> List.len |> Num.toI16,
}

inBounds : Bounds -> (Pos -> Bool)
inBounds = \{ height, width } -> \{ x, y } -> x >= 0 && x < width && y >= 0 && y < height

resonateOnce : Bounds -> Resonance
resonateOnce = \bounds ->
    \{ a, b } ->
        nextA = { x: a.x - (a.x - b.x) * 2, y: a.y - (a.y - b.y) * 2 }
        nextB = { x: b.x - (b.x - a.x) * 2, y: b.y - (b.y - a.y) * 2 }

        [nextA, nextB] |> List.keepIf (inBounds bounds)

countUniqueAntinodes : Matrix, (Bounds -> Resonance) -> U64
countUniqueAntinodes = \matrix, resonateWithinBounds ->
    findNodes matrix
    |> List.map \group ->
        ListUtil.pairs group
        |> List.joinMap \(a, b) ->
            { a, b } |> (getBounds matrix |> resonateWithinBounds)
        |> Set.fromList
    |> ListUtil.joinSets
    |> Set.len

resonateRecurring : Bounds -> Resonance
resonateRecurring = \bounds ->
    \{ a, b } ->
        loop = \acc, { x, y }, delta ->
            next = { x: x - delta.x, y: y - delta.y }

            if (inBounds bounds) next then
                List.append acc next |> loop next delta
            else
                acc

        loop [] a { x: a.x - b.x, y: a.y - b.y }
        |> List.concat (loop [] b { x: b.x - a.x, y: b.y - a.y })

part1 = \input -> parse input |> countUniqueAntinodes resonateOnce |> Num.toStr |> Ok
part2 = \input -> parse input |> countUniqueAntinodes resonateRecurring |> Num.toStr |> Ok

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
