app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    util: "../util/util.roc",
    answers: "../answers/answers.roc",
}
import "./input.txt" as inputData : Str
import answers.A exposing [answers]
import util.ListUtil

parse = \str ->
    Str.toUtf8 str
    |> List.splitOn '\n'
    |> List.walkWithIndex (Dict.empty {}) \dict, row, y ->
        List.walkWithIndex row (Dict.empty {}) \rowDict, cell, x ->
            Dict.insert rowDict (Num.toI16 x, Num.toI16 y) (cell - '0')
        |> Dict.insertAll dict

neighbours = \(x, y) -> [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]

open = \map, fromPos, fromHeight ->
    neighbours fromPos
    |> List.keepIf \neighbour ->
        Dict.get map neighbour
        |> Result.withDefault 0
        |> Num.subSaturated fromHeight
        |> Bool.isEq 1

searchGoals = \map, fromPos, fromHeight ->
    if fromHeight == 9 then
        [fromPos]
    else
        open map fromPos fromHeight
        |> List.joinMap \pos ->
            searchGoals map pos (fromHeight + 1)

findStartingPoints = \map ->
    Dict.keepIf map \(_, v) -> v == 0
    |> Dict.keys

solve1 = \map ->
    findStartingPoints map
    |> ListUtil.sumBy \start ->
        searchGoals map start 0 |> Set.fromList |> Set.len

searchPaths = \map, fromPos, fromHeight ->
    if fromHeight == 9 then
        1
    else
        open map fromPos fromHeight
        |> ListUtil.sumBy \pos ->
            searchPaths
                map
                pos
                (fromHeight + 1)

solve2 = \map ->
    findStartingPoints map
    |> ListUtil.sumBy \start -> searchPaths map start 0

part1 = \input -> parse input |> solve1 |> Num.toStr |> Ok
part2 = \input -> parse input |> solve2 |> Num.toStr |> Ok

mediumExampleData =
    """
    89010123
    78121874
    87430965
    96549874
    45678903
    32019012
    01329801
    10456732
    """

expect
    actual = part1 mediumExampleData
    actual == Ok "36"

expect
    actual = part1 inputData
    actual == Ok answers.day10.part1

expect
    actual = part2 mediumExampleData
    actual == Ok "81"

expect
    actual = part2 inputData
    actual == Ok answers.day10.part2
