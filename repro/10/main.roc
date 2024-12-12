app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}

import pf.Stdout
import "./input.txt" as inputData : Str

main =
    Stdout.line! (part1 inputData)
    Stdout.line! (part2 inputData)

parse = \str ->
    Str.toUtf8 str
    |> List.splitOn '\n'
    |> List.walkWithIndex (Dict.empty {}) \dict, row, y ->
        List.walkWithIndex row dict \rowDict, cell, x ->
            Dict.insert rowDict (Num.toI16 x, Num.toI16 y) (cell - '0')

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
        |> List.joinMap \pos -> searchGoals map pos (fromHeight + 1)

findStartingPoints = \map ->
    Dict.keepIf map \(_, h) -> h == 0
    |> Dict.keys

solve1 = \map ->
    findStartingPoints map
    |> List.map \start ->
        searchGoals map start 0 |> Set.fromList |> Set.len
    |> List.sum

searchPaths = \map, fromPos, fromHeight ->
    if fromHeight == 9 then
        1
    else
        open map fromPos fromHeight
        |> List.map \pos -> searchPaths map pos (fromHeight + 1)
        |> List.sum

solve2 = \map ->
    findStartingPoints map
    |> List.map \start -> searchPaths map start 0
    |> List.sum

part1 = \input -> parse input |> solve1 |> Num.toStr
part2 = \input -> parse input |> solve2 |> Num.toStr
