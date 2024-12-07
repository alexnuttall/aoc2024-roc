app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    util: "../util/util.roc",
    answers: "../answers/answers.roc",
}

import "./input.txt" as inputData : Str
import util.StrUtil
import util.ListUtil
import util.ResultUtil
import answers.A exposing [answers]

main = Task.ok {}

Input : (List U64, List U64)

parse : Str -> Result Input _
parse = \input ->
    Str.splitOn input "\n"
    |> List.walkTry ([], []) \(ls, rs), line ->
        (lStr, rStr) = try StrUtil.splitTwo line "   "
        (l, r) = try ResultUtil.toTuple (Str.toU64 lStr) (Str.toU64 rStr)
        (List.append ls l, List.append rs r) |> Ok

solve1 : Input -> U64
solve1 = \(ls, rs) ->
    List.map2 (List.sortAsc ls) (List.sortAsc rs) Num.absDiff
    |> List.sum

solve2 : Input -> U64
solve2 = \(ls, rs) -> ListUtil.sumBy ls \l -> l * ListUtil.countIn rs l

exampleData =
    """
    3   4
    4   3
    2   5
    1   3
    3   9
    3   3
    """

expect
    actual = parse exampleData |> Result.map solve1
    actual == Ok 11

expect
    actual = parse inputData |> Result.map solve1
    actual == Ok answers.day01.part1

expect
    actual = parse exampleData |> Result.map solve2
    actual == Ok 31

expect
    actual = parse inputData |> Result.map solve2
    actual == Ok answers.day01.part2
