app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    util: "../util/util.roc",
    answers: "../answers/answers.roc",
}
import "./input.txt" as inputData : Str
import util.StrUtil
import util.ListUtil
import util.ResultUtil
import answers.A exposing [answers]

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

part1 = \input -> parse input |> Result.map solve1 |> Result.map Num.toStr
part2 = \input -> parse input |> Result.map solve2 |> Result.map Num.toStr

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
    actual = part1 exampleData
    actual == Ok "11"

expect
    actual = part1 inputData
    actual == Ok answers.day01.part1

expect
    actual = part2 exampleData
    actual == Ok "31"

expect
    actual = part2 inputData
    actual == Ok answers.day01.part2
