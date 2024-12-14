app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    util: "../util/util.roc",
    answers: "../answers/answers.roc",
}
import "./input.txt" as inputData : Str
import util.ListUtil
import answers.A exposing [answers]

solve1 : List (List U64) -> U64
solve1 = \input -> List.countIf input isValid

solve2 : List (List U64) -> U64
solve2 = \input ->
    List.countIf input \report ->
        List.mapWithIndex report \_, i ->
            List.dropAt report i
        |> List.prepend report
        |> List.any isValid

isValid : List U64 -> Bool
isValid = \report ->
    ListUtil.pairwise report
    |> List.mapTry classifyJump
    |> Result.try checkDirectionality
    |> Result.isOk

classifyJump : (U64, U64) -> Result [LT, GT, EQ] [Invalid]
classifyJump = \(a, b) ->
    when Num.absDiff a b is
        1 | 2 | 3 -> Ok (Num.compare a b)
        _ -> Err Invalid

checkDirectionality : List [LT, GT, EQ] -> Result [Valid] [Invalid]
checkDirectionality = \directions ->
    when directions |> Set.fromList |> Set.len is
        1 -> Ok Valid
        _ -> Err Invalid

parse : Str -> List (List U64)
parse = \input ->
    Str.splitOn input "\n"
    |> List.map \line ->
        Str.splitOn line " "
        |> List.keepOks Str.toU64

part1 = \input -> parse input |> solve1 |> Num.toStr |> Ok
part2 = \input -> parse input |> solve2 |> Num.toStr |> Ok

exampleData =
    """
    7 6 4 2 1
    1 2 7 8 9
    9 7 6 2 1
    1 3 2 4 5
    8 6 4 4 1
    1 3 6 7 9
    """

expect
    actual = part1 exampleData
    actual == Ok "2"

expect
    actual = part1 inputData
    actual == Ok answers.day02.part1

expect
    actual = part2 exampleData
    actual == Ok "4"

expect
    actual = part2 inputData
    actual == Ok answers.day02.part2
