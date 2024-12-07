app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    util: "../util/util.roc",
    answers: "../answers/answers.roc",
}

import "./input.txt" as inputData : Str
import util.ListUtil
import answers.A exposing [answers]

main = Task.ok {}

parse : Str -> List (List U64)
parse = \input ->
    Str.splitOn input "\n"
    |> List.map \line ->
        Str.splitOn line " "
        |> List.keepOks Str.toU64

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

isValid : List U64 -> Bool
isValid = \report ->
    ListUtil.pairwise report
    |> List.mapTry classifyJump
    |> Result.try checkDirectionality
    |> Result.isOk

solve1 : List (List U64) -> U64
solve1 = \input -> List.countIf input isValid

solve2 : List (List U64) -> U64
solve2 = \input ->
    List.countIf input \report ->
        List.mapWithIndex report \_, i ->
            List.dropAt report i
        |> List.prepend report
        |> List.any isValid

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
    actual = parse exampleData |> solve1
    actual == 2

expect
    actual = parse inputData |> solve1
    actual == answers.day02.part1

expect
    actual = parse exampleData |> solve2
    actual == 4

expect
    actual = parse inputData |> solve2
    actual == answers.day02.part2
