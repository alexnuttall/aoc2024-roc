app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    util: "../util/util.roc",
    answers: "../answers/answers.roc",
}

import "./input.txt" as inputData : Str
import util.ListUtil
import util.ResultUtil
import answers.A exposing [answers]

main = Task.ok {}

parseMuls : Str -> List (U64, U64)
parseMuls = \str ->
    Str.splitOn str "mul("
    |> List.keepOks \segment ->
        when Str.splitFirst segment ")" is
            Ok { before } ->
                when Str.splitOn before "," is
                    [a, b] -> ResultUtil.toTuple (Str.toU64 a) (Str.toU64 b)
                    _ -> Err NonMul

            Err _ -> Err NonMul

solve1 : Str -> U64
solve1 = \input ->
    parseMuls input
    |> ListUtil.sumBy \(a, b) -> a * b

stripInactive = \str ->
    when Str.splitOn str "don't()" is
        [_single] | [] -> str
        [initial, .. as rest] ->
            List.keepOks rest \dontSegment ->
                when Str.splitFirst dontSegment "do()" is
                    Ok { after } -> Ok after
                    _ -> Err Final
            |> Str.joinWith ""
            |> Str.withPrefix initial

solve2 : Str -> U64
solve2 = \input ->
    stripInactive input
    |> solve1

exampleData = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

expect
    actual = solve1 exampleData
    actual == 161

expect
    actual = solve1 inputData
    actual == answers.day03.part1

exampleData2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

expect
    actual = solve2 exampleData2
    actual == 48

expect
    actual = solve2 inputData
    actual == answers.day03.part2
