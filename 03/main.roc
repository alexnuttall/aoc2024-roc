app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    util: "../util/util.roc",
    answers: "../answers/answers.roc",
}

import "./input.txt" as inputData : Str
import util.ListUtil
import util.ResultUtil
import answers.A exposing [answers]

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
solve1 = \input -> parseMuls input |> ListUtil.sumBy \(a, b) -> a * b

stripInactive : Str -> Str
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
solve2 = \input -> stripInactive input |> solve1

part1 = \input -> solve1 input |> Num.toStr |> Ok
part2 = \input -> solve2 input |> Num.toStr |> Ok

exampleData = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

expect
    actual = part1 exampleData
    actual == Ok "161"

expect
    actual = part1 inputData
    actual == Ok answers.day03.part1

exampleData2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

expect
    actual = part2 exampleData2
    actual == Ok "48"

expect
    actual = part2 inputData
    actual == Ok answers.day03.part2
