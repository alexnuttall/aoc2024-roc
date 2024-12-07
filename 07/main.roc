app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    util: "../util/util.roc",
    answers: "../answers/answers.roc",
}

import "./input.txt" as inputData : Str
import util.StrUtil
import util.ResultUtil
import util.ListUtil
import answers.A exposing [answers]

main = Task.ok {}

Input : List Equation
Equation : (U64, List U64)

parse : Str -> Result Input _
parse = \str ->
    Str.splitOn str "\n"
    |> List.mapTry \line ->
        (targetStr, numsString) = try StrUtil.splitTwo line ": "

        ResultUtil.toTuple
            (Str.toU64 targetStr)
            (Str.splitOn numsString " " |> List.mapTry Str.toU64)

loop1 : List U64, U64, U64 -> Bool
loop1 = \rem, total, target ->
    when rem is
        [] -> total == target
        _ if total > target -> Bool.false
        [a, .. as rest] ->
            loop1 rest (total + a) target
            || (loop1 rest (total * a) target)

solve1 : List Equation -> U64
solve1 = \equations ->
    ListUtil.sumBy equations \(target, xs) ->
        when xs is
            [first, .. as rest] ->
                if loop1 rest first target then target else 0

            _ -> 0

concat : U64, U64 -> U64
concat = \a, b ->
    "$(Num.toStr a)$(Num.toStr b)"
    |> Str.toU64
    |> Result.withDefault 0

loop2 : List U64, U64, U64 -> Bool
loop2 = \rem, total, target ->
    when rem is
        [] -> total == target
        _ if total > target -> Bool.false
        [a, .. as rest] ->
            loop2 rest (total + a) target
            || (loop2 rest (total * a) target)
            || (loop2 rest (concat total a) target)

solve2 : List Equation -> U64
solve2 = \equations ->
    ListUtil.sumBy equations \(target, xs) ->
        when xs is
            [first, .. as rest] ->
                if loop2 rest first target then target else 0

            _ -> 0

exampleData =
    """
    190: 10 19
    3267: 81 40 27
    83: 17 5
    156: 15 6
    7290: 6 8 6 15
    161011: 16 10 13
    192: 17 8 14
    21037: 9 7 18 13
    292: 11 6 16 20
    """

expect
    actual = parse exampleData |> Result.map solve1
    actual == Ok 3749

expect
    actual = parse inputData |> Result.map solve1
    actual == Ok answers.day07.part1

expect
    actual = parse exampleData |> Result.map solve2
    actual == Ok 11387

expect
    actual = parse inputData |> Result.map solve2
    actual == Ok answers.day07.part2
