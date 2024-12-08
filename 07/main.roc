app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    util: "../util/util.roc",
    answers: "../answers/answers.roc",
}

import "./input.txt" as inputData : Str
import util.StrUtil
import util.ResultUtil
import util.ListUtil
import answers.A exposing [answers]

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
        [] ->
            total == target

        _ if total > target ->
            Bool.false

        [a, .. as next] ->
            when {} is
                _ if loop1 next (total + a) target -> Bool.true
                _ if loop1 next (total * a) target -> Bool.true
                _ -> Bool.false

part1 : Str -> Result Str _
part1 = \input ->
    parse input
    |> try
    |> ListUtil.sumBy \(target, xs) ->
        when xs is
            [first, .. as rest] ->
                if loop1 rest first target then target else 0

            _ -> 0
    |> Num.toStr
    |> Ok

concat : U64, U64 -> U64
concat = \a, b ->
    getMulti = \multiplier, rem ->
        nextRem = Num.divTrunc rem 10
        if nextRem == 0 then multiplier else getMulti (multiplier * 10) nextRem

    getMulti 10 b |> Num.mul a |> Num.add b

loop2 : List U64, U64, U64 -> Bool
loop2 = \rem, total, target ->
    when rem is
        [] ->
            total == target

        _ if total > target ->
            Bool.false

        [a, .. as next] ->
            when {} is
                _ if loop2 next (total + a) target -> Bool.true
                _ if loop2 next (total * a) target -> Bool.true
                _ if loop2 next (concat total a) target -> Bool.true
                _ -> Bool.false

part2 : Str -> Result Str _
part2 = \input ->
    parse input
    |> try
    |> ListUtil.sumBy \(target, xs) ->
        when xs is
            [first, .. as rest] ->
                if loop2 rest first target then target else 0

            _ -> 0
    |> Num.toStr
    |> Ok

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
    actual = part1 exampleData
    actual == Ok "3749"

expect
    actual = part1 inputData
    actual == Ok answers.day07.part1

expect
    actual = part2 exampleData
    actual == Ok "11387"

expect
    actual = part2 inputData
    actual == Ok answers.day07.part2
