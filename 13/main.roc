app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    util: "../util/util.roc",
    answers: "../answers/answers.roc",
}
import "./input.txt" as inputData : Str
import answers.A exposing [answers]
import util.StrUtil
import util.ListUtil

Pair : { x : I64, y : I64 }

Machine : {
    a : Pair,
    b : Pair,
    prize : Pair,
}

solve : List Machine, I64 -> I64
solve = \machines, add ->
    List.keepOks machines \m -> solveMachine m add
    |> ListUtil.sumBy \{ m, n } -> 3 * m + n

solveMachine : Machine, I64 -> Result { m : I64, n : I64 } [NoSolution]
solveMachine = \{ a, b, prize }, add ->
    { x: ax, y: ay } = a
    { x: bx, y: by } = b
    px = prize.x + add
    py = prize.y + add

    m = (px * by - bx * py) // (ax * by - bx * ay)
    n = (py - m * ay) // by

    if
        (m * ax + n * bx == px) && (m * ay + n * by == py)
    then
        Ok { m, n }
    else
        Err NoSolution

parse : Str -> Result (List Machine) _
parse = \str ->
    Str.splitOn str "\n\n"
    |> List.mapTry \groupLines ->
        when Str.splitOn groupLines "\n" is
            [aStr, bStr, cStr] ->
                a = try parseLine aStr "+"
                b = try parseLine bStr "+"
                prize = try parseLine cStr "="

                Ok { a, b, prize }

            _ -> Err Invalid

parseLine = \line, op ->
    (_, xyStr) = try StrUtil.splitTwo line ": "
    (xStr, yStr) = try StrUtil.splitTwo xyStr ", "
    x = Str.dropPrefix xStr "X" |> Str.dropPrefix op |> try Str.toI64
    y = Str.dropPrefix yStr "Y" |> Str.dropPrefix op |> try Str.toI64

    Ok { x, y }

part1 = \input -> try parse input |> \in -> solve in 0 |> Num.toStr |> Ok
part2 = \input -> try parse input |> \in -> solve in 10000000000000 |> Num.toStr |> Ok

exampleData =
    """
    Button A: X+94, Y+34
    Button B: X+22, Y+67
    Prize: X=8400, Y=5400

    Button A: X+26, Y+66
    Button B: X+67, Y+21
    Prize: X=12748, Y=12176

    Button A: X+17, Y+86
    Button B: X+84, Y+37
    Prize: X=7870, Y=6450

    Button A: X+69, Y+23
    Button B: X+27, Y+71
    Prize: X=18641, Y=10279
    """

expect
    actual = part1 exampleData
    actual == Ok "480"

expect
    actual = part1 inputData
    actual == Ok answers.day13.part1

expect
    actual = part2 inputData
    actual == Ok answers.day13.part2
