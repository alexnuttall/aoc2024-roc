app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    util: "../util/util.roc",
    answers: "../answers/answers.roc",
}
import "./input.txt" as inputData : Str
import answers.A exposing [answers]
import util.DictUtil

parse = \str -> Str.splitOn str " " |> List.mapTry Str.toU64

incDict = \dict, n, x -> DictUtil.upsert dict n (\present -> present + x) x
decDict = \dict, n, x -> DictUtil.upsert dict n (\present -> present - x) 0

toDict = \input -> List.walk input (Dict.empty {}) \dict, n -> incDict dict n 1

toRevDigits = \n ->
    loop = \acc, rem ->
        nextRem = rem // 10
        if nextRem == 0 then
            List.append acc rem
        else
            List.append acc (rem % 10)
            |> loop nextRem

    loop [] n

revDigitsToNum = \ds ->
    List.walkWithIndex ds 0 \sum, d, i ->
        sum + (d * Num.powInt 10 i)

split = \ds, len ->
    { before, others } = List.splitAt ds (len // 2)
    [
        revDigitsToNum others,
        revDigitsToNum before,
    ]

apply = \n ->
    if n == 0 then
        [1]
    else
        digits = toRevDigits n
        len = List.len digits

        if len % 2 == 0 then
            split digits len
        else
            [n * 2024]

solve = \input, iterMax ->
    loop = \d, iter ->
        if iter == iterMax then
            d
        else
            Dict.walk d d \next, n, count ->
                apply n
                |> List.walk next \dictState, new -> incDict dictState new count
                |> \dictState -> decDict dictState n count
            |> loop (iter + 1)

    toDict input |> loop 0 |> Dict.values |> List.sum

part1 = \input -> (parse input |> Result.map \in -> solve in 25) |> Result.map Num.toStr
part2 = \input -> (parse input |> Result.map \in -> solve in 75) |> Result.map Num.toStr

exampleData = "125 17"

expect
    actual = part1 exampleData
    actual == Ok "55312"

expect
    actual = part1 inputData
    actual == Ok answers.day11.part1

expect
    actual = part2 exampleData
    actual == Ok "65601038650482"

expect
    actual = part2 inputData
    actual == Ok answers.day11.part2