app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    util: "../util/util.roc",
    answers: "../answers/answers.roc",
}
import "./input.txt" as inputData : Str
import answers.A exposing [answers]
import util.DictUtil

solve : List U128, U128 -> U128
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

apply : U128 -> List U128
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

split : List U8, U64 -> List U128
split = \ds, len ->
    { before, others } = List.splitAt ds (len // 2)
    [
        revDigitsToNum others,
        revDigitsToNum before,
    ]

toRevDigits : U128 -> List U8
toRevDigits = \n ->
    loop = \acc, rem ->
        nextRem = rem // 10
        if nextRem == 0 then
            List.append acc (Num.toU8 rem)
        else
            List.append acc (rem % 10 |> Num.toU8)
            |> loop nextRem

    loop [] n

revDigitsToNum : List U8 -> U128
revDigitsToNum = \ds ->
    List.walkWithIndex ds 0u128 \sum, d, i ->
        sum + ((Num.toU128 d) * Num.powInt 10 (Num.toU128 i))
    
incDict = \dict, n, x -> DictUtil.upsert dict n (\present -> present + x) x
decDict = \dict, n, x -> DictUtil.upsert dict n (\present -> Num.subSaturated present x) 0

toDict : List U128 -> Dict U128 U128
toDict = \input -> List.walk input (Dict.empty {}) \dict, n -> incDict dict n 1u128

parse = \str -> Str.splitOn str " " |> List.mapTry Str.toU128

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
