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

Page : U64
Rule : (Page, Page)
Update : List Page
Input : { rules : List Rule, updates : List Update }

parse : Str -> Result Input _
parse = \str ->
    (ruleBlock, updateBlock) = try StrUtil.splitTwo str "\n\n"

    rules =
        Str.splitOn ruleBlock "\n"
        |> try List.mapTry \line ->
            (aStr, bStr) = try StrUtil.splitTwo line "|"
            ResultUtil.toTuple (Str.toU64 aStr) (Str.toU64 bStr)

    updates =
        Str.splitOn updateBlock "\n"
        |> try List.mapTry \line ->
            Str.splitOn line "," |> List.mapTry Str.toU64

    Ok { rules, updates }

validate : Update, List Rule -> Bool
validate = \update, rules ->
    List.all rules \(a, b) ->
        when
            (
                List.findFirstIndex update \u -> u == a,
                List.findFirstIndex update \u -> u == b,
            )
        is
            (Ok aI, Ok bI) if aI < bI -> Bool.true
            (Ok _, Ok _) -> Bool.false
            _ -> Bool.true

mid : Update -> U64
mid = \update ->
    List.len update
    |> Num.divTrunc 2
    |> \m -> List.get update m
    |> Result.withDefault 0

solve1 : Input -> U64
solve1 = \{ updates, rules } ->
    List.keepIf updates \update -> validate update rules
    |> ListUtil.sumBy mid

sort : Update, List Rule -> Update
sort = \update, rules ->
    List.sortWith update \a, b ->
        if List.contains rules (a, b) then
            LT
        else if List.contains rules (b, a) then
            GT
        else
            EQ

solve2 : Input -> U64
solve2 = \{ updates, rules } ->
    List.dropIf updates \update -> validate update rules
    |> List.map \update -> sort update rules
    |> ListUtil.sumBy mid

part1 = \input -> parse input |> Result.map solve1 |> Result.map Num.toStr
part2 = \input -> parse input |> Result.map solve2 |> Result.map Num.toStr

exampleData =
    """
    47|53
    97|13
    97|61
    97|47
    75|29
    61|13
    75|53
    29|13
    97|29
    53|29
    61|53
    97|53
    61|29
    47|13
    75|47
    97|75
    47|61
    75|61
    47|29
    75|13
    53|13

    75,47,61,53,29
    97,61,53,29,13
    75,29,13
    75,97,47,61,53
    61,13,29
    97,13,75,29,47
    """

expect
    actual = part1 exampleData
    actual == Ok "143"

expect
    actual = part1 inputData
    actual == Ok answers.day05.part1

expect
    actual = part2 exampleData
    actual == Ok "123"

expect
    actual = part2 inputData
    actual == Ok answers.day05.part2
