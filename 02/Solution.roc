module [parse, solve1, solve2]

import util.ListUtil

Input : List (List U64)

parse : Str -> Input
parse = \input ->
    Str.splitOn input "\n"
    |> List.map \line ->
        Str.splitOn line " "
        |> List.keepOks Str.toU64

isValid : List U64 -> Bool
isValid = \report ->
    ListUtil.pairwise report
    |> List.mapTry \(a, b) ->
        when Num.absDiff a b is
            1 | 2 | 3 -> Ok (Num.compare a b)
            _ -> Err Invalid
    |> Result.try \comparisons ->
        if comparisons |> Set.fromList |> Set.len == 1 then
            Ok Valid
        else
            Err Invalid
    |> Result.isOk

solve1 : Input -> U64
solve1 = \input -> List.countIf input isValid

permutations : List U64 -> List (List U64)
permutations = \report ->
    List.mapWithIndex report \_, i ->
        List.dropAt report i
    |> List.append report

solve2 : Input -> U64
solve2 = \input ->
    List.countIf input \report ->
        permutations report
        |> List.any isValid
