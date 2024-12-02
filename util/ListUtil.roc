module [
    sumBy,
    countIn,
    groupBy,
    countBy,
    counts,
    partition,
    partitionWithIndex,
    slidingWindow,
    pairwise
]

import Internal exposing [upsertDict, id, unwrap]

sumBy : List a, (a -> Num b) -> Num b
sumBy = \xs, f -> List.map xs f |> List.sum

countIn : List a, a -> U64 where a implements Eq
countIn = \xs, y -> List.countIf xs \x -> x == y

groupBy : List a, (a -> b) -> Dict b (List a) where b implements Hash & Eq
groupBy = \xs, f ->
    List.walk xs (Dict.empty {}) \dict, x ->
        upsertDict dict (f x) (\v -> List.append v x) [x]

countBy : List a, (a -> b) -> Dict b (Num c) where b implements Hash & Eq
countBy = \xs, f ->
    List.walk xs (Dict.empty {}) \dict, x -> upsertDict dict (f x) (\n -> n + 1) 1

counts : List a -> Dict a (Num b) where a implements Hash & Eq
counts = \xs -> countBy xs id

partition : List a, (a -> Bool) -> (List a, List a)
partition = \xs, predicate ->
    List.walk xs ([], []) \(true, false), x ->
        if predicate x then (List.append true x, false) else (true, List.append false x)

partitionWithIndex : List a, (a -> Bool) -> (List (U64, a), List (U64, a))
partitionWithIndex = \xs, predicate ->
    List.walkWithIndex xs ([], []) \(true, false), x, i ->
        if predicate x then (List.append true (i, x), false) else (true, List.append false (i, x))

slidingWindow : List a, Int b -> List (List a)
slidingWindow = \xs, windowLen ->
    listLen = List.len xs
    when Num.toU64Checked windowLen is
        Err OutOfBounds -> []
        Ok posWindowLen ->
            when Num.subChecked listLen posWindowLen is
                Err Overflow -> []
                Ok 0 -> []
                Ok len ->
                    List.range { start: At 0, end: Length (len + 1) }
                    |> List.map \start ->
                        List.sublist xs { start, len: posWindowLen }

pairwise = \xs ->
    len = List.len xs

    if len < 2 then
        []
    else
        List.range { start: At 0, end: Length (len - 1) }
        |> List.map \i ->
            (
                List.get xs i |> unwrap,
                List.get xs (i + 1) |> unwrap,
            )

expect
    actual = pairwise [1, 2, 3, 4, 5]
    actual == [(1, 2), (2, 3), (3, 4), (4, 5)]
