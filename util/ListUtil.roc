module [
    sumBy,
    countIn,
    groupBy,
    countBy,
    counts,
    partition,
    pairwise,
    pairs,
    joinSets,
    splitAlternating,
]

import Internal exposing [upsertDict, id, unwrap]

sumBy : List a, (a -> Num b) -> Num b
sumBy = \xs, f -> List.walk xs 0 \sum, x -> sum + (f x)

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
        if predicate x then
            (List.append true x, false)
        else
            (true, List.append false x)

pairwise : List a -> List (a, a)
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

pairs : List a -> List (a, a)
pairs = \xs ->
    len = List.len xs
    List.mapWithIndex xs \x, i ->
        sliceLen = len - i - 1
        (x, List.takeLast xs sliceLen)
    |> List.joinMap \(a, bs) ->
        List.map bs \b -> (a, b)

joinSets : List (Set a) -> Set a
joinSets = \xs -> List.walk xs (Set.empty {}) Set.union

splitAlternating : List a -> (List a, List a)
splitAlternating = \xs ->
    init = List.len xs // 2 + 1 |> List.withCapacity
    List.walkWithIndex xs (init, init) \(even, odd), x, i ->
        when i % 2 is
            0 -> (List.append even x, odd)
            _ -> (even, List.append odd x)
