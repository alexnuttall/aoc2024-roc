module [
    sumBy,
    countIn,
    groupBy,
    countBy,
    counts,
]

import Internal exposing [upsertDict, id]

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
