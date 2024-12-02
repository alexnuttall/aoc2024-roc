module [
    upsertDict,
    id,
    unwrap,
]

upsertDict : Dict a b, a, (b -> b), b -> Dict a b where a implements Hash & Eq
upsertDict = \dict, key, update, default ->
    Dict.update dict key \k ->
        when k is
            Ok present -> Ok (update present)
            Err Missing -> Ok default

id : a -> a
id = \x -> x

unwrap : Result a err -> a
unwrap = \result ->
    when result is
        Ok ok -> ok
        Err _ -> crash "unreachable"
