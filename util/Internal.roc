module [
    upsertDict,
    id,
]

upsertDict : Dict a b, a, (b -> b), b -> Dict a b where a implements Hash & Eq
upsertDict = \dict, key, update, default ->
    Dict.update dict key \k ->
        when k is
            Ok present -> Ok (update present)
            Err Missing -> Ok default

id : a -> a
id = \x -> x
