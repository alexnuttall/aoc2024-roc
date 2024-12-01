module [
    upsert,
    invert,
]

import Internal exposing [upsertDict]

upsert : Dict a b, a, (b -> b), b -> Dict a b where a implements Hash & Eq
upsert = upsertDict

invert : Dict a b -> Dict b (List a) where a implements Hash & Eq, b implements Hash & Eq
invert = \dict ->
    Dict.walk dict (Dict.empty {}) \acc, key, value ->
        upsertDict acc value (\keys -> List.append keys key) [key]
