module [
    toTuple,
]

toTuple : Result a err, Result b err -> Result (a, b) err
toTuple = \a, b -> Result.map2 a b \aOk, bOk -> (aOk, bOk)
