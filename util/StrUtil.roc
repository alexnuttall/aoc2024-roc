module [
    splitTwo,
]

splitTwo : Str, Str -> Result (Str, Str) [CouldNotSplit Str]
splitTwo = \str, on ->
    when Str.splitOn str on is
        [a, b] -> Ok (a, b)
        _ -> Err (CouldNotSplit str)