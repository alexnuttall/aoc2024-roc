app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    util: "../util/util.roc",
}

import "./input.txt" as inputData : Str

main = Task.ok {}

# parse : Str -> Str
parse = \str -> str

# solve1 : Str -> Str
solve1 = \x -> x

# solve2 : Str -> Str
solve2 = \x -> x

exampleData =
    """

    """

# expect
#     actual = parse exampleData |> solve1
#     actual == Ok 0

# expect
#     actual = parse inputData |> solve1
#     actual == Ok 0

# expect
#     actual = parse exampleData |> solve2
#     actual == Ok 0

# expect
#     actual = parse inputData |> solve2
#     actual == Ok 0
