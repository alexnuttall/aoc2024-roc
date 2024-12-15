app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    util: "../util/util.roc",
    answers: "../answers/answers.roc",
}
import "./input.txt" as inputData : Str
import answers.A exposing [answers]

# parse : Str -> Result _ _
parse = \str -> str

part1 = \input -> Ok input
part2 = \input -> Ok input

example_data =
    """
    """

# expect
#     actual = part1 exampleData
#     actual == Ok "3749"

# expect
#     actual = part1 inputData
#     actual == Ok answers.day07.part1

# expect
#     actual = part2 exampleData
#     actual == Ok "11387"

# expect
#     actual = part2 inputData
#     actual == Ok answers.day07.part2
