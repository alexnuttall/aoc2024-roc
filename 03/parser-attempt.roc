app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    util: "../util/util.roc",
}

import "./input.txt" as inputData : Str
# import Solution exposing [solve1, solve2]

main = Task.ok {}

exampleData = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

Mul : (U64, U64)
ParseResult a : Result (a, Str) [ParseError]

literal : Str, Str -> ParseResult Str
literal = \str, lit ->
    if
        Str.startsWith str lit
    then
        Ok (lit, Str.dropPrefix str lit)
    else
        Err ParseError

oneOf : List (ParseResult a) -> ParseResult a
oneOf = \parseResults ->
    List.walkUntil parseResults (Err ParseError) \_, p ->
        when p is
            Ok res -> Break (Ok res)
            Err ParseError -> Continue (Err ParseError)

digit = \str ->
    oneOf [
        literal str "0",
        literal str "1",
        literal str "2",
        literal str "3",
        literal str "4",
        literal str "5",
        literal str "6",
        literal str "7",
        literal str "8",
        literal str "9",
    ]

unsignedInt : Str -> ParseResult U64
unsignedInt = \str ->
    accumulate = \s, acc ->
        when digit s is
            Ok (d, rest) -> accumulate rest (List.append acc d)
            Err ParseError ->
                Str.joinWith acc ""
                |> Str.toU64
                |> Result.map \n -> (n, s)
                |> Result.mapErr \_ -> ParseError

    accumulate str []

mul : Str -> ParseResult Mul
mul = \str ->
    (_, aRest) = try literal str "mul("
    (left, bRest) = try unsignedInt aRest
    (_, cRest) = try literal bRest ","
    (right, dRest) = try unsignedInt cRest
    (_, eRest) = try literal dRest ")"

    Ok ((left, right), eRest)

solve1 : Str -> U64
solve1 = \input -> 0

solve2 : Str -> Str
solve2 = \input -> input

expect
    actual = literal "this" "th"
    actual == Ok ("th", "is")

expect
    actual = literal "this" "is"
    actual == Err ParseError

expect
    actual = mul "mul(123,987)"
    actual == Ok ((0, 0), "")
