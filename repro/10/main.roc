app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
}
import pf.Stdout

data2 = "0123456789"

main =
    map = parse data2

    starts = findStartingPoints map
    # this is always [(0, 0)] - even when the bug occurs
    # but hardcoding this stops the bug for some reason

    ends = List.map starts \start ->
        searchForNine map start 0
    # should be [[(9, 0)]] - the location of the 9
    # but when bug occurs, it's [[]]

    Stdout.line! (Inspect.toStr ends)

parse = \str ->
    Str.toUtf8 str
    |> List.walkWithIndex (Dict.empty {}) \dict, cell, x ->
        Dict.insert dict (Num.toI16 x, Num.toI16 0) (cell - '0')

open = \map, (x, y), fromHeight ->
    [(x + 1, y)]
    |> List.keepIf \neighbour ->
        Dict.get map neighbour
        |> Result.withDefault 0
        |> Num.subSaturated fromHeight
        |> Bool.isEq 1

searchForNine = \map, fromPos, fromHeight ->
    if fromHeight == 9 then
        [fromPos]
    else
        open map fromPos fromHeight
        |> List.joinMap \pos -> searchForNine map pos (fromHeight + 1)

findStartingPoints = \map ->
    Dict.keepIf map \(_, h) -> h == 0
    |> Dict.keys

