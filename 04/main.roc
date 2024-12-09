app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    util: "../util/util.roc",
    answers: "../answers/answers.roc",
}
import "./input.txt" as inputData : Str
import answers.A exposing [answers]

Grid : List (List U8)
GridDict : Dict (I32, I32) U8
Word : List U8

parse : Str -> Grid
parse = \str -> Str.toUtf8 str |> List.splitOn '\n'

directions : List (I32, I32)
directions = [(0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1)]

getWords : GridDict, I32, I32 -> List Word
getWords = \dict, x, y ->
    List.keepOks directions \(dx, dy) ->
        List.range { start: At 1, end: Length 3 }
        |> List.mapTry \n ->
            Dict.get dict (x + (n * dx), y + (n * dy))

toDict : List (List U8) -> GridDict
toDict = \grid ->
    List.walkWithIndex grid (Dict.empty {}) \dict, row, y ->
        List.walkWithIndex row (Dict.empty {}) \rowDict, cell, x ->
            Dict.insert rowDict (Num.toI32 x, Num.toI32 y) cell
        |> Dict.insertAll dict

solve1 : Grid -> U64
solve1 = \grid ->
    dict = toDict grid
    List.walkWithIndex grid 0 \count, row, y ->
        List.walkWithIndex row 0 \rowCount, cell, x ->
            if cell == 'X' then
                getWords dict (Num.toI32 x) (Num.toI32 y)
                |> List.countIf \word -> word == ['M', 'A', 'S']
                |> Num.add rowCount
            else
                rowCount
        |> Num.add count

getXWords : GridDict, I32, I32 -> Result (List U8) _
getXWords = \dict, x, y ->
    [(-1, -1), (1, -1), (1, 1), (-1, 1)]
    |> List.mapTry \(dx, dy) ->
        Dict.get dict (x + dx, y + dy)

isXmas : List U8 -> [Xmas, NotXmas]
isXmas = \xWords ->
    if
        xWords
        == ['M', 'M', 'S', 'S']
        || xWords
        == ['S', 'S', 'M', 'M']
        || xWords
        == ['S', 'M', 'M', 'S']
        || xWords
        == ['M', 'S', 'S', 'M']
    then
        Xmas
    else
        NotXmas

solve2 : Grid -> U64
solve2 = \grid ->
    dict = toDict grid

    List.walkWithIndex grid 0 \count, row, y ->
        List.walkWithIndex row 0 \rowCount, cell, x ->
            if cell == 'A' then
                getXWords dict (Num.toI32 x) (Num.toI32 y)
                |> \words ->
                    when Result.map words isXmas is
                        Ok Xmas -> rowCount + 1
                        _ -> rowCount
            else
                rowCount
        |> Num.add count

part1 = \input -> parse input |> solve1 |> Num.toStr |> Ok
part2 = \input -> parse input |> solve2 |> Num.toStr |> Ok

exampleData =
    """
    MMMSXXMASM
    MSAMXMSMSA
    AMXSXMAAMM
    MSAMASMSMX
    XMASAMXAMM
    XXAMMXXAMA
    SMSMSASXSS
    SAXAMASAAA
    MAMMMXMMMM
    MXMXAXMASX
    """

expect
    actual = part1 exampleData
    actual == Ok "18"

expect
    actual = part1 inputData
    actual == Ok answers.day04.part1

expect
    actual = part2 exampleData
    actual == Ok "9"

expect
    actual = part2 inputData
    actual == Ok answers.day04.part2
