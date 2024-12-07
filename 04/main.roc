app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    util: "../util/util.roc",
    answers: "../answers/answers.roc",
}

import "./input.txt" as inputData : List U8
import util.ResultUtil
import answers.A exposing [answers]

main = Task.ok {}

Grid : List (List U8)
GridDict : Dict (U64, U64) U8
WordCoords : List (U64, U64)
Word : List U8

parse : List U8 -> Grid
parse = \bytes -> List.splitOn bytes '\n'

addIndex : U64, I64 -> Result U64 [Overflow]
addIndex = \i, n -> Num.toI64 i |> Num.addChecked n |> Result.map Num.toU64

directions : List (I64, I64)
directions = [(0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1)]

getSearchCoordinates : U64, U64 -> List WordCoords
getSearchCoordinates = \x, y ->
    List.keepOks directions \(dx, dy) ->
        List.range { start: At 1, end: Length 3 }
        |> List.mapTry \n ->
            ResultUtil.toTuple (addIndex x (n * dx)) (addIndex y (n * dy))

readWord : GridDict, WordCoords -> Result Word _
readWord = \dict, coords ->
    List.mapTry coords \pos -> Dict.get dict pos

getWords : GridDict, U64, U64 -> List Word
getWords = \dict, x, y ->
    getSearchCoordinates x y
    |> List.keepOks \directionCoords ->
        readWord dict directionCoords

toDict : List (List U8) -> GridDict
toDict = \grid ->
    List.walkWithIndex grid (Dict.empty {}) \dict, row, y ->
        List.walkWithIndex row (Dict.empty {}) \rowDict, cell, x ->
            Dict.insert rowDict (x, y) cell
        |> Dict.insertAll dict

solve1 : Grid -> U64
solve1 = \grid ->
    dict = toDict grid

    List.walkWithIndex grid 0 \count, row, y ->
        List.walkWithIndex row 0 \rowCount, cell, x ->
            if cell == 'X' then
                getWords dict x y
                |> List.countIf \word -> word == ['M', 'A', 'S']
                |> Num.add rowCount
            else
                rowCount
        |> Num.add count

getXCoords : U64, U64 -> Result (List (U64, U64)) [Overflow]
getXCoords = \x, y ->
    [(-1, -1), (1, -1), (1, 1), (-1, 1)]
    |> List.mapTry \(dx, dy) ->
        ResultUtil.toTuple (addIndex x dx) (addIndex y dy)

getXWords : GridDict, U64, U64 -> Result (List U8) _
getXWords = \dict, x, y ->
    try getXCoords x y
    |> List.mapTry \pos ->
        Dict.get dict pos

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
                getXWords dict x y
                |> \words ->
                    when Result.map words isXmas is
                        Ok Xmas -> rowCount + 1
                        _ -> rowCount
            else
                rowCount
        |> Num.add count

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
    |> Str.toUtf8

expect
    actual = parse exampleData |> solve1
    actual == 18

expect
    actual = parse inputData |> solve1
    actual == answers.day04.part1

expect
    actual = parse exampleData |> solve2
    actual == 9

expect
    actual = parse inputData |> solve2
    actual == answers.day04.part2
