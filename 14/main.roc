app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    util: "../util/util.roc",
    answers: "../answers/answers.roc",
}
import "./input.txt" as inputData : Str
import answers.A exposing [answers]

Coord : { x : I64, y : I64 }
Robot : { p : Coord, v : Coord }
GridSize : { h : I64, w : I64 }

solve1 : List Robot, GridSize -> U64
solve1 = \robots, size ->
    List.map robots \robot -> moveSeconds robot 100 size
    |> safetyScore size

moveSeconds : Robot, I64, GridSize -> Coord
moveSeconds = \robot, seconds, size -> {
    x: (robot.p.x + seconds * robot.v.x) |> mod size.w,
    y: (robot.p.y + seconds * robot.v.y) |> mod size.h,
}

mod = \n, div ->
    when n % div is
        a if a < 0 -> a + div
        a -> a

safetyScore : List Coord, GridSize -> U64
safetyScore = \robots, size ->
    xm = size.w // 2
    ym = size.h // 2

    List.walk robots { se: 0, ne: 0, sw: 0, nw: 0 } \state, { x, y } ->
        if x > xm && y > ym then
            { state & se: state.se + 1 }
        else if x > xm && y < ym then
            { state & ne: state.ne + 1 }
        else if x < xm && y > ym then
            { state & sw: state.sw + 1 }
        else if x < xm && y < ym then
            { state & nw: state.nw + 1 }
        else
            state
    |> \{ se, ne, sw, nw } -> se * ne * sw * nw

solve2 : List Robot -> I64
solve2 = \robots ->
    set = Set.fromList robots
    rCount = Set.len set |> Num.toFrac
    loop = \iter ->
        rs = Set.map set \robot -> moveSeconds robot iter inputSize
        if isImage rs rCount 0.7 then
            dbg (print rs)
            iter
        else
            loop (iter + 1)
    loop 1

isImage : Set Coord, Frac a, Frac a -> Bool
isImage = \robots, rCount, ratio ->
    Set.walk robots 0 \count, robot ->
        hasNeighbour =
            neighbours robot
            |> List.keepIf \n -> Set.contains robots n
            |> List.len
            |> Num.isGt 0
        if hasNeighbour then count + 1 else count
    |> Num.toFrac
    |> Num.div rCount
    |> Num.isGt ratio

print : Set Coord -> Str
print = \robots ->
    xRange = List.range { start: At 0i64, end: At inputSize.w }
    yRange = List.range { start: At 0i64, end: At inputSize.h }

    List.map yRange \y ->
        List.map xRange \x ->
            if Set.contains robots { x, y } then '#' else '.'
        |> Str.fromUtf8
        |> Result.withDefault ""
    |> Str.joinWith "\n"

neighbours = \{ x, y } -> [
    { x, y: y - 1 },
    { x: x + 1, y: y - 1 },
    { x: x + 1, y },
    { x: x + 1, y: y + 1 },
    { x, y: y + 1 },
    { x: x - 1, y: y + 1 },
    { x: x - 1, y },
    { x: x - 1, y: y - 1 },
]

parse : Str -> Result (List Robot) _
parse = \str ->
    Str.splitOn str "\n"
    |> List.mapTry \line ->
        { before: pStr, after: vStr } = try Str.splitFirst line " "
        p = try parseXy pStr
        v = try parseXy vStr
        Ok { p, v }

parseXy = \str ->
    { after: xyStr } = try Str.splitFirst str "="
    { before: xStr, after: yStr } = try Str.splitFirst xyStr ","
    x = try Str.toI64 xStr
    y = try Str.toI64 yStr
    Ok { x, y }

inputSize = { h: 103, w: 101 }

part1 = \input -> try parse input |> \in -> solve1 in inputSize |> Num.toStr |> Ok
part2 = \input -> try parse input |> \in -> solve2 in |> Num.toStr |> Ok

exampleData =
    """
    p=0,4 v=3,-3
    p=6,3 v=-1,-3
    p=10,3 v=-1,2
    p=2,0 v=2,-1
    p=0,0 v=1,3
    p=3,0 v=-2,-2
    p=7,6 v=-1,-3
    p=3,0 v=-1,-2
    p=9,3 v=2,3
    p=7,3 v=-1,2
    p=2,4 v=2,-3
    p=9,5 v=-3,-3
    """

expect
    actual = parse exampleData |> Result.map \data -> solve1 data { h: 7, w: 11 }
    actual == Ok 12

expect
    actual = parse inputData |> Result.map \data -> solve1 data inputSize
    actual == Ok answers.day14.part1

expect
    actual = part2 inputData
    actual == Ok answers.day14.part2
