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
moveSeconds = \robot, seconds, size ->
    vec = { x: robot.v.x * seconds, y: robot.v.y * seconds }
    projected = { x: robot.p.x + vec.x, y: robot.p.y + vec.y }
    { x: wrap projected.x size.w, y: wrap projected.y size.h }

wrap : I64, I64 -> I64
wrap = \n, limit ->
    under = n < 0
    over = n >= limit

    if !under && !over then
        n
    else
        mod =
            when n % limit is
                0 -> -limit
                nonZero -> nonZero
        if under then
            limit + mod
        else
            mod

safetyScore : List Coord, GridSize -> U64
safetyScore = \robots, size ->
    xMid = size.w // 2
    yMid = size.h // 2

    List.walk robots { se: 0, ne: 0, sw: 0, nw: 0 } \state, { x, y } ->
        if x > xMid && y > yMid then
            { state & se: state.se + 1 }
        else if x > xMid && y < yMid then
            { state & ne: state.ne + 1 }
        else if x < xMid && y > yMid then
            { state & sw: state.sw + 1 }
        else if x < xMid && y < yMid then
            { state & nw: state.nw + 1 }
        else
            state

    |> \{ se, ne, sw, nw } -> se * ne * sw * nw

solve2 : List Robot -> I64
solve2 = \robots ->
    rCount = List.len robots |> Num.toFrac
    loop = \iter ->
        rs = List.map robots \robot -> moveSeconds robot iter inputSize
        if isImage rs rCount 0.7 then
            dbg (print rs)
            iter
        else
            loop (iter + 1)
    loop 1

isImage : List Coord, Frac a, Frac a -> Bool
isImage = \robots, rCount, ratio ->
    List.walk robots 0 \count, robot ->
        if
            (
                neighbours robot
                |> List.keepIf \n -> List.contains robots n
                |> List.len
                |> Num.isGt 0
            )
        then
            count + 1
        else
            count
    |> Num.toFrac
    |> Num.div rCount
    |> Num.isGt ratio

print : List Coord -> Str
print = \robots ->
    xRange = List.range { start: At 0, end: At inputSize.w }
    yRange = List.range { start: At 0, end: At inputSize.h }

    List.mapWithIndex yRange \_, y ->
        List.map xRange \x ->
            if
                List.contains robots { x: Num.toI64 x, y: Num.toI64 y }
            then
                '#'
            else
                '.'
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
