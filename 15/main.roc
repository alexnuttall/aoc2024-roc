app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    util: "../util/util.roc",
    answers: "../answers/answers.roc",
}
import "./input.txt" as inputData : Str
import answers.A exposing [answers]

Pos : { x : U16, y : U16 }
Map : Dict Pos [Wall, CrateHead]
Heading : [N, E, S, W]
State : { map : Map, robot : Pos }
Input : { state : State, instructions : List Heading }

############### part1 ###############

solve_1 : Input -> U64
solve_1 = \{ state, instructions } ->
    List.walk instructions state apply_instruction
    |> .map
    |> score_map

apply_instruction : State, Heading -> State
apply_instruction = \state, heading ->
    target_pos = get_target state.robot heading

    when get_cell target_pos state.map is
        Wall -> state
        Void -> { state & robot: target_pos }
        CrateHead -> push_crate state target_pos heading

push_crate : State, Pos, Heading -> State
push_crate = \{ map, robot }, crate_pos, heading ->
    next = get_target crate_pos heading
    when get_crate_chain map next heading is
        Immovable -> { map, robot }
        MovableChain terminus ->
            {
                map: apply_push map crate_pos terminus,
                robot: crate_pos,
            }

get_crate_chain : Map, Pos, Heading -> [MovableChain Pos, Immovable]
get_crate_chain = \map, crate_pos, heading ->
    loop = \pos ->
        when get_cell pos map is
            Void -> MovableChain pos
            Wall -> Immovable
            CrateHead -> get_target pos heading |> loop

    loop crate_pos

apply_push : Map, Pos, Pos -> Map
apply_push = \map, crate_pos, chain_terminus ->
    Dict.remove map crate_pos |> Dict.insert chain_terminus CrateHead

############### part2 ###############

solve_2 : Input -> U64
solve_2 = \{ state, instructions } ->
    List.walk instructions (scale state) apply_instruction_double_crates
    |> .map
    |> score_map

apply_instruction_double_crates : State, Heading -> State
apply_instruction_double_crates = \state, heading ->
    target_pos = get_target state.robot heading
    west_pos = w_pos target_pos

    when (get_cell west_pos state.map, get_cell target_pos state.map) is
        (_, CrateHead) -> push_double_crate state target_pos target_pos heading
        (CrateHead, Void) -> push_double_crate state target_pos west_pos heading
        (_, Wall) -> state
        (_, Void) -> { state & robot: target_pos }
        _ -> crash "unreachable"

push_double_crate : State, Pos, Pos, Heading -> State
push_double_crate = \{ map, robot }, target_pos, crate_head_pos, heading ->
    blocks_to_push =
        when heading is
            N | S -> get_blocks_ns map crate_head_pos heading
            E | W -> get_blocks_ew map crate_head_pos heading

    when blocks_to_push is
        Immovable -> { map, robot }
        MovableBlocks positions ->
            {
                map: apply_push_double map positions heading,
                robot: target_pos,
            }

get_blocks_ew : Map, Pos, Heading -> [MovableBlocks (List Pos), Immovable]
get_blocks_ew = \map, crate_head, heading ->
    loop = \acc, pos ->
        next = get_target pos heading
        after_next = get_target next heading

        when (get_cell next map, get_cell after_next map) is
            (Void, CrateHead) -> loop (List.append acc after_next) after_next
            (Void, Void) -> MovableBlocks acc
            (Void, Wall) if heading == W -> MovableBlocks acc
            _ -> Immovable

    loop [crate_head] crate_head

get_blocks_ns : Map, Pos, Heading -> [MovableBlocks (List Pos), Immovable]
get_blocks_ns = \map, crate_head, heading ->
    loop = \acc, active ->
        when List.mapTry active \pos -> get_next_heads map pos heading is
            Err Wall -> Immovable
            Ok list ->
                when List.join list is
                    [] -> MovableBlocks (List.concat acc active)
                    heads -> loop (List.concat acc active) heads

    loop [] [crate_head]

get_next_heads : Map, Pos, Heading -> Result (List Pos) [Wall]
get_next_heads = \map, pos, heading ->
    mid = get_target pos heading
    e = e_pos mid
    w = w_pos mid

    when (get_cell w map, get_cell mid map, get_cell e map) is
        (_, Wall, _) | (_, _, Wall) -> Err Wall
        (CrateHead, Void, CrateHead) -> Ok [w, e]
        (CrateHead, Void, Void) -> Ok [w]
        (_, Void, CrateHead) -> Ok [e]
        (_, CrateHead, _) -> Ok [mid]
        _ -> Ok []

apply_push_double : Map, List Pos, Heading -> Map
apply_push_double = \map, to_push, heading ->
    removed = List.walk to_push map \state, pos ->
        Dict.remove state pos

    List.walk to_push removed \state, pos ->
        Dict.insert state (get_target pos heading) CrateHead

scale = \{ map, robot } -> { map: scaled_map map, robot: scaled_pos robot }

scaled_map = \map ->
    init_map = Dict.len map |> Dict.withCapacity
    Dict.walk map init_map \state, pos, el ->
        new = scaled_pos pos
        when el is
            Wall ->
                Dict.insert state new Wall
                |> Dict.insert (e_pos new) Wall

            CrateHead -> Dict.insert state new CrateHead

scaled_pos = \{ x, y } -> { x: x * 2, y }

############### shared ###############

score_map : Map -> U64
score_map = \map ->
    Dict.walk map 0 \score, { x, y }, el ->
        when el is
            CrateHead -> score + (Num.toU64 x) + 100 * (Num.toU64 y)
            Wall -> score

get_cell : Pos, Map -> [Wall, CrateHead, Void]
get_cell = \pos, map ->
    when Dict.get map pos is
        Ok Wall -> Wall
        Ok CrateHead -> CrateHead
        Err KeyNotFound -> Void

get_target = \pos, heading ->
    when heading is
        N -> n_pos pos
        E -> e_pos pos
        S -> s_pos pos
        W -> w_pos pos

e_pos = \{ x, y } -> { x: x + 1, y }
n_pos = \{ x, y } -> { x, y: y - 1 }
s_pos = \{ x, y } -> { x, y: y + 1 }
w_pos = \{ x, y } -> { x: x - 1, y }

parse : Str -> Result Input _
parse = \str ->
    { before: mapStr, after: instrStr } = try Str.splitFirst str "\n\n"
    state = try parse_map mapStr
    instructions = parse_instructions instrStr
    Ok { state, instructions }

parse_map = \str ->
    Str.splitOn str "\n"
    |> List.walkWithIndex { map: Dict.empty {}, robot: Err NotFound } \state, line, y ->
        Str.toUtf8 line
        |> List.walkWithIndex state \rowState, cell, x ->
            pos = { x: Num.toU16 x, y: Num.toU16 y }
            when cell is
                '#' -> { rowState & map: Dict.insert rowState.map pos Wall }
                'O' -> { rowState & map: Dict.insert rowState.map pos CrateHead }
                '@' -> { rowState & robot: Ok pos }
                _ -> rowState
    |> \parsed ->
        when parsed is
            { map, robot: Ok pos } -> Ok { map, robot: pos }
            _ -> Err NoRobot

parse_instructions = \str ->
    Str.toUtf8 str
    |> List.keepOks \byte ->
        when byte is
            '<' -> Ok W
            'v' -> Ok S
            '^' -> Ok N
            '>' -> Ok E
            _ -> Err Ignore

part1 = \input -> parse input |> Result.map solve_1 |> Result.map Num.toStr
part2 = \input -> parse input |> Result.map solve_2 |> Result.map Num.toStr

example_data =
    """
    ##########
    #..O..O.O#
    #......O.#
    #.OO..O.O#
    #..O@..O.#
    #O#..O...#
    #O..O..O.#
    #.OO.O.OO#
    #....O...#
    ##########

    <vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
    vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
    ><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
    <<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
    ^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
    ^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
    >^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
    <><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
    ^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
    v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
    """

expect
    actual = part1 example_data
    actual == Ok "10092"

expect
    actual = part1 inputData
    actual == Ok answers.day15.part1

expect
    actual = part2 example_data
    actual == Ok "9021"

expect
    actual = part2 inputData
    actual == Ok answers.day15.part2
