module [parse, solve1, solve2]

import util.StrUtil
import util.ListUtil
import util.ResultUtil

Input : (List U64, List U64)

parse : Str -> Result Input _
parse = \input ->
    Str.splitOn input "\n"
    |> List.walkTry ([], []) \(ls, rs), line ->
        (lStr, rStr) = try StrUtil.splitTwo line "   "
        (l, r) = try ResultUtil.toTuple (Str.toU64 lStr) (Str.toU64 rStr)
        (List.append ls l, List.append rs r) |> Ok

solve1 : Input -> U64
solve1 = \(ls, rs) ->
    List.map2 (List.sortAsc ls) (List.sortAsc rs) Num.absDiff
    |> List.sum

solve2 : Input -> U64
solve2 = \(ls, rs) -> ListUtil.sumBy ls \l -> l * ListUtil.countIn rs l
