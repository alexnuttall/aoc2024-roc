app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    util: "../util/util.roc",
    answers: "../answers/answers.roc",
}

import "./input.txt" as inputData : Str
import answers.A exposing [answers]
import util.ListUtil

Block : [Free, Data U64]

parse : Str -> List U8
parse = Str.toUtf8

blockList : Block, Int a -> List Block
blockList = \block, count -> List.repeat block (Num.toU64 count)

append : List Block, Block, U64 -> List Block
append = \list, block, count -> List.concat list (blockList block count)

toBlocks : List U8 -> List Block
toBlocks = \input ->
    List.walkWithIndex
        input
        (List.len input * 9 |> List.withCapacity)
        \blocks, sizeCh, i ->
            size = sizeCh - '0' |> Num.toU64
            when i % 2 is
                0 -> append blocks (Data (i // 2)) size
                _ -> append blocks Free size

findFreeIndex : List Block, U64, U64 -> Result U64 [NotFound]
findFreeIndex = \blocks, start, end ->
    List.walkFromUntil
        blocks
        start
        { i: start, freeIndex: Err NotFound }
        \state, block ->
            if state.i > end then
                Break state
            else
                when block is
                    Data _ -> Continue { state & i: state.i + 1 }
                    Free -> Break { state & freeIndex: Ok state.i }
    |> .freeIndex

chompFinalBlock : List Block, U64 -> Result { id : U64, endIndex : U64 } [Empty]
chompFinalBlock = \blocks, from ->
    backwards = \i ->
        when List.get blocks i is
            Ok (Data id) -> Ok { id, endIndex: i - 1 }
            Ok Free -> backwards (i - 1)
            Err _ -> Err Empty

    backwards from

slice : List Block, U64 -> List Block
slice = \blocks, end -> List.sublist blocks { start: 0, len: end + 1 }

compactHelper : List Block, U64, U64 -> List Block
compactHelper = \blocks, start, end ->
    when chompFinalBlock blocks end is
        Err Empty -> blocks
        Ok { id, endIndex: newEnd } ->
            when findFreeIndex blocks start newEnd is
                Err NotFound -> slice blocks end
                Ok freeIndex ->
                    List.set blocks freeIndex (Data id)
                    |> compactHelper (freeIndex + 1) newEnd

compact : List Block -> List Block
compact = \blocks -> compactHelper blocks 0 (List.len blocks - 1)

checksum : List Block -> U64
checksum = \blocks ->
    List.mapWithIndex blocks \block, i ->
        when block is
            Data id -> id * i
            Free -> 0
    |> List.sum

solve1 : List U8 -> U64
solve1 = \input -> toBlocks input |> compact |> checksum

part1 : Str -> Result Str _
part1 = \input -> parse input |> solve1 |> Num.toStr |> Ok

File : { id : U64, size : U8 }
FileIndex : [Remaining File, Deleted U8]
Gap : { size : U8, insertions : List File }
DiskSequence : { files : List FileIndex, gaps : List Gap }

toDiskSequence : List U8 -> DiskSequence
toDiskSequence = \input ->
    ListUtil.splitAlternating input
    |> \(files, gaps) -> {
        files: List.mapWithIndex files \sizeCh, id -> Remaining { size: sizeCh - '0', id },
        gaps: List.map gaps \sizeCh -> { size: sizeCh - '0', insertions: [] },
    }

findGapCandidate : List FileIndex, U8, U64 -> Result File [NoCandidate]
findGapCandidate = \files, size, minIndex ->
    List.walkBackwardsUntil files { index: List.len files - 1, candidate: Err NoCandidate } \state, fileIndex ->
        if state.index < minIndex then
            Break state
        else
            when fileIndex is
                Remaining file if file.size <= size -> Break { state & candidate: Ok file }
                _ -> Continue { state & index: state.index - 1 }
    |> .candidate

move : Gap, List FileIndex, File -> { filled : Gap, files : List FileIndex }
move = \gap, files, file -> {
    files: List.set files file.id (Deleted file.size),
    filled: {
        size: gap.size - file.size,
        insertions: List.append gap.insertions file,
    },
}

fillGap : Gap, U64, List FileIndex -> { filled : Gap, files : List FileIndex }
fillGap = \gap, index, files ->
    when findGapCandidate files gap.size index is
        Ok file ->
            moved = move gap files file
            if moved.filled.size == 0 then
                moved
            else
                fillGap moved.filled index moved.files

        Err NoCandidate -> { filled: gap, files: files }

compact2 : DiskSequence -> DiskSequence
compact2 = \{ gaps, files } ->
    List.walkWithIndex gaps { gaps, files } \state, gap, index ->
        { filled, files: nextFiles } = fillGap gap index state.files

        {
            gaps: List.set state.gaps index filled,
            files: nextFiles,
        }

diskSequenceToBlocks : DiskSequence -> List Block
diskSequenceToBlocks = \{ gaps, files } ->
    List.map2
        files
        (List.append gaps { size: 0, insertions: [] })
        \fileIndex, gap ->
            remaining =
                when fileIndex is
                    Deleted size -> blockList Free size
                    Remaining file -> blockList (Data file.id) file.size

            inserted = List.joinMap gap.insertions \file ->
                blockList (Data file.id) file.size

            gapBlocks = blockList Free gap.size

            remaining |> List.concat inserted |> List.concat gapBlocks
    |> List.join

solve2 = \input ->
    toDiskSequence input
    |> compact2
    |> diskSequenceToBlocks
    |> checksum

part2 : Str -> Result Str _
part2 = \input -> parse input |> solve2 |> Num.toStr |> Ok

exampleData = "2333133121414131402"

expect
    actual = part1 exampleData
    actual == Ok "1928"

expect
    actual = part1 inputData
    actual == Ok answers.day09.part1

expect
    actual = part2 exampleData
    actual == Ok "2858"

expect
    actual = part2 inputData
    actual == Ok answers.day09.part2
