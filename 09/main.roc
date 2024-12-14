app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    util: "../util/util.roc",
    answers: "../answers/answers.roc",
}
import "./input.txt" as inputData : Str
import answers.A exposing [answers]
import util.ListUtil

Block : [Free, Data U16]

solve1 : List U8 -> U64
solve1 = \input -> toBlocks input |> compact |> checksum

compact : List Block -> List Block
compact = \blocks ->
    loop = \bs, start, end ->
        when chompFinalBlock bs end is
            Err Empty -> bs
            Ok { id, endIndex: newEnd } ->
                when findFreeIndex bs start newEnd is
                    Err NotFound -> List.sublist bs { start: 0, len: end + 1 }
                    Ok i -> List.set bs i (Data id) |> loop (i + 1) newEnd

    loop blocks 0 (List.len blocks - 1)

findFreeIndex : List Block, U64, U64 -> Result U64 [NotFound]
findFreeIndex = \blocks, start, end ->
    init = { i: start, freeIndex: Err NotFound }
    List.walkFromUntil blocks start init \state, block ->
        if state.i > end then
            Break state
        else
            when block is
                Data _ -> Continue { state & i: state.i + 1 }
                Free -> Break { state & freeIndex: Ok state.i }
    |> .freeIndex

chompFinalBlock : List Block, U64 -> Result { id : U16, endIndex : U64 } [Empty]
chompFinalBlock = \blocks, from ->
    backwards = \i ->
        when List.get blocks i is
            Ok (Data id) -> Ok { id, endIndex: i - 1 }
            Ok Free -> backwards (i - 1)
            Err _ -> Err Empty

    backwards from

checksum : List Block -> U64
checksum = \blocks ->
    List.mapWithIndex blocks \block, i ->
        when block is
            Data id -> Num.toU64 id * i
            Free -> 0
    |> List.sum

toBlocks : List U8 -> List Block
toBlocks = \input ->
    init = List.len input * 9 |> List.withCapacity
    List.walkWithIndex input init \blocks, sizeCh, i ->
        size = sizeCh - '0'
        when i % 2 is
            0 -> append blocks (Data ((Num.toU16 i) // 2)) size
            _ -> append blocks Free size

blockList : Block, Int a -> List Block
blockList = \block, count -> List.repeat block (Num.toU64 count)

append : List Block, Block, U8 -> List Block
append = \list, block, count -> List.concat list (blockList block count)

File : { id : U16, size : U8 }
FileIndex : [Remaining File, Deleted U8]
Gap : { size : U8, insertions : List File }
DiskSequence : { files : List FileIndex, gaps : List Gap }

solve2 : List U8 -> U64
solve2 = \input -> toDiskSequence input |> compactFiles |> diskSequenceToBlocks |> checksum

compactFiles : DiskSequence -> DiskSequence
compactFiles = \{ gaps, files } ->
    List.walkWithIndex gaps { gaps, files } \state, gap, index ->
        { gap: filled, files: nextFiles } = fillGap gap index state.files
        {
            gaps: List.set state.gaps index filled,
            files: nextFiles,
        }

fillGap : Gap, U64, List FileIndex -> { gap : Gap, files : List FileIndex }
fillGap = \gap, index, files ->
    when findGapCandidate files gap.size index is
        Ok file ->
            remainingGap = gap.size - file.size
            insertions = List.append gap.insertions file
            deleteFileIdx = Num.toU64 file.id

            next = {
                files: List.set files deleteFileIdx (Deleted file.size),
                gap: { size: remainingGap, insertions },
            }

            if remainingGap == 0 then
                next
            else
                fillGap next.gap index next.files

        Err NoCandidate -> { gap, files }

toDiskSequence : List U8 -> DiskSequence
toDiskSequence = \input ->
    ListUtil.splitAlternating input
    |> \(files, gaps) -> {
        files: List.mapWithIndex files \sizeCh, id ->
            Remaining { size: sizeCh - '0', id: Num.toU16 id },
        gaps: List.map gaps \sizeCh ->
            { size: sizeCh - '0', insertions: [] },
    }

findGapCandidate : List FileIndex, U8, U64 -> Result File [NoCandidate]
findGapCandidate = \files, size, minIndex ->
    init = { index: List.len files - 1, candidate: Err NoCandidate }
    List.walkBackwardsUntil files init \state, fileIndex ->
        if state.index < minIndex then
            Break state
        else
            when fileIndex is
                Remaining file if file.size <= size -> Break { state & candidate: Ok file }
                _ -> Continue { state & index: state.index - 1 }
    |> .candidate

diskSequenceToBlocks : DiskSequence -> List Block
diskSequenceToBlocks = \{ gaps, files } ->
    List.map2 files (List.append gaps { size: 0, insertions: [] }) \fileIndex, gap ->
        original =
            when fileIndex is
                Deleted size -> blockList Free size
                Remaining file -> blockList (Data file.id) file.size

        inserted = List.joinMap gap.insertions \file -> blockList (Data file.id) file.size
        gapBlocks = blockList Free gap.size

        original |> List.concat inserted |> List.concat gapBlocks
    |> List.join

part1 = \input -> Str.toUtf8 input |> solve1 |> Num.toStr |> Ok
part2 = \input -> Str.toUtf8 input |> solve2 |> Num.toStr |> Ok

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
