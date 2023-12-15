app "day-12"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "./parser/package/main.roc",
        array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.1.0/ssMT0bDIv-qE7d_yNUyCByGQHvpNkQJZsGUS6xEFsIY.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        pf.Utc,
        "inputs/day-12.txt" as inputData : Str,
        array2d.Array2D.{ Array2D },
    ]
    provides [main] to pf

main : Task {} *
main =
    start <- Task.await Utc.now

    result =
        exampleInput
        |> Str.trim
        |> Str.split "\n"
        |> List.walk 0 \state, a -> calculateArrangements a + state

    end <- Task.await Utc.now
    _ <- Task.await (Stdout.line "answer \(Num.toStr result)")

    Stdout.line "delta \(Num.toStr (Utc.deltaAsMillis start end))"

calculateArrangements = \line ->
    row =
        Str.split line " "
        |> List.get 0
        |> Result.map Str.graphemes
        |> Result.withDefault []
        |> List.repeat 5
        |> List.intersperse ["?"]
        |> List.join

    contiguousGroupOfDamagedSprings =
        Str.split line " "
        |> List.get 1
        |> Result.map (\a -> Str.split a ",")
        |> Result.withDefault []
        |> List.keepOks Str.toNat
        |> List.repeat 5
        |> List.join

    getArrangementCount [(row, contiguousGroupOfDamagedSprings)] 0

getArrangementCount = \list, state ->
    dbg 
        List.len list

    when list is
        [] -> state
        [a, ..] ->
            when a.1 is
                [] ->
                    if List.any a.0 \a1 -> a1 == "#" then
                        getArrangementCount (List.dropFirst list 1) state
                    else
                        getArrangementCount (List.dropFirst list 1) (state + 1)

                [contiguousGroupOfDamagedSprings, ..] ->
                    when List.findFirstIndex a.0 \s -> s == "?" || s == "#" is
                        Ok firstPossibleIndex ->
                            possibleGroup = List.sublist a.0 {
                                start: firstPossibleIndex,
                                len: contiguousGroupOfDamagedSprings,
                            }

                            if List.len possibleGroup != contiguousGroupOfDamagedSprings then
                                getArrangementCount (List.dropFirst list 1) state
                            else if
                                (List.all possibleGroup \s -> s == "#" || s == "?")
                                && (
                                    List.get a.0 (firstPossibleIndex + contiguousGroupOfDamagedSprings)
                                    != Ok "#"
                                )
                            then
                                getArrangementCount
                                    (
                                        list
                                        |> List.dropFirst 1
                                        |> List.prepend (
                                            List.dropFirst a.0 (firstPossibleIndex + contiguousGroupOfDamagedSprings + 1),
                                            List.dropFirst a.1 1,
                                        )
                                        |> \list1 ->
                                            if List.get a.0 firstPossibleIndex != Ok "#" then
                                                List.prepend list1 (
                                                    List.dropFirst a.0 (firstPossibleIndex + 1),
                                                    a.1,
                                                )
                                            else
                                                list1
                                    )
                                    state
                            else if List.get a.0 firstPossibleIndex == Ok "#" then
                                getArrangementCount (List.dropFirst list 1) state
                            else
                                getArrangementCount
                                    (List.dropFirst list (firstPossibleIndex + 1))
                                    state

                        Err _ ->
                            getArrangementCount (List.dropFirst list 1) state

exampleInput =
    """
    ???.### 1,1,3
    .??..??...?##. 1,1,3
    ?#?#?#?#?#?#?#? 1,3,1,6
    ????.#...#... 4,1,1
    ????.######..#####. 1,6,5
    ?###???????? 3,2,1
    """

expect
    result = calculateArrangements "???.### 1,1,3"
    result == 1

expect
    result = calculateArrangements ".??..??...?##. 1,1,3"
    result == 16384

expect
    result = calculateArrangements "?#?#?#?#?#?#?#? 1,3,1,6"
    result == 1

expect
    result = calculateArrangements "????.#...#... 4,1,1"
    result == 16

expect
    result = calculateArrangements ".# 1"
    result == 1

# expect
#     result = exampleInput |> Str.trim |> Str.split "\n" |> List.walk 0 \state, a -> state + calculateArrangements a |> \a -> [a]
#    result == [1, 4, 1, 1, 4, 10]

# expect
#    x = calculateArrangements "?###???????? 3,2,1"
#    x == 10

