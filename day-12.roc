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
                    if List.isEmpty a.0 then
                        getArrangementCount (List.dropFirst list 1) state
                    else if fits2 a.0 contiguousGroupOfDamagedSprings then
                        getArrangementCount
                            (
                                list
                                |> List.dropFirst 1
                                |> List.prepend (
                                    a.0
                                    |> List.dropFirst (contiguousGroupOfDamagedSprings + 1),
                                    a.1 |> List.dropFirst 1,
                                )
                                |> if List.get a.0 0 != Ok "#" then
                                    \list1 -> List.prepend list1 (
                                            a.0 |> List.dropFirst 1,
                                            a.1,
                                        )
                                else
                                    \list1 -> list1
                            )
                            state
                    else
                        getArrangementCount
                            (
                                list
                                |> List.dropFirst 1
                                |> if List.get a.0 0 != Ok "#" then
                                    \list1 -> List.prepend list1 (
                                            a.0 |> List.dropFirst 1,
                                            a.1,
                                        )
                                else
                                    \list1 -> list1

                            )
                            state

fits2 = \springs, n ->
    springs
    |> List.takeFirst n
    |> List.map \a -> if a == "?" then "#" else a
    |> \a -> (Str.joinWith a "" == (List.repeat "#" n |> Str.joinWith ""))
        && (
            (List.get springs n == Ok "?")
            || (List.get springs n == Ok ".")
            || (Result.isErr (List.get springs n))
        )

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

# expect
#    result = calculateArrangements ".??..??...?##. 1,1,3"
#    result == 16384

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

