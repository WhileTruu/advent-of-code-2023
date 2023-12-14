app "day-13"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "./parser/package/main.roc",
        array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.1.0/ssMT0bDIv-qE7d_yNUyCByGQHvpNkQJZsGUS6xEFsIY.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        pf.Utc,
        "inputs/day-14.txt" as inputData : Str,
        array2d.Array2D.{ Array2D },
    ]
    provides [main] to pf

main : Task {} *
main =
    start <- Task.await Utc.now

    result = calculate inputData |> Num.toStr

    end <- Task.await Utc.now

    _ <- Task.await (Stdout.line "result \(result)")
    Stdout.line "delta \(Num.toStr (Utc.deltaAsMillis start end))"

calculate = \input ->
    input
    |> Str.trim
    |> Str.split "\n"
    |> List.map Str.graphemes
    |> Array2D.fromLists FitShortest
    |> rollRocksToNorth
    |> Array2D.toLists
    |> List.walkWithIndex 0 \state, line, index ->
        state + ((index + 1) * List.countIf line \a -> a == "O")

rollRocksToNorth = \arrayOfRocks ->
    arrayOfRocks
    |> Array2D.rotateClockwise
    |> Array2D.toLists
    |> List.map rollALineOfRocksToTheRight
    |> Array2D.fromLists FitShortest
    |> Array2D.rotateClockwise

# north, then west, then south, then east.
rollALineOfRocksToTheRight = \lineOfRocks ->
    lineOfRocks
    |> List.reverse
    |> List.walkWithIndex ([], NotAvailable) \(state, index), a, i ->
        when a is
            "#" -> (List.append state "#", NotAvailable)
            "." ->
                doink =
                    when index is
                        NotAvailable -> Available i
                        _ -> index

                (List.append state ".", doink)

            "O" ->
                when index is
                    Available availableIndex ->
                        (
                            state
                            |> List.set availableIndex "O"
                            |> List.append ".",
                            Available (availableIndex + 1),
                        )

                    NotAvailable ->
                        (List.append state "O", NotAvailable)

            _ -> crash "yolo"
    |> \a -> a.0
    |> List.reverse

exampleInput =
    """
    O....#....
    O.OO#....#
    .....##...
    OO.#O....O
    .O.....O#.
    O.#..O.#.#
    ..O..#O..O
    .......O..
    #....###..
    #OO..#....
    """

expect
    input = "##..O.O.OO"
    result = rollALineOfRocksToTheRight (Str.graphemes input)
    result == Str.graphemes "##....OOOO"

expect
    input =
        """
        O....#....
        O.OO#....#
        .....##...
        OO.#O....O
        .O.....O#.
        O.#..O.#.#
        ..O..#O..O
        .......O..
        #....###..
        #OO..#....
        """

    result = calculate input

    result == 136
