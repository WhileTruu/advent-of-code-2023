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
    result = calculateP2 inputData |> Num.toStr

    end <- Task.await Utc.now

    _ <- Task.await (Stdout.line "result \(result)")
    Stdout.line "delta \(Num.toStr (Utc.deltaAsMillis start end))"


calculateP2 = \input ->
    input
    |> Str.trim
    |> Str.split "\n"
    |> List.map Str.graphemes
    |> Array2D.fromLists FitShortest
    |> Array2D.rotateClockwise
    |> runCycles 1000
    |> Array2D.rotateClockwise
    |> rollRocksToTheRight
    |> Array2D.toLists
    |> \a -> 
        dbg 
            (a |> List.map (\a1 -> Str.joinWith a1 "") |> Str.joinWith "\n")
        a
    |> List.walkWithIndex 0 \state, line, index ->
        state + ((index + 1) * List.countIf line \a -> a == "O")


runCycles = \input, n -> runCyclesHelp input n 0

# north, then west, then south, then east.
runCyclesHelp = \input, state, n ->
    if n < state then
        input 
        |> rollRocksToTheRight
        |> Array2D.rotateClockwise
        |> rollRocksToTheRight
        |> Array2D.rotateClockwise
        |> rollRocksToTheRight
        |> Array2D.rotateClockwise
        |> rollRocksToTheRight
        |> \x ->
            something =
                x 
                |> Array2D.rotateClockwise
                |> Array2D.rotateClockwise
                |> rollRocksToTheRight
                |> Array2D.toLists 
                |> List.map (\a1 -> Str.joinWith a1 "") 
                |> Str.joinWith "\n"

            # dbg something
                
            dbg 
                x
                |> Array2D.rotateClockwise
                |> Array2D.rotateClockwise
                |> rollRocksToTheRight
                |> Array2D.toLists
                    #|> \a -> 
                    #    dbg 
                    #        (a |> List.map (\a1 -> Str.joinWith a1 "") |> Str.joinWith "\n")
                    #    a
                |> List.walkWithIndex 0 \state1, line, index ->
                    state1 + ((index + 1) * List.countIf line \a -> a == "O")
            x
        |> Array2D.rotateClockwise
        |> runCyclesHelp state (n + 1)

    else
        input
        



calculateP1 = \input ->
    input
    |> Str.trim
    |> Str.split "\n"
    |> List.map Str.graphemes
    |> Array2D.fromLists FitShortest
    |> Array2D.rotateClockwise
    |> rollRocksToTheRight
    |> Array2D.rotateClockwise
    |> Array2D.toLists
    |> List.walkWithIndex 0 \state, line, index ->
        state + ((index + 1) * List.countIf line \a -> a == "O")

rollRocksToTheRight = \arrayOfRocks ->
    arrayOfRocks
    |> Array2D.toLists
    |> List.map rollALineOfRocksToTheRight
    |> Array2D.fromLists FitShortest

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

    result = calculateP1 input

    result == 136
