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
        "inputs/day-13.txt" as inputData : Str,
        array2d.Array2D.{ Array2D },
    ]
    provides [main] to pf

main : Task {} *
main =
    start <- Task.await Utc.now

    result = sum inputData

    end <- Task.await Utc.now
    _ <- Task.await (Stdout.line "answer \(Num.toStr result)")

    Stdout.line "delta \(Num.toStr (Utc.deltaAsMillis start end))"

sum = \input ->
    input
    |> Str.trim
    |> Str.split "\n\n"
    |> List.walk (0, 0) \state, a ->
        (state.0 + summarize a, state.1 + summarizeVertical a)
    |> \a -> a.0 * 100 + a.1

summarizeVertical = \input ->
    list =
        Str.split input "\n"
        |> List.map Str.graphemes
        |> Array2D.fromLists FitShortest
        |> Array2D.rotateClockwise
        |> Array2D.toLists
        |> List.map \a1 -> Str.joinWith a1 ""

    when list is
        [a, ..] -> summarizeHelp 0 [a] (List.dropFirst list 1)
        _ -> 0

summarize = \input ->
    list = Str.split input "\n"

    when list is
        [a, ..] -> summarizeHelp 0 [a] (List.dropFirst list 1)
        _ -> 0

summarizeHelp = \state, left, right ->
    if List.isEmpty left || List.isEmpty right then
        0
    else if areSame left right then
        state + 1
    else
        when right is
            [a, ..] ->
                summarizeHelp
                    (state + 1)
                    (List.prepend left a)
                    (List.dropFirst right 1)

            _ -> 0

areSame = \a, b ->
    when (a, b) is
        ([a1, ..], [b1, ..]) ->
            if a1 == b1 then
                areSame (List.dropFirst a 1) (List.dropFirst b 1)
            else
                Bool.false

        _ -> Bool.true

expect
    input =
        """
        #..########..##
        ....#....#....#
        .##...##...##..
        .##...##...##..
        ####.#..#.####.
        ##############.
        .##...##...##..
        ......##......#
        .##.#....#.##..
        ##.#......#.###
        ####......####.
        """

    result = sum input
    result == 7

expect
    input =
        """
        #####...####.#.
        ##..###..#..#.#
        ...#.###.....#.
        ...#.###.....#.
        ##..###..#..#.#
        #####...####.#.
        ##....####.....
        #..##.....#..#.
        #..##.....#..#.
        ##....####.....
        #####...######.
        ##..###..#..#.#
        ...#.###.....#.
        """
    result = sum input
    result == 300

expect
    input =
        """
        #.##..##.
        ..#.##.#.
        ##......#
        ##......#
        ..#.##.#.
        ..##..##.
        #.#.##.#.

        #...##..#
        #....#..#
        ..##..###
        #####.##.
        #####.##.
        ..##..###
        #....#..#
        """
    result = sum input
    result == 405

expect
    input =
        """
        #.##..##.
        ..#.##.#.
        ##......#
        ##......#
        ..#.##.#.
        ..##..##.
        #.#.##.#.

        #...##..#
        #....#..#
        ..##..###
        #####.##.
        #####.##.
        ..##..###
        #....#..#

        #####...####.#.
        ##..###..#..#.#
        ...#.###.....#.
        ...#.###.....#.
        ##..###..#..#.#
        #####...####.#.
        ##....####.....
        #..##.....#..#.
        #..##.....#..#.
        ##....####.....
        #####...######.
        ##..###..#..#.#
        ...#.###.....#.

        #..########..##
        ....#....#....#
        .##...##...##..
        .##...##...##..
        ####.#..#.####.
        ##############.
        .##...##...##..
        ......##......#
        .##.#....#.##..
        ##.#......#.###
        ####......####.
        """
    result = sum input
    result == 712

expect
    input =
        """
        ...##.#
        .##.###
        .##.###
        ...##.#
        #...###
        .#..##.
        ##.##.#
        ...####
        #.#.###
        #....#.
        ..#.##.
        ..#.##.
        #....#.
        #.#.#.#
        ...####

        ###..##.######.##
        ..#..#.#......#.#
        #.##.#..#.##.#..#
        #.#.##.#..##..#.#
        #..#...###..###..
        #..#...###..###..
        #.#.##.#..##..#.#
        #.##.#..#.##.#..#
        ..#..#.#......#.#
        ###..##.######.##
        ...#.#..######..#
        ..###....#..#..#.
        #.###....#..#....
        ##.#####.#..#.###
        ...#.....####....
        ..#...#.#.##.#.#.
        ...###.#......#.#

        ##.##.#####
        #.####.#..#
        #.####.#..#
        ##.##.###.#
        ########...
        .#....#....
        ###..#####.

        ..#.#.#...####.
        ..#.#...#....##
        ##.##.#..##....
        ...#..#.....#..
        ##......#.##.#.
        ..####.#.#.#...
        ..####.#.###...
        """

    result = sum input
    result == 705

expect
    input =
        """
        ###..##.######.##
        ..#..#.#......#.#
        #.##.#..#.##.#..#
        #.#.##.#..##..#.#
        #..#...###..###..
        #..#...###..###..
        #.#.##.#..##..#.#
        #.##.#..#.##.#..#
        ..#..#.#......#.#
        ###..##.######.##
        ...#.#..######..#
        ..###....#..#..#.
        #.###....#..#....
        ##.#####.#..#.###
        ...#.....####....
        ..#...#.#.##.#.#.
        ...###.#......#.#
        """

    result = sum input
    result == 500

expect
    input =
        """
        ...##.#
        .##.###
        .##.###
        ...##.#
        #...###
        .#..##.
        ##.##.#
        ...####
        #.#.###
        #....#.
        ..#.##.
        ..#.##.
        #....#.
        #.#.#.#
        ...####
        """

    result = sum input
    result == 200

expect
    input =
        """
        ##.##.#####
        #.####.#..#
        #.####.#..#
        ##.##.###.#
        ########...
        .#....#....
        ###..#####.
        """

    result = sum input
    result == 4
