app "day-16"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.1.0/ssMT0bDIv-qE7d_yNUyCByGQHvpNkQJZsGUS6xEFsIY.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        pf.Utc,
        "inputs/day-16.txt" as inputData : Str,
        array2d.Array2D.{ Array2D, Index },
    ]
    provides [main] to pf

main : Task {} *
main =
    start <- Task.await Utc.now

    result = howManyTilesAreEnergized inputData

    end <- Task.await Utc.now

    _ <- Task.await (Stdout.line "result \(Num.toStr result)")

    Stdout.line "delta \(Num.toStr (Utc.deltaAsMillis start end))"

howManyTilesAreEnergized = \input ->
    array2D =
        input
        |> Str.trim
        |> Str.split "\n"
        |> List.map Str.toUtf8
        |> Array2D.fromLists FitShortest
        |> Array2D.transpose

    { dimX, dimY } = Array2D.shape array2D

    top =
        List.range { start: At 0, end: Before dimX }
        |> List.map \a ->
            (
                Set.single ({ x: a, y: 0 }, Down),
                [(Ok { x: a, y: 0 }, Down)],
            )
    right =
        List.range { start: At 0, end: Before dimY }
        |> List.map \a ->
            (
                Set.single ({ x: dimX - 1, y: a }, Left),
                [(Ok { x: dimX - 1, y: a }, Left)],
            )
    down =
        List.range { start: At 0, end: Before dimX }
        |> List.map \a ->
            (
                Set.single ({ x: a, y: dimY - 1 }, Up),
                [(Ok { x: a, y: dimY - 1 }, Up)],
            )

    left =
        List.range { start: At 0, end: Before dimY }
        |> List.map \a ->
            (
                Set.single ({ x: 0, y: a }, Right),
                [(Ok { x: 0, y: a }, Right)],
            )

    [top, right, down, left]
    |> List.join
    |> List.walk 0 \state, a ->
        x = loopidiloop array2D a.0 a.1
        if x > state then x else state

# Tweet out: "Python is always faster than C++"
# Shave moustache
# Replace all Us with Üs
# Listen to Christmas songs

Direction : [Up, Right, Down, Left]

loopidiloop : Array2D U8, Set (Index, Direction), List ([Ok Index, Yolo], Direction) -> Nat
loopidiloop = \array2D, energizedTiles, stack ->
    move = \(t, s), movements ->
        movements
        |> List.walk (t, s) \state, (i, d) ->
            ni = moveInDirection array2D d i

            when ni is
                Ok nni ->
                    if Set.contains state.0 (nni, d) then
                        state
                    else
                        (
                            Set.insert state.0 (nni, d),
                            List.append state.1 (ni, d),
                        )

                Yolo -> state

    when stack is
        [] ->
            energizedTiles
            |> Set.walk (Set.empty {}) \state, k -> Set.insert state k.0
            |> Set.len

        [.., (Yolo, direction)] ->
            newStack = List.dropIf stack \a -> a.0 == Yolo

            loopidiloop array2D energizedTiles newStack

        [.., (Ok index, direction)] ->
            # move in direction will break like if x is 0 and moving left
            when Array2D.get array2D index is
                Ok '.' ->
                    (tiles, newStack) =
                        (energizedTiles, List.dropLast stack 1)
                        |> move [(index, direction)]

                    loopidiloop array2D tiles newStack

                Ok '|' ->
                    if direction == Up || direction == Down then
                        (tiles, newstack) =
                            (energizedTiles, List.dropLast stack 1)
                            |> move [(index, direction)]

                        loopidiloop array2D tiles newstack
                    else
                        (tiles, newstack) =
                            (energizedTiles, List.dropLast stack 1)
                            |> move [(index, Up), (index, Down)]

                        loopidiloop array2D tiles newstack

                Ok '-' ->
                    if direction == Left || direction == Right then
                        (tiles, newstack) =
                            (energizedTiles, List.dropLast stack 1)
                            |> move [(index, direction)]

                        loopidiloop array2D tiles newstack
                    else
                        (tiles, newstack) =
                            (energizedTiles, List.dropLast stack 1)
                            |> move [(index, Left), (index, Right)]

                        loopidiloop array2D tiles newstack

                Ok '\\' ->
                    when direction is
                        Up ->
                            (tiles, newstack) =
                                (energizedTiles, List.dropLast stack 1)
                                |> move [(index, Left)]

                            loopidiloop array2D tiles newstack

                        Right ->
                            (tiles, newstack) =
                                (energizedTiles, List.dropLast stack 1)
                                |> move [(index, Down)]

                            loopidiloop array2D tiles newstack

                        Down ->
                            (tiles, newstack) =
                                (energizedTiles, List.dropLast stack 1)
                                |> move [(index, Right)]

                            loopidiloop array2D tiles newstack

                        Left ->
                            (tiles, newstack) =
                                (energizedTiles, List.dropLast stack 1)
                                |> move [(index, Up)]

                            loopidiloop array2D tiles newstack

                Ok '/' ->
                    when direction is
                        Up ->
                            (tiles, newstack) =
                                (energizedTiles, List.dropLast stack 1)
                                |> move [(index, Right)]

                            loopidiloop array2D tiles newstack

                        Right ->
                            (tiles, newstack) =
                                (energizedTiles, List.dropLast stack 1)
                                |> move [(index, Up)]

                            loopidiloop array2D tiles newstack

                        Down ->
                            (tiles, newstack) =
                                (energizedTiles, List.dropLast stack 1)
                                |> move [(index, Left)]

                            loopidiloop array2D tiles newstack

                        Left ->
                            (tiles, newstack) =
                                (energizedTiles, List.dropLast stack 1)
                                |> move [(index, Down)]

                            loopidiloop array2D tiles newstack

                _ ->
                    crash "yolo \(Num.toStr index.x), \(Num.toStr index.y))"

moveInDirection : Array2D U8, Direction, Index -> [Ok Index, Yolo]
moveInDirection = \array2D, direction, { x, y } ->
    when direction is
        Up ->
            if y == 0 then
                Yolo
            else
                Ok { x, y: y - 1 }

        Right ->
            if x >= (Array2D.shape array2D).dimX - 1 then
                Yolo
            else
                Ok { y, x: x + 1 }

        Down ->
            if y >= (Array2D.shape array2D).dimY - 1 then
                Yolo
            else
                Ok { x, y: y + 1 }

        Left ->
            if x == 0 then
                Yolo
            else
                Ok { y, x: x - 1 }

expect
    input =
        """
        .|...\\....
        |.-.\\.....
        .....|-...
        ........|.
        ..........
        .........\\
        ..../.\\\\..
        .-.-/..|..
        .|....-|.\\
        ..//.|....
        """

    result = howManyTilesAreEnergized input
    result == 46

