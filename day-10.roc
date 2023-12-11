app "day-10"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "./parser/package/main.roc",
        array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.1.0/ssMT0bDIv-qE7d_yNUyCByGQHvpNkQJZsGUS6xEFsIY.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        pf.Utc,
        "inputs/day-10.txt" as inputData : Str,
        array2d.Array2D.{ Array2D },
    ]
    provides [main] to pf

main : Task {} *
main =
    start <- Task.await Utc.now

    result = calc (Str.trim inputData)

    end <- Task.await Utc.now
    _ <- Task.await (Stdout.line "answer \(Num.toStr result)")

    Stdout.line "delta \(Num.toStr (Utc.deltaAsMillis start end))"

katie :
    Array2D Str,
    Dict Nat (List (Str, Nat)),
    Nat,
    (Str, Array2D.Index),
    (Str, Array2D.Index)
    -> Nat
katie = \array2D, positions, step, prev, current ->
    nextPos = getNextPos prev current

    nextGrapheme =
        Array2D.get array2D nextPos
        |> withLazyDefault \_ -> crash "katie yolo"

    nextStep = step + 1

    if nextGrapheme == "S" then
        positions
        |> Dict.values
        |> List.walk
            0
            (\state, x ->
                state
                +
                (
                    List.sortWith x \a, b -> Num.compare a.1 b.1
                    |> diffs
                )
            )
    else
        katie
            array2D
            (
                Dict.update positions nextPos.y \possibleValue ->
                    when possibleValue is
                        Missing -> Present [(nextGrapheme, nextPos.x)]
                        Present value -> Present (List.append value (nextGrapheme, nextPos.x))
            )
            nextStep
            current
            (nextGrapheme, nextPos)

diffs : List (Str, Nat) -> Nat
diffs = \list -> diffsHelp list 0 0 Bool.false

diffsHelp = \list, state, i, inside ->
    x = 
        a <- Result.try (List.get list i)
        b <- Result.try (List.get list (i + 1))

        newInside =
            when a.0 is
                "|" | "L" | "J" -> !inside
                _ -> inside

        if newInside then
            enclosed = (Num.absDiff a.1 b.1) - 1

            Ok (diffsHelp list (state + enclosed) (i + 1) newInside)
        else
            Ok (diffsHelp list state (i + 1) newInside)
    
    x
    |> Result.withDefault state

getNextPos : (Str, Array2D.Index), (Str, Array2D.Index) -> Array2D.Index
getNextPos = \p, c ->
    xDiff = Num.absDiff p.1.x c.1.x
    yDiff = Num.absDiff p.1.y c.1.y

    when c.0 is
        # vertical pipe connecting north and south.
        "|" ->
            {
                x: c.1.x,
                y: if c.1.y > p.1.y then c.1.y + 1 else c.1.y - 1,
            }

        # horizontal pipe connecting east and west.
        "-" ->
            {
                x: if c.1.x > p.1.x then c.1.x + 1 else c.1.x - 1,
                y: c.1.y,
            }

        # 90-degree bend connecting north and east.
        "L" -> { x: c.1.x + yDiff, y: c.1.y - xDiff }
        # 90-degree bend connecting north and west.
        "J" -> { x: c.1.x - yDiff, y: c.1.y - xDiff }
        # 90-degree bend connecting south and west.
        "7" -> { x: c.1.x - Num.absDiff c.1.y p.1.y, y: c.1.y + Num.absDiff c.1.x p.1.x }
        # 90-degree bend connecting south and east.
        "F" -> { x: c.1.x + (p.1.y - c.1.y), y: c.1.y + (p.1.x - c.1.x) }
        # ground; there is no pipe in this tile.
        "." -> crash "ground; there is no pipe in this tile"
        # is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.
        "S" -> crash "S yolo"
        _ -> crash "unknown grapheme yolo"

expect
    pos = getNextPos ("|", { x: 0, y: 0 }) ("|", { x: 0, y: 1 })
    pos == { x: 0, y: 2 }

expect
    pos = getNextPos ("|", { x: 0, y: 0 }) ("L", { x: 0, y: 1 })
    pos == { x: 1, y: 1 }

expect
    pos = getNextPos ("-", { x: 2, y: 2 }) ("L", { x: 1, y: 2 })
    pos == { x: 1, y: 1 }

expect
    pos = getNextPos ("-", { x: 0, y: 2 }) ("J", { x: 1, y: 2 })
    pos == { x: 1, y: 1 }

expect
    pos = getNextPos ("|", { x: 2, y: 2 }) ("J", { x: 2, y: 3 })
    pos == { x: 1, y: 3 }

expect
    pos = getNextPos ("|", { x: 2, y: 2 }) ("7", { x: 2, y: 1 })
    pos == { x: 1, y: 1 }

expect
    pos = getNextPos ("-", { x: 1, y: 1 }) ("7", { x: 2, y: 1 })
    pos == { x: 2, y: 2 }

expect
    pos = getNextPos ("-", { x: 1, y: 1 }) ("F", { x: 0, y: 1 })
    pos == { x: 0, y: 2 }

expect
    pos = getNextPos ("|", { x: 1, y: 1 }) ("F", { x: 1, y: 0 })
    pos == { x: 2, y: 0 }

expect
    pos = getNextPos ("L", { x: 0, y: 4 }) ("J", { x: 1, y: 4 })
    pos == { x: 1, y: 3 }

expect
    pos = getNextPos ("J", { x: 1, y: 4 }) ("F", { x: 1, y: 3 })
    pos == { x: 2, y: 3 }

expect
    pos = getNextPos ("7", { x: 124, y: 110 }) ("-", { x: 123, y: 110 })
    pos == { x: 122, y: 110 }

withLazyDefault = \r, f ->
    when r is
        Ok a -> a
        Err _ -> f {}

calc = \input ->
    pipeField =
        input
        |> Str.split "\n"
        |> List.map Str.graphemes
        |> Array2D.fromExactLists
        |> \array2D ->
            when array2D is
                Ok a -> a
                Err _ -> crash "failed with the array stuff"
        |> Array2D.transpose

    s =
        pipeField
        |> Array2D.findFirstIndex \a -> a == "S"
        |> withLazyDefault \_ -> crash "startIndex yolo"

    next =
        pipeField
        |> Array2D.get { x: s.x, y: s.y + 1 }
        |> withLazyDefault \_ -> crash "next yolo"

    katie
        pipeField
        (
            Dict.empty {}
            |> Dict.insert s.y [("S", s.x)]
            |> Dict.insert (s.y + 1) [(next, s.x)]
        )
        0
        ("S", s)
        (next, { s & y: s.y + 1 })

exampleInput =
    """
    FF7FSF7F7F7F7F7F---7
    L|LJ||||||||||||F--J
    FL-7LJLJ||||||LJL-77
    F--JF--7||LJLJ7F7FJ-
    L---JF-JLJ.||-FJLJJ7
    |F|F-JF---7F7-L7L|7|
    |FFJF7L7F-JF7|JL---7
    7-L-JL7||F7|L7F-7F7|
    L.L7LFJ|||||FJL7||LJ
    L7JLJL-JLJLJL--JLJ.L
    """

exampleInput2 =
    """
    ...........
    .S-------7.
    .|F-----7|.
    .||.....||.
    .||.....||.
    .|L-7.F-J|.
    .|..|.|..|.
    .L--J.L--J.
    ...........
    """

exampleInput3 =
    """
    .F----7F7F7F7F-7....
    .|F--7||||||||FJ....
    .||.FJ||||||||L7....
    FJL7L7LJLJ||LJ.L-7..
    L--J.L7...LJS7F-7L7.
    ....F-J..F7FJ|L7L7L7
    ....L7.F7||L7|.L7L7|
    .....|FJLJ|FJ|F7|.LJ
    ....FJL-7.||.||||...
    ....L---J.LJ.LJLJ...
    """

expect
    result = calc exampleInput2
    result == 4

expect
    result = calc exampleInput
    result == 10

expect
    result = calc exampleInput3
    result == 8
