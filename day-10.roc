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

    pipeField =
        inputData
        |> Str.split "\n"
        |> List.map Str.graphemes
        |> List.dropLast 1
        |> Array2D.fromExactLists
        |> \array2D ->
            when array2D is
                Ok a -> a
                Err _ -> crash "failed with the array stuff"
        |> Array2D.transpose # |> Array2D.rotateClockwise
        # |> Array2D.flipX
        # |> Array2D.flipY
        |> Array2D.mapWithIndex \a, index ->
            dbg
                (a, index)

            a

    s =
        pipeField
        |> Array2D.findFirstIndex \a -> a == "S"
        |> withLazyDefault \_ -> crash "startIndex yolo"

    x = katie pipeField 0 ("S", s) ("|", { s & y: s.y + 1 })

    result = x

    end <- Task.await Utc.now
    _ <- Task.await (Stdout.line "answer \(Num.toStr result)")

    Stdout.line "delta \(Num.toStr (Utc.deltaAsMillis start end))"

katie : Array2D Str, Nat, (Str, Array2D.Index), (Str, Array2D.Index) -> Nat
katie = \array2D, step, prev, current ->
    nextPos = getNextPos prev current

    nextGrapheme =
        Array2D.get array2D nextPos
        |> withLazyDefault \_ -> crash "katie yolo"

    nextStep = step + 1
    dbg
        nextStep

    if nextGrapheme == "S" then
        (step + 2) // 2
    else
        katie array2D nextStep current (nextGrapheme, nextPos)

getNextPos : (Str, Array2D.Index), (Str, Array2D.Index) -> Array2D.Index
getNextPos = \p, c ->
    dbg
        (p, c)

    dbg
        ("hello", c.0)

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
        "-" -> { 
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
    pos = getNextPos ("7", {x: 124, y: 110}) ("-", {x: 123, y: 110})
    pos == { x: 122, y: 110 }


withLazyDefault = \r, f ->
    when r is
        Ok a -> a
        Err _ -> f {}

exampleInput =
    """
    7-F7-
    .FJ|7
    SJLL7
    |F--J
    LJ.LJ
    """

