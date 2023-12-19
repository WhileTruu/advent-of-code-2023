app "day-17"
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

    result = findMinimumHeatLoss exampleInput

    end <- Task.await Utc.now

    _ <- Task.await (Stdout.line "result \(Num.toStr result)")

    Stdout.line "delta \(Num.toStr (Utc.deltaAsMillis start end))"

findMinimumHeatLoss = \input ->
    x = parseToLikeASolidArray2D input

    max = Array2D.walk x 0 { direction: Forwards } \state, a, _ ->
        state + Num.toNat a - 48

    findMinimumHeatLossHelp x max 
        [{ sum: 0, index: { x: 0, y: 0 }, dir: Right, i: 0 }]

Direction : [Up, Down, Left, Right]

findMinimumHeatLossHelp :
    Array2D U8,
    Nat,
    List { sum : Nat, index : Index, dir : Direction, i : Nat }
    -> Nat
findMinimumHeatLossHelp = \array2D, state, stack ->
    move = \s, { index, direction, sum, i } ->
        ni = moveInDirection array2D direction index
        when ni is
            Ok a ->
                dbg ("lol2", index)
                when Array2D.get array2D a is
                    Ok a1 ->
                        dbg ("lol", index)
                        List.append s 
                            { 
                                sum: sum + Num.toNat a1 - 48,
                                index: a, 
                                dir: direction,
                                i,
                            }
                    Err _ ->
                        s

            _ ->
                s

    when stack is
        [] -> state
        [.., { sum, dir, index, i }] ->
            # dbg (index, sum, state, Array2D.shape array2D, )

            if sum > state then
                findMinimumHeatLossHelp array2D state (List.dropLast stack 1)
            else
                { dimX, dimY } = Array2D.shape array2D
                dbg (index, sum)

                if index == { x: dimX - 1, y: dimY - 1 } then
                    dbg "found the end"

                    findMinimumHeatLossHelp array2D (Num.min state sum) (List.dropLast stack 1)
                else
                    when dir is
                        Up ->
                            appleSauce = 
                                (if i < 2 then [Left, Right, Up] else [Left, Right])
                                |> List.walk (List.dropLast stack 1) \s, d ->
                                    i1 = if d == Up then i + 1 else 0
                                    move s { index, direction: d, sum, i: i1 }

                            findMinimumHeatLossHelp array2D state appleSauce

                        Right ->
                            appleSauce =
                                (if i < 2 then [Up, Down, Right] else [Up , Down])
                                |> List.walk (List.dropLast stack 1) \s, d ->
                                    i1 = if d == Right then i + 1 else 0
                                    move s { index, direction: d, sum, i: i1 }

                            findMinimumHeatLossHelp array2D state appleSauce

                        Down ->
                            appleSauce =
                                (if i < 2 then [Left, Right, Down] else [Left, Right])
                                |> List.walk (List.dropLast stack 1) \s, d ->
                                    i1 = if d == Down then i + 1 else 0
                                    move s { index, direction: d, sum, i: i1 }

                            findMinimumHeatLossHelp array2D state appleSauce

                        Left ->
                            appleSauce =
                                (if i < 2 then [Up, Down, Left] else [Up, Down])
                                |> List.walk (List.dropLast stack 1) \s, d ->
                                    i1 = if d == Left then i + 1 else 0
                                    move s { index, direction: d, sum, i: i1 }

                            findMinimumHeatLossHelp array2D state appleSauce

moveInDirection : Array2D U8, Direction, Index -> Result Index [Yolo]
moveInDirection = \array2D, direction, { x, y } ->
    when direction is
        Up ->
            if y == 0 then
                Err Yolo
            else
                Ok { x, y: y - 1 }

        Right ->
            if x >= (Array2D.shape array2D).dimX - 1 then
                Err Yolo
            else
                Ok { y, x: x + 1 }

        Down ->
            if y >= (Array2D.shape array2D).dimY - 1 then
                Err Yolo
            else
                Ok { x, y: y + 1 }

        Left ->
            if x == 0 then
                Err Yolo
            else
                Ok { y, x: x - 1 }

parseToLikeASolidArray2D = \input ->
    input
    |> Str.trim
    |> Str.split "\n"
    |> List.map Str.toUtf8
    |> Array2D.fromLists FitShortest
    |> Array2D.transpose

exampleInput =
    """
    2413432311323
    3215453535623
    3255245654254
    3446585845452
    4546657867536
    1438598798454
    4457876987766
    3637877979653
    4654967986887
    4564679986453
    1224686865563
    2546548887735
    4322674655533    
    """

expect
    result = findMinimumHeatLoss exampleInput
    result == 102
