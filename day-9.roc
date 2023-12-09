app "day-9"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "./parser/package/main.roc",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        pf.Utc,
        "inputs/day-9.txt" as inputData : Str,
    ]
    provides [main] to pf

main : Task {} *
main =
    start <- Task.await Utc.now

    result = 
        inputData
        |> Str.split "\n"
        |> List.map (\a -> Str.split a " " |> List.keepOks Str.toI128)
        |> List.walk 0 \state, a -> state + predict a
            

    end <- Task.await Utc.now
    _ <- Task.await (Stdout.line "answer \(Num.toStr result)")

    Stdout.line "delta \(Num.toStr (Utc.deltaAsMillis start end))"

predict = \values ->
    sequences = \state, a ->
        diffs = differences a
        
        if List.all diffs \value -> value == 0 then 
            state |> List.append a |> List.append diffs 
        else
            sequences (List.append state a) diffs
    
    sequences [] values
    |> List.walkBackwards 0 \state, diffs ->
        when diffs is
            [.., a] -> state + a
            _ -> state
                

expect
    result = predict [0, 3, 6, 9, 12, 15]
    result == 18


differences = \values ->
    values
    |> List.walk [] \state, a ->
        when state is
            [] -> [a]
            [.., a1] -> 
                state 
                |> List.dropLast 1 
                |> List.append (a - a1)
                |> List.append a
    |> List.dropLast 1

expect
    result = differences [0, 3, 6, 9, 12, 15]
    result == [3, 3, 3, 3, 3]

exampleInput =
    """
    0 3 6 9 12 15
    1 3 6 10 15 21
    10 13 16 21 30 45
    """

