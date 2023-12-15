app "day-15"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        pf.Utc,
        "inputs/day-15.txt" as inputData : Str,
    ]
    provides [main] to pf

main : Task {} *
main =
    start <- Task.await Utc.now
    
    # 1034480 too high

    result = inputData
        |> Str.trim
        |> Str.split ","
        |> List.walk (Dict.empty {}) \state, str -> 
            if Str.endsWith str "-" then
                Str.graphemes str
                |> List.dropLast 1
                |> Str.joinWith ""
                |> \key -> 
                    Dict.update state (hash key) \value ->
                        when value is
                        Present list -> Present (List.dropIf list \a -> a.0 == key)
                        Missing -> Missing
            else
                when Str.split str "=" is
                   [key, value] ->
                        Dict.update state (hash key) \value1 ->
                            when value1 is
                                Present list -> 
                                    list
                                    |> List.findFirstIndex \a -> a.0 == key
                                    |> \a -> 
                                        when a is
                                            Ok index ->
                                                list 
                                                |> List.set index (key, Str.toNat value |> Result.withDefault 0)
                                                |> Present
                                            _ ->                    
                                                list
                                                |> List.append (key, Str.toNat value |> Result.withDefault 0)
                                                |> Present
                                                
                                Missing -> Present [ (key, Str.toNat value |> Result.withDefault 0)]
                   _ -> state

        |> Dict.toList
        |> \a -> 
            dbg a
            a

        |> List.walk 0 \state, (k, v) ->
            val = List.walkWithIndex v 0 \state1, (_, focalLen), i ->
                dbg 
                    (k + 1) * (i + 1) * focalLen
                (k + 1) * (i + 1) * focalLen + state1
            val + state
                

    end <- Task.await Utc.now

    _ <- Task.await (Stdout.line "result \(Num.toStr result)")
    Stdout.line "delta \(Num.toStr (Utc.deltaAsMillis start end))"

hash = \str ->
    str
    |> Str.toUtf8
    |> List.walk 0 \state, asciiCode ->
        ((17 * (state + Num.toNat asciiCode)) % 256)

exampleInput = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

expect
    input = "rn=1"
    result = hash input
    result == 30

expect
    input = "cm-"
    result = hash input
    result == 253
expect
    input = "qp=3"
    result = hash input
    result == 97
expect
    input = "cm=2"
    result = hash input
    result == 47
expect
    input = "qp-"
    result = hash input
    result == 14
expect
    input = "pc=4"
    result = hash input
    result == 180

expect
    input = "ot=9"
    result = hash input
    result == 9

expect
    input = "ab=5"
    result = hash input
    result == 197

expect
    input = "pc-"
    result = hash input
    result == 48

expect
    input = "pc=6"
    result = hash input
    result == 214

expect
    input = "ot=7"
    result = hash input
    result == 231

