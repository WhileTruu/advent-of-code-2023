app "day-2"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "./parser/package/main.roc",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        pf.Utc,
        "day-5-input.txt" as inputData : Str,
    ]
    provides [main] to pf

main : Task {} *
main =
    start <- Task.await Utc.now
    lowestLocNumStr = findLowestLocNum inputData
        |> Result.map (\a -> Num.toStr a)
        |> \a ->
            dbg a
            a
        |> Result.withDefault "fail"
    end <- Task.await Utc.now
    _ <- Task.await (Stdout.line "answer \(lowestLocNumStr)")

    Stdout.line "delta \(Num.toStr (Utc.deltaAsMillis start end))"

findLowestLocNum = \input ->
    inputParts = Str.split input "\n\n"
    seedsLine <- Result.try (List.get inputParts 0)
     
    seeds = 
        seedsLine 
        |> Str.replaceFirst "Seeds: " "" 
        |> Str.split " " 
        |> List.keepOks \a -> Str.toNat a
        |> List.chunksOf 2
        |> List.keepOks \a -> 
            when a is
                [start, len] ->
                    Ok (start, start + len)
                _ ->
                    Err Yolo
        |> List.walk [] \state, (s1, e1) ->
            state
            |> List.findFirst \(s2, e2) ->
                s1 >= s2 && s1 <= e2 || e1 >= s2 && e1 <= e2
            |> Result.map \(s2, e2) ->
                dbg "hello found"
                List.append state (Num.min s1 s2, Num.max e1 e2)
            |> Result.withDefault (List.append state (s1, e1))
            |> \a ->
                dbg a
                a
        |> List.map \(start, end) ->
            List.range { start: At start, end: At end }
        |> List.join
    max = List.len seeds 
    dbg ("len", List.len seeds)
    
    conversionMaps <- inputParts 
        |> List.dropFirst 1
        |> List.walk (Ok []) \state, rawConversionMap ->
            conversionMapLines <- 
                Str.split rawConversionMap "\n"
                |> List.dropFirst 1
                # Comment the next line line out when running example
                |> List.dropLast 1
                |> List.walk (Ok []) \state1, a ->
                    when Str.split a " " is
                        [dst, src, len] -> 
                            src1 <- Result.try (Str.toNat src)
                            dst1 <- Result.try (Str.toNat dst)
                            len1 <- Result.try (Str.toNat len)

                            state1 
                            |> Result.map \a1 -> 
                                List.append a1 { src: src1, dst: dst1, len: len1 }
                        _ -> Err (InvalidStr a)
                |> Result.try



           
            state |> Result.map (\a -> List.append a conversionMapLines)
        |> Result.try
           
    dbg "conversionmaps done"

    seeds
    |> List.walk (0, 9999999999) \(i, state), seed ->
        conversionMaps
        |> List.walk (seed) \state1, conversionMap ->
            appleSauce state1 conversionMap
        |> \a -> 
            dbg ("min", (i, max), Num.min state a)
            (i + 1, Num.min state a)
    |> \(_, a) -> Ok a

       

appleSauce : Nat, List { src : Nat, dst : Nat, len : Nat } -> Nat
appleSauce = \val, conversionMap ->
    conversionMap 
    |> List.findFirst \{ src, dst, len } -> val >= src && val <= (src + len) 
    |> Result.map \{ src, dst, len } -> dst + val - src
    |> Result.withDefault val


exampleInput =
    """
    seeds: 79 14 55 13

    seed-to-soil map:
    50 98 2
    52 50 48

    soil-to-fertilizer map:
    0 15 37
    37 52 2
    39 0 15

    fertilizer-to-water map:
    49 53 8
    0 11 42
    42 0 7
    57 7 4

    water-to-light map:
    88 18 7
    18 25 70

    light-to-temperature map:
    45 77 23
    81 45 19
    68 64 13

    temperature-to-humidity map:
    0 69 1
    1 0 69

    humidity-to-location map:
    60 56 37
    56 93 4
    """

expect
    result = findLowestLocNum exampleInput
    result == Ok 35

