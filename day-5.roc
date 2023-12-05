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
    lowestLocNumStr = findLowestLocNum exampleInput
        |> Result.map (\a -> Num.toStr a)
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
                    Ok (List.range { start: At start, end: Length len })
                _ ->
                    Err Yolo
        |> List.join
        |> \a ->
            dbg a
            a
        |> \a ->
            [82]
        
    
    conversionMaps <- inputParts 
        |> List.dropFirst 1
        |> List.walk (Ok []) \state, rawConversionMap ->
            headerLine <- 
                Str.split rawConversionMap "\n"
                |> List.get 0
                |> Result.map \a -> Str.replaceFirst a " map:" ""
                |> Result.try 

            conversionMapLines <- 
                Str.split rawConversionMap "\n"
                |> List.dropFirst 1
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



           
            state |> Result.map (\a -> List.append a (headerLine, conversionMapLines))
        |> Result.try
            
    conversionMaps
    |> List.walk seeds \state, conversionMap ->
         state
         |> List.map \a -> appleSauce a conversionMap
    |> List.min
    

appleSauce : Nat, (Str, List { src : Nat, dst : Nat, len : Nat }) -> Nat
appleSauce = \val, (line, conversionMap) ->
    conversionMap 
    |> List.findFirst \{ src, dst, len } -> val >= src && val <= (src + len) 
    |> Result.map \{ src, dst, len } -> dst + val - src
    |> Result.withDefault val
    |> \a ->
        dbg (line, val, a)
        a


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

