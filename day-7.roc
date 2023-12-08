app "day-6"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "./parser/package/main.roc",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        pf.Utc,
        "day-7-input.txt" as inputData : Str,
    ]
    provides [main] to pf

main : Task {} *
main =
    start <- Task.await Utc.now
    

    dbg (calc exampleInput)
    result = calc inputData

    end <- Task.await Utc.now
    _ <- Task.await (Stdout.line "answer \(Num.toStr result)")

    Stdout.line "delta \(Num.toStr (Utc.deltaAsMillis start end))"

calc = \input ->
    input
    |> Str.split "\n"
        |> List.keepOks \a -> 
            when Str.split a " " is
                [k, v] -> Str.toNat v |> Result.map \bid -> (k, bid)
                _ -> Err Yolo
        |> List.map \(a, bid) -> (a, bid, categoryRank a)
        # sort the list in the next line by comparing the two numbers
        # r1 and r2 respectively
        |> List.sortWith \(_, _, r1), (_, _, r2) -> Num.compare r1 r2
        |> List.mapWithIndex \(a, bid, _), i -> (a, bid, i + 1)
        |> List.walk 0 \state, (_, bid, rank) -> state + bid * rank

    


categoryRank = \hand ->
    jokerCount =
        hand 
        |> Str.graphemes 
        |> List.keepIf \g -> g == "J"
        |> List.len

    hand
    |> Str.graphemes
    |> List.walk (Dict.empty {}) \state, label -> 
        Dict.update state label \possibleLabel ->
            when possibleLabel is
                Missing -> Present 1
                Present value -> Present (value + 1)
    |> Dict.toList
    |> List.map \(_, a) -> a
    |> List.sortDesc
    |> \a -> 
        when a is
            [ 5 ] -> 7
            [ 4, 1 ] -> 
                if jokerCount != 0 then 
                    7 
                else 
                    6
            [ 3, 2 ] ->
                if jokerCount == 2 || jokerCount == 3 then 
                    7 
                else 
                    5
            [ 3, 1, 1 ] ->
                if jokerCount == 3 || jokerCount == 1 then 
                    6
                else 
                    4

            [ 2, 2, 1 ] -> 
                if jokerCount == 2 then 
                    6
                else if jokerCount == 1 then 
                    5
                else 
                    3

            [ 2, 1, 1, 1 ] -> 
                if jokerCount == 1 || jokerCount == 2 then 
                    4 
                else 
                    2
            _ -> 
                if jokerCount == 1 then 
                    2 
                else 
                    1
    |> \a -> a * 1000000000000000 + orderRanking hand
    |> \a -> a // 100000

cardOrder = \label ->
    "AKQT98765432J"
    |> Str.graphemes
    |> List.reverse
    |> List.findFirstIndex \a -> a == label
    |> Result.map \a -> a + 1
    |> Result.withDefault 0

orderRanking = \hand ->
    hand 
    |> Str.graphemes
    |> List.walkWithIndex 0 \state, label, index ->
        state * 1000 + Num.powInt 10 (index + 1) * (10 + cardOrder label)
        
      
exampleInput =
    """
    32T3K 765
    T55J5 684
    KK677 28
    KTJJT 220
    QQQJA 483
    """


