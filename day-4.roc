app "day-2"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "./parser/package/main.roc",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        pf.Utc,
        "day-4-b.sample-data.txt" as inputData : Str,
    ]
    provides [main] to pf

main : Task {} *
main =
    start <- Task.await Utc.now
    a = calculate inputData
    _ <- Task.await (Stdout.line "\(Num.toStr a)")
    end <- Task.await Utc.now

    Stdout.line "\(Num.toStr (Utc.deltaAsMillis start end))"

calculate = \input ->
    input
    |> Str.split "\n"
    |> List.map \line ->
        line
        |> Str.replaceFirst "Card" ""
        |> Str.split ": "
        |> List.map \a -> Str.split a " | "
        |> List.join
    |> List.keepOks \cardContents ->
        when cardContents is
            [cardNum, winningNumbers, numbersYouHave] ->
                cardNum1 <- Result.try (Str.toNat (Str.trim cardNum))

                Ok {
                    cardNum: cardNum1,
                    winning: Str.split winningNumbers " "
                    |> List.map \a -> Str.trim a
                    |> List.dropIf \a -> Str.isEmpty a
                    |> List.keepOks \a -> Str.toNat a,
                    numbers: Str.split numbersYouHave " "
                    |> List.map \a -> Str.trim a
                    |> List.dropIf \a -> Str.isEmpty a
                    |> List.keepOks \a -> Str.toNat a,
                }

            _ ->
                Err Yolo
    |> List.map \{ winning, numbers } ->
        numbers
        |> List.walk 0 \a, num ->
            if List.contains winning num then
                a + 1
            else
                a
    |> \cards ->
        calc { s: 0, e: List.len cards } (List.len cards) cards

calc = \{ s, e }, sum, all ->
    if s > e then
        sum
    else
        when List.get all s is
            Err _ -> sum
            Ok cardWins ->
                sum
                + cardWins
                + calc { s: s + 1, e: e } 0 all
                + calc { s: s + 1, e: s + cardWins } 0 all

exampleInput =
    """
    Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
    Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
    Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
    Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
    Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
    """

expect
    result = calculate exampleInput
    result == 13

