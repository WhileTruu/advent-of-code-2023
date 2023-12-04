app "day-2"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "./parser/package/main.roc",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        "day-4-input.txt" as inputData : Str,
    ]
    provides [main] to pf

main : Task {} *
main =
    a = calculate inputData
    Stdout.line "\(Num.toStr a)"

calculate = \input ->
    input
    |> Str.split "\n"
    |> List.map \line ->
        line
        |> Str.replaceFirst "Card " ""
        |> Str.split ": "
        |> List.map \a -> Str.split a " | "
        |> List.join
    |> List.keepOks \cardContents ->
        when cardContents is
            [cardNum, winningNumbers, numbersYouHave] ->
                Ok {
                    cardNum: cardNum,
                    winning: Str.split winningNumbers " "
                    |> List.map \a -> Str.trim a
                    |> List.dropIf \a -> Str.isEmpty a
                    |> List.keepOks \a -> Str.toNat a,
                    numbers: Str.split numbersYouHave " "
                    |> List.map \a -> Str.trim a
                    |> List.dropIf \a -> Str.isEmpty a
                    |> List.keepOks \a -> Str.toNat a,
                }

            _ -> Err {}
    |> List.map \{ winning, numbers } ->
        numbers
        |> List.walk 0 \state, num ->
            if List.contains winning num then Num.max 1 (state * 2) else state
    |> List.sum

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

