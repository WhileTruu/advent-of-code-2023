app "day-6"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "./parser/package/main.roc",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        pf.Utc,
        "day-8-input.txt" as inputData : Str,
    ]
    provides [main] to pf

main : Task {} *
main =
    start <- Task.await Utc.now

    { networks, instructions } = parseInput inputData

    result =
        networks
        |> Dict.keys
        |> List.keepIf \a -> Str.endsWith a "A"
        |> List.map \a -> katie instructions networks 0 a
        |> \a -> 
            dbg a
            a
        |> lcms

    end <- Task.await Utc.now
    _ <- Task.await (Stdout.line "answer \(Num.toStr result)")

    Stdout.line "delta \(Num.toStr (Utc.deltaAsMillis start end))"

katie = \instructions, network, steps, labeledNode ->
    idx = steps % (Str.countGraphemes instructions)

    leftOrRight =
        List.get (Str.graphemes instructions) idx
        |> Result.withDefault "L"

    nextLabeledNode =
        when leftOrRight is
            "L" ->
                Dict.get network labeledNode
                |> Result.map \a -> a.0
                |> Result.withDefault "ZZZ"

            "R" ->
                Dict.get network labeledNode
                |> Result.map \a -> a.1
                |> Result.withDefault "ZZZ"

            _ ->
                "ZZZ"

    if Str.endsWith nextLabeledNode "Z" then
        steps + 1
    else
        katie instructions network (steps + 1) nextLabeledNode

parseInput = \str ->
    when Str.split str "\n\n" is
        [instructions, otherStuff] ->
            networks =
                otherStuff
                |> Str.split "\n"
                |> List.map \a ->
                    a
                    |> Str.replaceFirst " = (" ","
                    |> Str.replaceFirst ", " ","
                    |> Str.replaceFirst ")" ""
                    |> Str.split ","
                |> List.walk (Dict.empty {}) \state, line ->
                    when line is
                        [a, b, c] -> Dict.insert state a (b, c)
                        _ -> state

            { instructions, networks }

        _ ->
            { instructions: "", networks: Dict.empty {} }

gcd = \a, b -> if b == 0 then a else gcd b (a % b)

lcm = \a, b ->
    if a == 0 && b == 0 then
        0
    else
        b * (a // (gcd a b))

lcms = \s ->
    lcmsHelp 1 s

lcmsHelp = \state, s ->
    when s is
        [x1] -> lcm x1 state
        [x1, ..] -> lcmsHelp (lcm x1 state) (List.dropFirst s 1)
        [] -> 1

exampleInput =
    """
    LR

    11A = (11B, XXX)
    11B = (XXX, 11Z)
    11Z = (11B, XXX)
    22A = (22B, XXX)
    22B = (22C, 22C)
    22C = (22Z, 22Z)
    22Z = (22B, 22B)
    XXX = (XXX, XXX)
    """

