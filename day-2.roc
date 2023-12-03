app "day-2"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        parser.Core,
        parser.String,
        "day-2-input.txt" as inputData : Str,
    ]
    provides [main] to pf

main : Task {} *
main =
    output =
        inputData |> calculate

    Stdout.line ("part 1: \(Num.toStr output.p1), part 2: \(Num.toStr output.p2)")

calculate = \input ->
    List.walk (Str.split input "\n") { p1: 0, p2: 0 } \state, line ->
        Core.const \id -> \revealed ->
                possible =
                    List.all (List.join revealed) \a ->
                        when a is
                            (count, Red) -> count <= 12
                            (count, Green) -> count <= 13
                            (count, Blue) -> count <= 14

                powerOfMinSetCubes =
                    List.walk (List.join revealed) { red: 0, green: 0, blue: 0 } \a, (count, color) ->
                        when color is
                            Red -> { a & red: Num.max a.red count }
                            Green -> { a & green: Num.max a.green count }
                            Blue -> { a & blue: Num.max a.blue count }
                    |> \a -> a.red * a.green * a.blue

                {
                    p1: if possible then state.p1 + id else state.p1,
                    p2: state.p2 + powerOfMinSetCubes,
                }
        |> Core.skip (String.string "Game ")
        |> Core.keep String.digits
        |> Core.skip (String.string ": ")
        |> Core.keep
            (
                Core.oneOf [
                    Core.const (\num -> (num, Green))
                    |> Core.keep String.digits
                    |> Core.skip (String.string " green"),
                    Core.const (\num -> (num, Red))
                    |> Core.keep String.digits
                    |> Core.skip (String.string " red"),
                    Core.const (\num -> (num, Blue))
                    |> Core.keep String.digits
                    |> Core.skip (String.string " blue"),
                ]
                |> Core.sepBy (String.string ", ")
                |> Core.sepBy (String.string "; ")
            )
        |> String.parseStr line
        |> Result.withDefault state

exampleInput =
    """
    Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
    Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
    Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
    Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
    """

expect (calculate exampleInput) == { p1: 8, p2: 2286 }
