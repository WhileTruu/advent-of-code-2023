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

    result = 
        katie 
            exampleInputLeftOrRightInstructions 
            exampleInputNetworkOfLabeledNodes 
            0 
            "AAA"
       

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
    
    if nextLabeledNode == "ZZZ" then 
        steps + 1  
    else
        katie instructions network (steps + 1) nextLabeledNode 
            

exampleInputLeftOrRightInstructions = "LLR"

exampleInputNetworkOfLabeledNodes =
    Dict.empty {}
    |> Dict.insert "AAA" ("BBB", "BBB")
    |> Dict.insert "BBB" ("AAA", "ZZZ")
    |> Dict.insert "ZZZ" ("ZZZ", "ZZZ")
