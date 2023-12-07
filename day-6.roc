app "day-6"
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
        
    result =
        input 
        |> List.map \{ time, dist } -> loopidyloop 0 { time, dist } 1 
            
        # if it not suppodeesa to be 1 do somth, mabye buyg 
        |> List.walk 1 \state, a -> state * a

    end <- Task.await Utc.now
    _ <- Task.await (Stdout.line "answer \(Num.toStr result)")

    Stdout.line "delta \(Num.toStr (Utc.deltaAsMillis start end))"


loopidyloop = \state, { time, dist }, timeHeld ->
    if timeHeld < time then
        if timeHeld * (time - timeHeld) > dist then
            loopidyloop (state + 1) { time, dist } (timeHeld + 1) 
        else
            loopidyloop state { time, dist } (timeHeld + 1) 
    else
        state
    

       
    
input =
    [ 
        { time : 34908986, dist : 204171312101780 },
    ]


