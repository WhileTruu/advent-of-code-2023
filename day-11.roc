app "day-11"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "./parser/package/main.roc",
        array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.1.0/ssMT0bDIv-qE7d_yNUyCByGQHvpNkQJZsGUS6xEFsIY.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        pf.Utc,
        "inputs/day-11.txt" as inputData : Str,
        array2d.Array2D.{ Array2D },
    ]
    provides [main] to pf

main : Task {} *
main =
    start <- Task.await Utc.now

    data =
        inputData 
        |> Str.trim
        |> Str.split "\n"
        |> List.map Str.graphemes
        |> Array2D.fromExactLists
        |> \array2D ->
            when array2D is
                Ok a -> a
                Err _ -> crash "failed with the array stuff"
        |> Array2D.transpose

    findEmptySpaceListIndex = \array2D ->
        array2D
        |> Array2D.toLists
        |> List.walkWithIndex
            []
            (\state, list, index ->
                if List.all list \a -> a == "." then
                    List.append state index
                else
                    state
            )

    emptyCols =
        data
        |> findEmptySpaceListIndex

    emptyRows =
        data
        |> Array2D.rotateClockwise
        |> findEmptySpaceListIndex

    galaxyIndices =
        data
        |> Array2D.walk
            []
            { direction: Forwards }
            \state, grapheme, index ->
                if grapheme == "#" then
                    List.append state index
                else
                    state
    
    # 7611654 - too LOW

    result = sumOfShortesPathBetweenAllGalaxyPairs emptyCols emptyRows galaxyIndices

    end <- Task.await Utc.now
    _ <- Task.await (Stdout.line "answer \(Num.toStr result)")

    Stdout.line "delta \(Num.toStr (Utc.deltaAsMillis start end))"

sumOfShortesPathBetweenAllGalaxyPairs = \emptyX, emptyY, indices ->
    List.walk indices [] \state, index ->
        indices
        |> List.dropIf \a -> a == index
        |> List.walk [] \otherState, otherIndex ->
            dist = 
                Num.absDiff index.x otherIndex.x
                + Num.absDiff index.y otherIndex.y
                + (List.walk emptyX 0 \s, x ->
                    if (index.x < x && x < otherIndex.x)
                        || (otherIndex.x < x && x < index.x)
                    then s + 1
                    else s
                  )
                + (List.walk emptyY 0 \s, y ->
                    if (index.y < y && y < otherIndex.y)
                        || (otherIndex.y < y && y < index.y)
                    then s + 1
                    else s
                  )
            
            isInState = \s, iA, iB ->
               List.any s \a -> 
                   a.0 == iA && a.1 == iB 
                   || a.0 == iB && a.1 == iA 
            
            if isInState state index otherIndex then
                otherState 
            else
                List.append otherState (index, otherIndex, dist)
        |> \a -> List.concat state a 
    |> List.walk 0 \state, a -> state + a.2


exampleInput =
    """
    ...#......
    .......#..
    #.........
    ..........
    ......#...
    .#........
    .........#
    ..........
    .......#..
    #...#.....
    """
