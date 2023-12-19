module Main exposing (main)

import Array exposing (Array)
import Array2D exposing (Array2D)
import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type alias Model =
    { count : Int }


initialModel : Model
initialModel =
    { count = 0 }


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }

        Decrement ->
            { model | count = model.count - 1 }


view : Model -> Html Msg
view model =
    let
        x =
            findMinimumHeatLoss exampleInput
    in
    div []
        [ button [ onClick Increment ] [ text "+1" ]
        , div [] [ text <| String.fromInt x ]
        , button [ onClick Decrement ] [ text "-1" ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }


findMinimumHeatLoss =
    \input ->
        let
            x =
                parseToLikeASolidArray2D input
                |> Debug.log "1"

            -- max =
            --    Array2D.toFlx
            --         0
            --         { direction = Forwards }
            --         (\state a _ ->
            --            state + Num.toNat a - 48
            --         )
        in
        x
            |> Maybe.map
                (\a ->
                    findMinimumHeatLossHelp a
                        10
                        [ { sum = 0, index = { x = 0, y = 0 }, dir = Right, i = 0 } ]
                )
            |> Maybe.withDefault 0


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Step =
    { sum : Int
    , index : { x : Int, y : Int }
    , dir : Direction
    , i : Int
    }


findMinimumHeatLossHelp : Array2D Char -> Int -> List Step -> Int
findMinimumHeatLossHelp =
    \array2D state stack ->
        let
            move : List Step -> Step -> List Step
            move =
                \s { index, dir, sum, i } ->
                    let
                        ni =
                            moveInDirection array2D dir index
                    in
                    case ni of
                        Ok a ->
                            case Array2D.get a.y a.x array2D of
                                Just a1 ->
                                    { sum = sum + Char.toCode a1 - 48
                                    , index = a
                                    , dir = dir
                                    , i = i
                                    }
                                        :: s

                                Nothing ->
                                    s

                        _ ->
                            s
        in
        case stack of
            [] ->
                state

            { sum, dir, index, i } :: others ->
                let
                    _ = Debug.log "" (index, sum, state)
                    -- dbg (index, sum, state, Array2D.shape array2D, )
                in
                if sum > state then
                    findMinimumHeatLossHelp array2D state others

                else
                    let
                        { dimX, dimY } =
                            { dimX = Array2D.columns array2D
                            , dimY = Array2D.rows array2D
                            }
                    in
                    if index == { x = dimX - 1, y = dimY - 1 } then
                        findMinimumHeatLossHelp array2D (min state sum) others

                    else
                        case dir of
                            Up ->
                                let
                                    appleSauce : List Step
                                    appleSauce =
                                        (if i < 2 then
                                            [ Left, Right, Up ]

                                         else
                                            [ Left, Right ]
                                        )
                                            |> List.reverse
                                            |> List.foldl
                                                (\d s ->
                                                    let
                                                        i1 =
                                                            if d == Up then
                                                                i + 1

                                                            else
                                                                0
                                                    in
                                                    move s
                                                        { index = index
                                                        , dir = d
                                                        , sum = sum
                                                        , i = i1
                                                        }
                                                )
                                                others
                                            |> List.reverse
                                in
                                findMinimumHeatLossHelp array2D state appleSauce

                            Right ->
                                let
                                    appleSauce =
                                        (if i < 2 then
                                            [ Up, Down, Right ]

                                         else
                                            [ Up, Down ]
                                        )
                                            |> List.reverse
                                            |> List.foldl
                                                (\d s ->
                                                    let
                                                        i1 =
                                                            if d == Right then
                                                                i + 1

                                                            else
                                                                0
                                                    in
                                                    move s
                                                        { index = index
                                                        , dir = d
                                                        , sum = sum
                                                        , i = i1
                                                        }
                                                )
                                                others
                                            |> List.reverse
                                in
                                findMinimumHeatLossHelp array2D state appleSauce

                            Down ->
                                let
                                    appleSauce =
                                        (if i < 2 then
                                            [ Left, Right, Down ]

                                         else
                                            [ Left, Right ]
                                        )
                                            |> List.reverse
                                            |> List.foldl
                                                (\d s ->
                                                    let
                                                        i1 =
                                                            if d == Down then
                                                                i + 1

                                                            else
                                                                0
                                                    in
                                                    move s
                                                        { index = index
                                                        , dir= d
                                                        , sum = sum
                                                        , i = i1
                                                        }
                                                )
                                                others
                                            |> List.reverse
                                in
                                findMinimumHeatLossHelp array2D state appleSauce

                            Left ->
                                let
                                    appleSauce =
                                        (if i < 2 then
                                            [ Up, Down, Left ]

                                         else
                                            [ Up, Down ]
                                        )
                                            |> List.reverse
                                            |> List.foldl
                                                (\d s ->
                                                    let
                                                        i1 =
                                                            if d == Left then
                                                                i + 1

                                                            else
                                                                0
                                                    in
                                                    move s
                                                        { index = index
                                                        , dir= d
                                                        , sum = sum
                                                        , i = i1
                                                        }
                                                )
                                                others
                                            |> List.reverse
                                in
                                findMinimumHeatLossHelp array2D state appleSauce


moveInDirection =
    \array2D direction { x, y } ->
        case direction of
            Up ->
                if y == 0 then
                    Err "Yolo"

                else
                    Ok { x = x, y = y - 1 }

            Right ->
                if x >= Array2D.columns array2D - 1 then
                    Err "Yolo"

                else
                    Ok { y = y, x = x + 1 }

            Down ->
                if y >= Array2D.rows array2D - 1 then
                    Err "Yolo"

                else
                    Ok { x = x, y = y + 1 }

            Left ->
                if x == 0 then
                    Err "Yolo"

                else
                    Ok { y = y, x = x - 1 }


parseToLikeASolidArray2D =
    \input ->
        input
            |> String.trim
            |> String.split "\n"
            |> List.map (Array.fromList << String.toList)
            |> Array.fromList
            |> Array.map (\a -> 
                let
                    _ = Debug.log "" (Array.length a, a)
                in
                a)
            |> Array2D.fromRows


exampleInput =
    """2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533    
"""

