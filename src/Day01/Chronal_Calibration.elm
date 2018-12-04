module Day01.Chronal_Calibration exposing (Model, init)

import Day01.Input exposing (input)
import Html exposing (Html, div, text)
import Set exposing (Set)


type alias Model =
    { sum : Int
    , dubled : Int
    }


init : Model
init =
    let
        sum =
            input
                |> format
                |> List.sum

        dubled =
            input
                |> format
                |> repeatedFrequency
    in
    Model sum dubled


findRepeatingSum : Int -> Set Int -> List Int -> List Int -> Int
findRepeatingSum sum seen nums allNums =
    case nums of
        [] ->
            findRepeatingSum sum seen allNums allNums

        head :: tail ->
            let
                summm =
                    sum + head
            in
            if Set.member summm seen then
                summm

            else
                findRepeatingSum summm (Set.insert summm seen) tail allNums


repeatedFrequency : List Int -> Int
repeatedFrequency input =
    findRepeatingSum 0 Set.empty input input


isNotEmpty : String -> Bool
isNotEmpty value =
    not (String.isEmpty value)


toInt : String -> Int
toInt item =
    item
        |> String.toInt
        |> Maybe.withDefault 0


format : String -> List Int
format input =
    input
        |> String.split "\n"
        |> List.map String.trim
        |> List.filter isNotEmpty
        |> List.map toInt
