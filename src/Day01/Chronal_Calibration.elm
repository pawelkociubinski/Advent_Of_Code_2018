module Day01.Chronal_Calibration exposing (Model, init)

import Day01.Input exposing (puzzleInput)
import Html exposing (Html, div, text)
import Set exposing (Set)


findRepeatedFrequency : Int -> Set Int -> PuzzleInput -> PuzzleInput -> Int
findRepeatedFrequency sum calculatedFrequencies vertexes collection =
    case vertexes of
        [] ->
            findRepeatedFrequency sum calculatedFrequencies collection collection

        head :: tail ->
            if Set.member (sum + head) calculatedFrequencies then
                sum + head

            else
                findRepeatedFrequency (sum + head) (Set.insert (sum + head) calculatedFrequencies) tail collection



getRepeatedFrequency : PuzzleInput -> Int
getRepeatedFrequency puzzleInput =
    findRepeatedFrequency 0 Set.empty puzzleInput puzzleInput


isNotEmpty : String -> Bool
isNotEmpty value =
    not (String.isEmpty value)


toInt : String -> Int
toInt value =
    value
        |> String.toInt
        |> Maybe.withDefault 0


format : String -> PuzzleInput
format puzzleInput =
    puzzleInput
        |> String.words
        |> List.map String.trim
        |> List.filter isNotEmpty
        |> List.map toInt



-- MODEL


type alias Model =
    { sumOfFrequencies : Int
    , repeatedFrequency : Int
    }


type alias PuzzleInput =
    List Int


init : Model
init =
    let
        sumOfFrequencies =
            puzzleInput
                |> format
                |> List.sum

        repeatedFrequency =
            puzzleInput
                |> format
                |> getRepeatedFrequency
    in
    Model sumOfFrequencies repeatedFrequency

myfunc : Int -> Int -> Int
myfunc a b =
a + b

myfunca 1 2