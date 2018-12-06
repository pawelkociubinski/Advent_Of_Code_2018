module Day03.No_Matter_How_You_Slice_It exposing (Model, init)

import Day03.Input exposing (puzzleInput)


parse : List String -> List Rect
parse =
    let
        rectValues =
            submatches "#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)"
                >> List.filterMap identity
                >> List.map toInt

        parseLine line =
            case rectValues line of
                id :: x :: y :: w :: h :: [] ->
                    Rect id x y w h

                _ ->
                    Rect 0 0 0 0 0
    in
    List.map parseLine

format : String -> PuzzleInput
format puzzleInput =
    puzzleInput
        |> String.words
        |> List.map String.trim
        |> List.filter isNotEmpty
        |> List.map toInt


-- MODEL


type alias Rect =
    { id : Int
    , x : Int
    , y : Int
    , width : Int
    , hight : Int
    }


type alias Model =
    { sumOfSharedClaims : Int
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

    Model sumOfFrequencies
