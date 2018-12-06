module Day02.Inventory_Management_System exposing (Model, init)

import Day02.Input exposing (puzzleInput)
import Dict exposing (Dict)
import Set exposing (Set)


incrament : Int -> Int
incrament value =
    value + 1


findOccurrencesFrequency : ( Int, Int ) -> List (List Int) -> ( Int, Int )
findOccurrencesFrequency tuple arr =
    case arr of
        [] ->
            tuple

        head :: tail ->
            if List.member 2 head && List.member 3 head then
                findOccurrencesFrequency (Tuple.mapBoth incrament incrament tuple) tail

            else if List.member 2 head then
                findOccurrencesFrequency (Tuple.mapFirst incrament tuple) tail

            else if List.member 3 head then
                findOccurrencesFrequency (Tuple.mapSecond incrament tuple) tail

            else
                findOccurrencesFrequency tuple tail


type alias OccurrencesFrequency =
    ( Int, Int )


getOccurrencesFrequency : List Occurrences -> OccurrencesFrequency
getOccurrencesFrequency arr =
    findOccurrencesFrequency ( 0, 0 ) arr


type alias Checksum =
    Int


getChecksum : OccurrencesFrequency -> Checksum
getChecksum occurrences =
    Tuple.first occurrences * Tuple.second occurrences


updateDict : Maybe Int -> Maybe Int
updateDict item =
    case item of
        Nothing ->
            Just 1

        Just i ->
            Just (i + 1)


findLetterOccurrences : Dict Char Int -> List Char -> Dict Char Int
findLetterOccurrences dict arr =
    case arr of
        [] ->
            dict

        head :: tail ->
            if Dict.member head dict then
                findLetterOccurrences (Dict.update head updateDict dict) tail

            else
                findLetterOccurrences (Dict.insert head 1 dict) tail


getLetterOccurrences : List Char -> Dict Char Int
getLetterOccurrences letters =
    findLetterOccurrences Dict.empty letters


filter : comparable -> Int -> Bool
filter _ value =
    value == 2 || value == 3


findOccurrences : String -> Occurrences
findOccurrences id =
    id
        |> String.toList
        |> getLetterOccurrences
        |> Dict.filter filter
        |> Dict.values


type alias Occurrences =
    List Int


getOccurrences : PuzzleInput -> List Occurrences
getOccurrences puzzleInput =
    List.map findOccurrences puzzleInput


trimByPosition : Int -> String -> String
trimByPosition position sentence =
    String.slice 0 position sentence ++ String.dropLeft (position + 1) sentence


findCommonLetter : Int -> Set String -> String -> List String -> List String -> Set String
findCommonLetter position result pattern arr collection =
    case arr of
        [] ->
            if position == String.length pattern then
                result

            else
                findCommonLetter (position + 1) result pattern collection collection

        head :: tail ->
            if trimByPosition position pattern == trimByPosition position head then
                Set.insert (trimByPosition position pattern) result

            else
                findCommonLetter position result pattern tail collection


findCommonLetters : Set String -> PuzzleInput -> PuzzleInput -> Set String
findCommonLetters result arr collection =
    case arr of
        [] ->
            result

        head :: tail ->
            findCommonLetters (findCommonLetter 0 result head tail tail) tail collection


getCommonLetters : PuzzleInput -> Set String
getCommonLetters puzzleInput =
    findCommonLetters Set.empty puzzleInput puzzleInput


isNotEmpty : String -> Bool
isNotEmpty value =
    not (String.isEmpty value)


format : String -> PuzzleInput
format puzzleInput =
    puzzleInput
        |> String.words
        |> List.map String.trim
        |> List.filter isNotEmpty



-- Model


type alias Model =
    { checksum : Int
    , commonLetters : CommonsList
    }


type alias CommonsList =
    List String


type alias PuzzleInput =
    List String


init : Model
init =
    let
        checksum =
            puzzleInput
                |> format
                |> getOccurrences
                |> getOccurrencesFrequency
                |> getChecksum

        commonLetters =
            puzzleInput
                |> format
                |> getCommonLetters
                |> Set.toList
    in
    Model checksum commonLetters
