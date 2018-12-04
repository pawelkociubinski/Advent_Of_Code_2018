module Day02.Inventory_Management_System exposing (Model, init)

import Day02.Input exposing (input)
import Dict exposing (Dict)
import Html exposing (Html, div, text)


updatefunc item =
    case item of
        Nothing ->
            Just 1

        Just i ->
            Just (i + 1)


rec dict arr =
    case arr of
        [] ->
            dict

        head :: tail ->
            if Dict.member head dict then
                rec (Dict.update head updatefunc dict) tail

            else
                rec (Dict.insert head 1 dict) tail


incrament : Int -> Int
incrament value =
    value + 1


isNotEmpty : String -> Bool
isNotEmpty value =
    not (String.isEmpty value)


rec2 : ( Int, Int ) -> List (List Int) -> ( Int, Int )
rec2 tuple arr =
    case arr of
        [] ->
            tuple

        head :: tail ->
            let
                hasTwo =
                    List.member 2 head

                hasThree =
                    List.member 3 head
            in
            if hasTwo && hasThree then
                rec2 (Tuple.mapBoth incrament incrament tuple) tail

            else if hasTwo then
                rec2 (Tuple.mapFirst incrament tuple) tail

            else if hasThree then
                rec2 (Tuple.mapSecond incrament tuple) tail

            else
                rec2 tuple tail


getOccurrences : List (List Int) -> ( Int, Int )
getOccurrences arr =
    let
        defaultValue =
            ( 0, 0 )
    in
    rec2 defaultValue arr


getChecksum : ( Int, Int ) -> Int
getChecksum occurrences =
    Tuple.first occurrences * Tuple.second occurrences



-- Filter out values other then value two and value three


filterout : comparable -> Int -> Bool
filterout _ value =
    value == 2 || value == 3


iteration : String -> List Int
iteration item =
    item
        |> String.split ""
        |> rec Dict.empty
        |> Dict.filter filterout
        |> Dict.values


qwert : List String -> List (List Int)
qwert arr =
    List.map iteration arr


format : String -> List String
format inputs =
    inputs
        |> String.split "\n"
        |> List.map String.trim
        |> List.filter isNotEmpty



-- MODEL


type alias Model =
    { checksum : Int
    , common : String
    }


init : Model
init =
    let
        checksum =
            input
                |> format
                |> qwert
                |> getOccurrences
                |> getChecksum
    in
    Model checksum ""
