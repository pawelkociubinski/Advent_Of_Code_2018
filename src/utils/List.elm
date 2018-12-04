module Utils exposing (format)


format : String -> Int | String -> List Int
format array default =
    array
        |> String.split "\n"
        |> List.map String.trim
        |> List.filter (\i -> not (String.isEmpty i))
        |> List.map (\i -> String.toInt i |> Maybe.withDefault default)
