module Main exposing (main)

import Browser
import Day01.Chronal_Calibration as ChronalCalibration
import Day02.Inventory_Management_System as InventoryManagementSystem
import Html exposing (Html, div, text)
import Set exposing (Set)



-- MODEL


type alias Model =
    { chronalCalibration : ChronalCalibration.Model
    , inventoryManagementSystem : InventoryManagementSystem.Model
    }


initialModel : Model
initialModel =
    { chronalCalibration = ChronalCalibration.init
    , inventoryManagementSystem = InventoryManagementSystem.init
    }



-- UPDATE


type Msg
    = None


update : Msg -> Model -> Model
update msg model =
    model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text <| String.fromInt model.chronalCalibration.sum ]
        , div [] [ text <| String.fromInt model.chronalCalibration.dubled ]
        , div [] [ text <| String.fromInt model.inventoryManagementSystem.checksum ]
        ]



-- PROGRAM


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
