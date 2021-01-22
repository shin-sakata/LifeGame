module Main exposing (main)

import Array exposing (Array)
import Browser
import Core exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Matrics exposing (Matrics(..), Point)
import Random



-- Constant


size : Int
size =
    100


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    Matrics CellularState


init : () -> ( Model, Cmd Msg )
init _ =
    ( allDead, Cmd.none )


allDead : Matrics CellularState
allDead =
    Matrics.initialize size (always Dead)



-- Update


type Msg
    = Click Point
    | NextGen
    | CreateRandomModel
    | Generated (Matrics CellularState)
    | Clear


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click p ->
            ( flipPoint p model, Cmd.none )

        NextGen ->
            ( nextGen model, Cmd.none )

        CreateRandomModel ->
            ( model, Random.generate Generated randomCellularStateMatrics )

        Generated matrics ->
            ( matrics, Cmd.none )

        Clear ->
            ( allDead, Cmd.none )


flipPoint : Point -> Matrics CellularState -> Matrics CellularState
flipPoint point matrics =
    Matrics.update point flip matrics


randomCellularStateMatrics : Random.Generator (Matrics CellularState)
randomCellularStateMatrics =
    Random.uniform Dead [ Dead, Dead, Alive ]
        |> Random.list size
        |> Random.map Array.fromList
        |> Random.list size
        |> Random.map Array.fromList
        |> Random.map Matrics.pure



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    div []
        [ table [ style "border-collapse" "collapse" ]
            [ tbody [] (body model) ]
        , button [ onClick NextGen ] [ text "Next Generation" ]
        , button [ onClick CreateRandomModel ] [ text "Random" ]
        , button [ onClick Clear ] [ text "Clear" ]
        ]


body : Matrics CellularState -> List (Html Msg)
body (Matrics matrics) =
    Array.toList <| Array.indexedMap row matrics


row : Int -> Array CellularState -> Html Msg
row rindex lads =
    tr [] (Array.toList <| Array.indexedMap (cell rindex) lads)


cell : Int -> Int -> CellularState -> Html Msg
cell rindex cindex lad =
    td
        (onClick (Click ( rindex, cindex )) :: cellStyle lad)
        []


cellStyle : CellularState -> List (Attribute Msg)
cellStyle lad =
    case lad of
        Dead ->
            deadStyle

        Alive ->
            aliveStyle


aliveStyle : List (Attribute Msg)
aliveStyle =
    baseCellStyle [ style "background-color" "lightgreen" ]


deadStyle : List (Attribute Msg)
deadStyle =
    baseCellStyle [ style "background-color" "gray" ]


baseCellStyle : List (Attribute msg) -> List (Attribute msg)
baseCellStyle attr =
    [ style "border" "1px solid gray"
    , style "width" "6px"
    , style "height" "6px"
    ]
        ++ attr
