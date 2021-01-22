module Core exposing (..)

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)
import Matrics exposing (Matrics(..), Point)
import Tuple exposing (pair)


type CellularState
    = Alive
    | Dead


flip : CellularState -> CellularState
flip cell =
    case cell of
        Alive ->
            Dead

        Dead ->
            Alive


nextGen : Matrics CellularState -> Matrics CellularState
nextGen matrics =
    let
        zipCountMap : Matrics ( Int, CellularState )
        zipCountMap =
            Matrics.map2 pair (countAround matrics) matrics
    in
    Matrics.map nextGenPoint zipCountMap


nextGenPoint : ( Int, CellularState ) -> CellularState
nextGenPoint ( count, state ) =
    case state of
        Alive ->
            case count of
                2 ->
                    Alive

                3 ->
                    Alive

                _ ->
                    Dead

        Dead ->
            case count of
                3 ->
                    Alive

                _ ->
                    Dead


countAround : Matrics CellularState -> Matrics Int
countAround (Matrics matrics) =
    Matrics
        (Array.indexedMap
            (\x row ->
                Array.indexedMap
                    (\y _ -> countLife <| Array.map (getCellularState (Matrics matrics)) (( x, y ) |> aroundPoint))
                    row
            )
            matrics
        )


countLife : Array CellularState -> Int
countLife lads =
    Array.length <| Array.filter ((==) Alive) lads


getCellularState : Matrics CellularState -> Point -> CellularState
getCellularState (Matrics matrics) ( x, y ) =
    case Array.get x matrics of
        Nothing ->
            Dead

        Just row ->
            case Array.get y row of
                Just v ->
                    v

                Nothing ->
                    Dead


aroundPoint : Point -> Array Point
aroundPoint ( x, y ) =
    Array.fromList
        [ ( x - 1, y - 1 )
        , ( x - 1, y )
        , ( x - 1, y + 1 )
        , ( x, y - 1 )
        , ( x, y + 1 )
        , ( x + 1, y - 1 )
        , ( x + 1, y )
        , ( x + 1, y + 1 )
        ]
