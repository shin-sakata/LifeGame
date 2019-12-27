module Core exposing (..)

import Array exposing (Array)
import Array.Extra as AE
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type LifeAndDeath
    = Life
    | Death
    | None


type alias Point =
    ( Int, Int )


type alias Model =
    Array (Array LifeAndDeath)


flip : LifeAndDeath -> LifeAndDeath
flip lad =
    case lad of
        Life ->
            Death

        Death ->
            Life

        None ->
            None


nextGen : Model -> Model
nextGen model =
    let
        zipCountMap : Array (Array ( Int, LifeAndDeath ))
        zipCountMap =
            AE.map2 (\counts lads -> AE.zip counts lads) (aroundCountMap model) model
    in
    Array.map (\maps -> Array.map nextGenPoint maps) zipCountMap


nextGenPoint : ( Int, LifeAndDeath ) -> LifeAndDeath
nextGenPoint ( count, state ) =
    case state of
        Life ->
            case count of
                2 ->
                    Life

                3 ->
                    Life

                _ ->
                    Death

        Death ->
            case count of
                3 ->
                    Life

                _ ->
                    Death

        None ->
            None


aroundCountMap : Model -> Array (Array Int)
aroundCountMap model =
    Array.indexedMap
        (\x row ->
            Array.indexedMap
                (\y _ -> countLife <| Array.map (getLifeAndDead model) (( x, y ) |> aroundPoint))
                row
        )
        model


countLife : Array LifeAndDeath -> Int
countLife lads =
    Array.length <| Array.filter ((==) Life) lads


getLifeAndDead : Model -> Point -> LifeAndDeath
getLifeAndDead model ( x, y ) =
    case Array.get x model of
        Nothing ->
            Death

        Just row ->
            case Array.get y row of
                Just v ->
                    v

                Nothing ->
                    None


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
