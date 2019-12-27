module Main exposing (main)

import Array exposing (Array)
import Array.Extra as AE
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)



-- Constant


size : Int
size =
    50


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- Model


type LifeAndDeath
    = Life
    | Death
    | None


flip : LifeAndDeath -> LifeAndDeath
flip lad =
    case lad of
        Life ->
            Death

        Death ->
            Life

        None ->
            None


type alias Model =
    Array (Array LifeAndDeath)


type alias Point =
    ( Int, Int )


init : Model
init =
    Array.initialize size <| always (Array.initialize size (always Death))


type Msg
    = Click Point


click : Point -> Model -> Model
click ( ri, ci ) model =
    AE.update ri (AE.update ci flip) model


update : Msg -> Model -> Model
update msg model =
    case msg of
        Click p ->
            click p model


view : Model -> Html Msg
view model =
    table [ style "border-collapse" "collapse" ]
        [ tbody [] (body model) ]


body : Model -> List (Html Msg)
body model =
    Array.toList <| Array.indexedMap row model


row : Int -> Array LifeAndDeath -> Html Msg
row rindex lads =
    tr [] (Array.toList <| Array.indexedMap (cell rindex) lads)


cell : Int -> Int -> LifeAndDeath -> Html Msg
cell rindex cindex lad =
    td
        (onClick (Click ( rindex, cindex )) :: cellStyle lad)
        []


cellStyle : LifeAndDeath -> List (Attribute Msg)
cellStyle lad =
    case lad of
        Life ->
            lifeStyle

        Death ->
            deathStyle

        None ->
            deathStyle


lifeStyle : List (Attribute Msg)
lifeStyle =
    [ style "border" "1px solid #333"
    , style "width" "10px"
    , style "height" "10px"
    , style "background-color" "#333"
    ]


deathStyle : List (Attribute Msg)
deathStyle =
    [ style "border" "1px solid #333"
    , style "width" "10px"
    , style "height" "10px"
    ]
