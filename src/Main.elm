module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)



-- Constant


columnNum : Int
columnNum =
    100


rowNum : Int
rowNum =
    100


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


type alias Model =
    Array (Array LifeAndDeath)


init : Model
init =
    Array.repeat rowNum <| Array.repeat columnNum Death


type Msg
    = Click


update : Msg -> Model -> Model
update msg model =
    model


view : Model -> Html Msg
view model =
    table []
        [ tbody [] (body model) ]


body : Model -> List (Html Msg)
body model =
    Array.toList <| Array.map row model


row : Array LifeAndDeath -> Html Msg
row lads =
    tr [] (Array.toList <| Array.map cell lads)


cell : LifeAndDeath -> Html Msg
cell lad =
    td
        (cellStyle lad)
        []


cellStyle : LifeAndDeath -> List (Attribute Msg)
cellStyle lad =
    case lad of
        Life ->
            lifeStyle

        Death ->
            deathStyle


lifeStyle : List (Attribute Msg)
lifeStyle =
    [ style "border" "1px solid #333"
    , style "width" "5px"
    , style "height" "5px"
    , style "background-color" "#333"
    ]


deathStyle : List (Attribute Msg)
deathStyle =
    [ style "border" "1px solid #333"
    , style "width" "5px"
    , style "height" "5px"
    ]
