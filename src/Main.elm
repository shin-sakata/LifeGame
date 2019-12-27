module Main exposing (main)

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
    List (List LifeAndDeath)


init : Model
init =
    List.repeat rowNum <| List.repeat columnNum Death


type Msg
    = Msg


update : Msg -> Model -> Model
update msg model =
    model


view model =
    table []
        [ tbody [] (body model) ]


body : Model -> List (Html Msg)
body model =
    List.map row model


row : List LifeAndDeath -> Html Msg
row lads =
    tr [] (List.map cell lads)


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


lifeStyle =
    [ style "border" "1px solid #333"
    , style "width" "5px"
    , style "height" "5px"
    , style "background-color" "#333"
    ]


deathStyle =
    [ style "border" "1px solid #333"
    , style "width" "5px"
    , style "height" "5px"
    ]
