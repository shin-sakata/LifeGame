module Main exposing (main)

import Array exposing (Array)
import Array.Extra as AE
import Browser
import Core exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random



-- Constant


size : Int
size =
    75


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- Model


init : () -> ( Model, Cmd Msg )
init _ =
    ( Array.initialize size <| always (Array.initialize size (always Death)), Cmd.none )



-- Update


type Msg
    = Click Point
    | NextGen
    | CreateRandomModel
    | RandomModel (List (List LifeAndDeath))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click p ->
            ( flipPoint p model, Cmd.none )

        NextGen ->
            ( nextGen model, Cmd.none )

        CreateRandomModel ->
            ( model, Random.generate RandomModel randomModel )

        RandomModel rModel ->
            ( Array.fromList <| List.map Array.fromList rModel, Cmd.none )


flipPoint : Point -> Model -> Model
flipPoint ( ri, ci ) model =
    AE.update ri (AE.update ci flip) model


randomModel : Random.Generator (List (List LifeAndDeath))
randomModel =
    Random.list size <| Random.list size (Random.uniform Death [ Death, Death, Life ])



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

        -- , textarea [ onKeyPress KeyDown ] []
        ]


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
