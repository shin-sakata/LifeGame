module Matrics exposing (..)

import Array exposing (Array)
import Array.Extra as AE



-- MatricsはArrayのArrayを表現していて、LifeGame行列を表すことができる


type Matrics a
    = Matrics (Array (Array a))


type alias Point =
    ( Int, Int )


run : Matrics a -> Array (Array a)
run (Matrics value) =
    value


pure : Array (Array a) -> Matrics a
pure =
    Matrics


map : (a -> b) -> Matrics a -> Matrics b
map f (Matrics matrics) =
    Matrics ((Array.map >> Array.map) f matrics)


map2 : (a -> b -> c) -> Matrics a -> Matrics b -> Matrics c
map2 f (Matrics m1) (Matrics m2) =
    Matrics ((AE.map2 >> AE.map2) f m1 m2)


update : Point -> (a -> a) -> Matrics a -> Matrics a
update ( r, c ) f (Matrics matrics) =
    Matrics (AE.update r (AE.update c f) matrics)


type alias Size =
    Int


type alias Index =
    Int


initialize : Size -> (Index -> a) -> Matrics a
initialize size f =
    Matrics (Array.initialize size <| always (Array.initialize size f))
