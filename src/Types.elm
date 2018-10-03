module Types exposing (Parameters, Speed(..))

import Array exposing (Array)


type Speed
    = Forward Int
    | Backward Int
    | Stopped


type alias Parameters =
    { steps : Int
    , points : Int
    , repeats : Array ( Int, Float, Speed )
    }
