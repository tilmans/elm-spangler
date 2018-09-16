module Main exposing (main)

import Browser
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Flags =
    {}


type alias Model =
    List Int


type Msg
    = NoOp


init flags =
    ( [], Cmd.none )


update msg model =
    ( model, Cmd.none )


subscriptions model =
    Sub.none


view model =
    svg [ width "100%", height "100%" ] [ circle [ cx "50", cy "50", r "50" ] [] ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
