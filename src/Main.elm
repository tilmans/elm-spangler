module Main exposing (main)

import Array
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Fractal
import Html


type alias Flags =
    {}


type alias Model =
    { steps : Int
    , points : Int
    , repeats : List Int
    }


type Msg
    = SetPoints Float
    | SetRepeat Int Float


init flags =
    ( Model 1 5 [ 5, 4 ], Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPoints p ->
            ( { model | points = floor p }, Cmd.none )

        SetRepeat position value ->
            ( { model
                | repeats =
                    model.repeats
                        |> Array.fromList
                        |> Array.set position (floor value)
                        |> Array.toList
              }
            , Cmd.none
            )


subscriptions model =
    Sub.none


view model =
    Element.layout []
        (row []
            [ column [ width (px 500) ] [ html (Fractal.draw model.points model.repeats) ]
            , column []
                (Input.slider []
                    { onChange = SetPoints
                    , label = Input.labelAbove [] (text "Number of Points")
                    , min = 3
                    , max = 15
                    , step = Just 1
                    , value = toFloat model.points
                    , thumb = Input.defaultThumb
                    }
                    :: List.indexedMap
                        (\i r ->
                            let
                                msg : Float -> Msg
                                msg =
                                    SetRepeat i
                            in
                            Input.slider []
                                { onChange = msg
                                , label = Input.labelAbove [] (text ("Repeat " ++ String.fromInt i))
                                , min = 1
                                , max = 15
                                , step = Just 1
                                , value = toFloat r
                                , thumb = Input.defaultThumb
                                }
                        )
                        model.repeats
                )
            ]
        )


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
