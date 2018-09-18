module Main exposing (main)

import Array
import Browser
import Browser.Events
import Css exposing (..)
import Fractal
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode


type alias Flags =
    {}


type ActiveItem
    = Steps
    | Points
    | Repeat Int
    | New


type alias Model =
    { steps : Int
    , points : Int
    , repeats : Array.Array Int
    , active : ActiveItem
    }


type Msg
    = SetPoints Float
    | SetRepeat Int Float
    | KeyEvent Key


type Key
    = Left
    | Right
    | Up
    | Down
    | Other


toKey : String -> Key
toKey string =
    case string of
        "ArrowLeft" ->
            Left

        "ArrowRight" ->
            Right

        "ArrowUp" ->
            Up

        "ArrowDown" ->
            Down

        _ ->
            Other


init flags =
    ( Model 1 5 (Array.fromList [ 5, 4 ]) Steps, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPoints p ->
            ( { model | points = floor p }, Cmd.none )

        SetRepeat position value ->
            ( { model
                | repeats =
                    model.repeats
                        |> Array.set position (floor value)
              }
            , Cmd.none
            )

        KeyEvent key ->
            let
                newModel =
                    case key of
                        Left ->
                            { model
                                | active =
                                    case model.active of
                                        Steps ->
                                            Steps

                                        Points ->
                                            Steps

                                        Repeat pos ->
                                            if pos > 0 then
                                                Repeat (pos - 1)

                                            else
                                                Points

                                        New ->
                                            Repeat (Array.length model.repeats - 1)
                            }

                        Right ->
                            { model
                                | active =
                                    case model.active of
                                        Steps ->
                                            Points

                                        Points ->
                                            Repeat 0

                                        Repeat pos ->
                                            if pos >= (Array.length model.repeats - 1) then
                                                New

                                            else
                                                Repeat (pos + 1)

                                        New ->
                                            New
                            }

                        Up ->
                            case model.active of
                                Steps ->
                                    { model
                                        | steps = incrementWithLimit model.steps
                                    }

                                Points ->
                                    { model
                                        | points = incrementWithLimit model.points
                                    }

                                Repeat index ->
                                    let
                                        current =
                                            Array.get index model.repeats
                                                |> Maybe.withDefault 0

                                        add =
                                            incrementWithLimit current

                                        new =
                                            Array.set index add model.repeats
                                    in
                                    { model | repeats = new }

                                _ ->
                                    model

                        Down ->
                            case model.active of
                                Steps ->
                                    { model
                                        | steps = decrementWithLimit model.steps
                                    }

                                Points ->
                                    { model
                                        | points = decrementWithLimit model.points
                                    }

                                Repeat index ->
                                    let
                                        current =
                                            Array.get index model.repeats
                                                |> Maybe.withDefault 0

                                        add =
                                            decrementWithLimit current

                                        new =
                                            Array.set index add model.repeats
                                    in
                                    { model | repeats = new }

                                _ ->
                                    model

                        _ ->
                            model
            in
            ( newModel, Cmd.none )


incrementWithLimit value =
    if value < 9 then
        value + 1

    else
        value


decrementWithLimit value =
    if value > 1 then
        value - 1

    else
        value


subscriptions model =
    Browser.Events.onKeyDown (Decode.map KeyEvent (Decode.map toKey keyDecoder))


keyDecoder =
    Decode.field "key" Decode.string


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ div [ class "controls-container" ]
            [ div [ class "controls" ] (inputControls model)
            ]
        , div [ class "fractal-container" ]
            [ div [ class "fractal" ] [ Fractal.draw model.points (Array.toList model.repeats) ]
            ]
        ]


inputControls model =
    [ control (model.active == Steps) (Just model.steps)
    , control (model.active == Points) (Just model.points)
    ]
        ++ List.indexedMap (\index value -> control (valueIsRepeat model.active index) (Just value)) (Array.toList model.repeats)
        ++ [ control (model.active == New) Nothing ]


valueIsRepeat : ActiveItem -> Int -> Bool
valueIsRepeat expected current =
    case expected of
        Repeat index ->
            index == current

        _ ->
            False


control : Bool -> Maybe Int -> Html Msg
control active value =
    div
        [ class
            (if active then
                "active-control"

             else
                "passive-control"
            )
        ]
        [ div [ class "control-text" ]
            [ case value of
                Just hasValue ->
                    text (String.fromInt hasValue)

                Nothing ->
                    text ""
            ]
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
