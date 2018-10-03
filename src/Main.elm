module Main exposing (main)

import Array
import Browser
import Browser.Events
import Browser.Navigation
import Color exposing (Color)
import Colors
import Dict exposing (Dict)
import Fractal
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode
import ParamParser as Parser
import Types exposing (Parameters, Speed)
import Url exposing (Url)


default =
    { steps = 1, points = 3, repeats = Array.fromList [ ( 3, 1, Types.Stopped ) ] }


type alias Model =
    { key : Browser.Navigation.Key
    , parameters : Parameters
    , active : ActiveItem
    , colors : List String
    , colored : Int
    }


type Msg
    = KeyEvent Key
    | UrlChange Url
    | UrlRequest
    | Tick Float


type alias Flags =
    {}


type ActiveItem
    = Steps
    | Points
    | Repeat Int
    | New


type Key
    = Left
    | Right
    | Up
    | Down
    | Back
    | Pause
    | Forward
    | Other


type SpeedChange
    = Increment
    | Decrement
    | Stop


deltaFraction =
    100


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick delta ->
            let
                newRepeats =
                    Array.map
                        (\( repeat, time, speed ) ->
                            let
                                newTime =
                                    case speed of
                                        Types.Forward s ->
                                            time + delta * toFloat s / deltaFraction

                                        Types.Backward s ->
                                            time - delta * toFloat s / deltaFraction

                                        Types.Stopped ->
                                            time
                            in
                            ( repeat, newTime, speed )
                        )
                        model.parameters.repeats

                oldParameters =
                    model.parameters

                newParameters =
                    { oldParameters | repeats = newRepeats }
            in
            ( { model
                | parameters = newParameters
              }
            , Cmd.none
            )

        UrlChange url ->
            ( { model | parameters = parametersFromUrl url (Just model.parameters) }
            , Cmd.none
            )

        UrlRequest ->
            ( model, Cmd.none )

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
                                            if Array.length model.parameters.repeats == 0 then
                                                Points

                                            else
                                                Repeat (Array.length model.parameters.repeats - 1)
                            }

                        Right ->
                            { model
                                | active =
                                    case model.active of
                                        Steps ->
                                            Points

                                        Points ->
                                            if Array.length model.parameters.repeats == 0 then
                                                New

                                            else
                                                Repeat 0

                                        Repeat pos ->
                                            if pos >= (Array.length model.parameters.repeats - 1) then
                                                New

                                            else
                                                Repeat (pos + 1)

                                        New ->
                                            New
                            }

                        Up ->
                            case model.active of
                                Steps ->
                                    let
                                        oldP =
                                            model.parameters

                                        newP =
                                            { oldP | steps = incrementWithLimit oldP.steps }
                                    in
                                    { model
                                        | parameters = newP
                                    }

                                Points ->
                                    let
                                        oldP =
                                            model.parameters

                                        newP =
                                            { oldP | points = incrementWithLimit oldP.points }
                                    in
                                    { model
                                        | parameters = newP
                                    }

                                Repeat index ->
                                    let
                                        oldP =
                                            model.parameters

                                        ( repeat, time, speed ) =
                                            model.parameters.repeats
                                                |> Array.get index
                                                |> Maybe.withDefault ( 0, 0, Types.Stopped )

                                        add =
                                            incrementWithLimit repeat

                                        new =
                                            Array.set index ( add, time, speed ) model.parameters.repeats

                                        newP =
                                            { oldP | repeats = new }
                                    in
                                    { model | parameters = newP }

                                New ->
                                    let
                                        oldP =
                                            model.parameters

                                        newP =
                                            { oldP
                                                | repeats =
                                                    Array.push ( 1, 0, Types.Stopped )
                                                        oldP.repeats
                                            }
                                    in
                                    { model
                                        | parameters = newP
                                        , active = Repeat (Array.length newP.repeats - 1)
                                    }

                        Down ->
                            case model.active of
                                Steps ->
                                    let
                                        oldP =
                                            model.parameters

                                        newP =
                                            { oldP | steps = decrementWithLimit oldP.steps }
                                    in
                                    { model | parameters = newP }

                                Points ->
                                    let
                                        oldP =
                                            model.parameters

                                        newP =
                                            { oldP | points = decrementWithLimit oldP.points }
                                    in
                                    { model | parameters = newP }

                                Repeat index ->
                                    let
                                        oldP =
                                            model.parameters

                                        lastIndex =
                                            Array.length oldP.repeats - 1

                                        ( steps, time, speed ) =
                                            Array.get index oldP.repeats
                                                |> Maybe.withDefault ( 0, 0, Types.Stopped )

                                        ( newRepeat, newPosition ) =
                                            if steps == 1 && index == lastIndex then
                                                ( Array.slice 0 lastIndex oldP.repeats
                                                , if lastIndex - 1 >= 0 then
                                                    Repeat (lastIndex - 1)

                                                  else
                                                    Points
                                                )

                                            else
                                                ( Array.set index ( decrementWithLimit steps, time, speed ) oldP.repeats
                                                , Repeat index
                                                )

                                        newP =
                                            { oldP | repeats = newRepeat }
                                    in
                                    { model | parameters = newP, active = newPosition }

                                New ->
                                    model

                        Back ->
                            case model.active of
                                Repeat index ->
                                    { model
                                        | parameters =
                                            changeSpeed model.parameters index Decrement
                                    }

                                _ ->
                                    model

                        Forward ->
                            case model.active of
                                Repeat index ->
                                    { model
                                        | parameters =
                                            changeSpeed model.parameters index Increment
                                    }

                                _ ->
                                    model

                        Pause ->
                            case model.active of
                                Repeat index ->
                                    { model
                                        | parameters =
                                            changeSpeed model.parameters index Stop
                                    }

                                _ ->
                                    model

                        Other ->
                            model
            in
            ( newModel, setURL newModel )


changeSpeed : Parameters -> Int -> SpeedChange -> Parameters
changeSpeed parameters index delta =
    let
        ( steps, time, speed ) =
            Array.get index parameters.repeats |> Maybe.withDefault ( 0, 0, Types.Stopped )

        newRepeats =
            Array.set index
                ( steps
                , time
                , case delta of
                    Increment ->
                        case speed of
                            Types.Forward s ->
                                Types.Forward (s + 1)

                            Types.Backward s ->
                                if s == 1 then
                                    Types.Stopped

                                else
                                    Types.Backward (s - 1)

                            Types.Stopped ->
                                Types.Forward 1

                    Decrement ->
                        case speed of
                            Types.Forward s ->
                                if s == 1 then
                                    Types.Stopped

                                else
                                    Types.Forward (s - 1)

                            Types.Backward s ->
                                Types.Backward (s + 1)

                            Types.Stopped ->
                                Types.Backward 1

                    Stop ->
                        Types.Stopped
                )
                parameters.repeats
    in
    { parameters | repeats = newRepeats }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown (Decode.map KeyEvent (Decode.map toKey keyDecoder))
        , Browser.Events.onAnimationFrameDelta Tick
        ]


view : Model -> Browser.Document Msg
view model =
    { title = Parser.toString model.parameters
    , body =
        [ div [ class "main" ]
            [ div [ class "controls-container" ]
                [ div [ class "controls" ] (inputControls model)
                ]
            , div [ class "fractal-container" ]
                [ div [ class "fractal" ]
                    [ Fractal.draw
                        model.parameters
                        model.colors
                    ]
                ]
            ]
        ]
    }


initModel : Browser.Navigation.Key -> Parameters -> Model
initModel key parameters =
    { key = key
    , parameters = parameters
    , active = Steps
    , colors = Colors.phoenix
    , colored = 0
    }


main : Program Flags Model Msg
main =
    Browser.application
        { init =
            \flags url key ->
                let
                    params =
                        parametersFromUrl url Nothing
                in
                ( initModel key params
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = \url -> UrlRequest
        , onUrlChange = \url -> UrlChange url
        }



-- UI Functions


control : Bool -> Speed -> Maybe Int -> Html Msg
control active speed value =
    div
        [ class "control"
        , classList [ ( "active", active ) ]
        ]
        [ div [ class "control-text" ]
            [ case value of
                Just hasValue ->
                    text (String.fromInt hasValue)

                Nothing ->
                    text ""
            ]
        ]


inputControls : Model -> List (Html Msg)
inputControls model =
    let
        repeatControls =
            Array.indexedMap
                (\index ( steps, time, speed ) ->
                    control (valueIsRepeat model.active index)
                        speed
                        (Just steps)
                )
                model.parameters.repeats
    in
    [ control (model.active == Steps) Types.Stopped (Just model.parameters.steps)
    , control (model.active == Points) Types.Stopped (Just model.parameters.points)
    ]
        ++ (repeatControls |> Array.toList)
        ++ [ control (model.active == New) Types.Stopped Nothing ]



-- Utilities


keyDecoder =
    Decode.field "key" Decode.string


toKey : String -> Key
toKey string =
    case string of
        --"Enter" ->
        --    AnimateToggle
        "ArrowLeft" ->
            Left

        "ArrowRight" ->
            Right

        "ArrowUp" ->
            Up

        "ArrowDown" ->
            Down

        "j" ->
            Back

        "k" ->
            Pause

        "l" ->
            Forward

        _ ->
            Other


setURL : Model -> Cmd msg
setURL model =
    Browser.Navigation.replaceUrl model.key ("#" ++ Parser.toString model.parameters)


incrementWithLimit value =
    if value < 99 then
        value + 1

    else
        value


decrementWithLimit value =
    if value > 1 then
        value - 1

    else
        value


valueIsRepeat : ActiveItem -> Int -> Bool
valueIsRepeat expected current =
    case expected of
        Repeat index ->
            index == current

        _ ->
            False


parametersFromUrl : Url -> Maybe Parameters -> Parameters
parametersFromUrl url state =
    case url.fragment of
        Nothing ->
            default

        Just fragment ->
            case Parser.parse fragment state of
                Err _ ->
                    default

                Ok result ->
                    result
