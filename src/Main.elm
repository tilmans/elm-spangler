module Main exposing (main)

import Array
import Browser
import Browser.Events
import Browser.Navigation
import Css exposing (..)
import Fractal
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode
import Url exposing (Url)


type alias Flags =
    {}


type ActiveItem
    = Steps
    | Points
    | Repeat Int
    | New


type alias Model =
    { key : Browser.Navigation.Key
    , steps : Int
    , points : Int
    , repeats : Array.Array Int
    , active : ActiveItem
    }


type Msg
    = SetPoints Float
    | SetRepeat Int Float
    | KeyEvent Key
    | UrlChange Url
    | UrlRequest


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange url ->
            let
                params =
                    parametersFromUrl url
            in
            ( { model | steps = params.steps, points = params.points, repeats = params.repeats }, Cmd.none )

        UrlRequest ->
            ( model, Cmd.none )

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
                                            if Array.length model.repeats == 0 then
                                                Points

                                            else
                                                Repeat (Array.length model.repeats - 1)
                            }

                        Right ->
                            { model
                                | active =
                                    case model.active of
                                        Steps ->
                                            Points

                                        Points ->
                                            if Array.length model.repeats == 0 then
                                                New

                                            else
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

                                New ->
                                    { model
                                        | repeats = Array.push 1 model.repeats
                                        , active = Repeat (Array.length model.repeats)
                                    }

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
                                        lastIndex =
                                            Array.length model.repeats - 1

                                        current =
                                            Array.get index model.repeats
                                                |> Maybe.withDefault 0

                                        ( newRepeat, newPosition ) =
                                            if current < 2 && index == lastIndex then
                                                ( Array.slice 0 lastIndex model.repeats
                                                , if lastIndex - 1 >= 0 then
                                                    Repeat (lastIndex - 1)

                                                  else
                                                    Points
                                                )

                                            else
                                                ( Array.set index (decrementWithLimit current) model.repeats
                                                , Repeat index
                                                )
                                    in
                                    { model | repeats = newRepeat, active = newPosition }

                                New ->
                                    model

                        _ ->
                            model
            in
            ( newModel, setURL newModel )


setURL : Model -> Cmd msg
setURL model =
    Browser.Navigation.replaceUrl model.key ("#" ++ stringForParams model)


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


subscriptions model =
    Browser.Events.onKeyDown (Decode.map KeyEvent (Decode.map toKey keyDecoder))


keyDecoder =
    Decode.field "key" Decode.string


view : Model -> Browser.Document Msg
view model =
    { title = stringForParams model
    , body =
        [ div [ class "main" ]
            [ div [ class "controls-container" ]
                [ div [ class "controls" ] (inputControls model)
                ]
            , div [ class "fractal-container" ]
                [ div [ class "fractal" ] [ Fractal.draw model.steps model.points (Array.toList model.repeats) ]
                ]
            ]
        ]
    }


stringForParams model =
    String.fromInt model.steps
        ++ ","
        ++ String.fromInt model.points
        ++ Array.foldl (\i a -> a ++ "," ++ String.fromInt i) "" model.repeats


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


parametersFromUrl url =
    case url.fragment of
        Nothing ->
            { steps = 1, points = 3, repeats = Array.fromList [] }

        Just fragment ->
            let
                parts =
                    Array.fromList <| String.split "," fragment

                steps =
                    Array.get 0 parts |> Maybe.withDefault "1" |> String.toInt |> Maybe.withDefault 1

                points =
                    Array.get 1 parts |> Maybe.withDefault "3" |> String.toInt |> Maybe.withDefault 3

                repeats =
                    Array.slice 2 (Array.length parts) parts
                        |> Array.map (String.toInt >> Maybe.withDefault 1)
            in
            { steps = steps, points = points, repeats = repeats }


main : Program Flags Model Msg
main =
    Browser.application
        { init =
            \flags url key ->
                let
                    params =
                        parametersFromUrl url
                in
                ( Model key params.steps params.points params.repeats Steps, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = \url -> UrlRequest
        , onUrlChange = \url -> UrlChange url
        }
