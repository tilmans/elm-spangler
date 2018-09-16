module Main exposing (main)

import Browser
import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Flags =
    {}


type alias Model =
    { steps : Int
    , points : Int
    , repeats : List Int
    }


type Msg
    = NoOp


init flags =
    ( Model 1 5 [ 5, 4 ], Cmd.none )


update msg model =
    ( model, Cmd.none )


subscriptions model =
    Sub.none


view model =
    let
        scale =
            100

        segments =
            2 * pi / toFloat model.points

        lines =
            List.range 0 (model.points - 1)
                |> List.map toFloat
                |> List.map (\m -> ( cos ((segments * (-1 * m)) + pi / 2), sin ((segments * (-1 * m)) + pi / 2) ))
                |> List.reverse
                |> (::) ( 0, 1 )
                |> List.map (\( x, y ) -> ( x * scale, y * scale * -1 ))
                |> List.foldr
                    (\( x, y ) accum ->
                        case accum.previous of
                            Nothing ->
                                let
                                    _ =
                                        Debug.log "Start" ( x, y )
                                in
                                { previous = Just ( x, y ), lines = accum.lines }

                            Just ( x_, y_ ) ->
                                let
                                    _ =
                                        Debug.log "Next" ( x, y )
                                in
                                { previous = Just ( x, y ), lines = drawLine x_ y_ x y :: accum.lines }
                    )
                    { previous = Nothing, lines = [] }

        items =
            List.foldl (\c svg -> groupItems c scale svg) lines.lines model.repeats

        pp =
            Debug.log "Point1" (pointOnCircle 3 0)

        _ =
            Debug.log "Points"
                (Debug.toString
                    (List.range 0 2
                        --|> List.map toFloat
                        |> List.map (pointOnCircle 3)
                        |> List.map (\( x, y ) -> ( x * scale, y * scale ))
                    )
                )
    in
    svg [ width "500", height "500", viewBox "-105 -105 210 210" ]
        (circle [ x "0", y "0", r "100", stroke "none", fill "none" ] [] :: items)


groupItems : Int -> Float -> List (Svg msg) -> List (Svg msg)
groupItems count scale items =
    List.range 0 (count - 1)
        --|> List.map toFloat
        |> List.map (pointOnCircle count)
        |> List.map (\( x, y ) -> ( x * scale, y * scale ))
        |> List.map (\( x, y ) -> g [ transform ("scale(0.5 0.5) translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")") ] items)


pointOnCircle : Int -> Int -> ( Float, Float )
pointOnCircle total index =
    let
        radius =
            2 * pi / toFloat total * toFloat index - pi / 2
    in
    Debug.log "Point" ( cos radius, sin radius )


drawLine x1_ y1_ x2_ y2_ =
    line [ x1 (String.fromFloat x1_), y1 (String.fromFloat y1_), x2 (String.fromFloat x2_), y2 (String.fromFloat y2_), stroke "black" ] []


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
