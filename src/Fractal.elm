module Fractal exposing (draw)

import Array
import Dict exposing (Dict)
import List.Extra
import Svg exposing (Svg, circle, g, line, svg)
import Svg.Attributes exposing (..)
import Types exposing (Parameters)


draw : Parameters -> List String -> Svg msg
draw parameters colors =
    let
        scale =
            100

        segments =
            2 * pi / toFloat parameters.points

        lines =
            List.range 0 (parameters.points - 1)
                |> List.map toFloat
                |> List.map (\m -> ( cos ((segments * (-1 * m)) + pi / 2), sin ((segments * (-1 * m)) + pi / 2) ))
                |> List.reverse
                |> List.map (\( x, y ) -> ( x * scale, y * scale * -1 ))
                |> drawPoints parameters.steps

        ( counter, items ) =
            Array.foldl
                (\( steps, time, _ ) ( count, svg ) ->
                    let
                        --rotation =
                        --    Dict.get count time |> Maybe.withDefault 0
                        colors_ =
                            if count == 0 then
                                colors

                            else
                                [ "default" ]
                    in
                    ( count + 1, groupItems time steps colors_ scale svg )
                )
                ( 0, lines )
                parameters.repeats
    in
    svg [ width "500", height "500", viewBox "-105 -105 210 210" ]
        (circle [ x "0", y "0", r "100", stroke "none", fill "none" ] [] :: items)


drawPoints : Int -> List ( Float, Float ) -> List (Svg msg)
drawPoints step points =
    let
        arrayOfPoints =
            Array.fromList points
    in
    List.indexedMap
        (\i ( x1_, y1_ ) ->
            let
                ( x2_, y2_ ) =
                    wrappingGet (i + step) arrayOfPoints
            in
            line
                [ x1 (String.fromFloat x1_)
                , y1 (String.fromFloat y1_)
                , x2 (String.fromFloat x2_)
                , y2 (String.fromFloat y2_)

                --, style "stroke: black;"
                ]
                []
        )
        points


wrappingGet : Int -> Array.Array ( Float, Float ) -> ( Float, Float )
wrappingGet position array =
    Array.get
        (modBy (Array.length array) position)
        array
        |> Maybe.withDefault ( 0, 0 )


groupItems : Float -> Int -> List String -> Float -> List (Svg msg) -> List (Svg msg)
groupItems rotation count colors scale items =
    List.range 0 (count - 1)
        |> List.map (\i -> ( i, 360 / toFloat count * toFloat i ))
        |> List.map2
            (\color ( i, r ) ->
                g
                    [ class color
                    , transform
                        ("scale(0.5 0.5) "
                            ++ "translate(0 -100) "
                            ++ "rotate("
                            ++ String.fromFloat (r + rotation)
                            ++ " 0 100)"
                        )
                    ]
                    items
            )
            (List.Extra.cycle count colors)


pointOnCircle : Int -> Int -> ( Float, Float )
pointOnCircle total index =
    let
        radius =
            2 * pi / toFloat total * toFloat index - pi / 2
    in
    ( cos radius, sin radius )



--drawLine : Float -> Float -> Float -> Float -> Svg msg
--drawLine x1_ y1_ x2_ y2_ =
--    line
--        [ x1 (String.fromFloat x1_)
--        , y1 (String.fromFloat y1_)
--        , x2 (String.fromFloat x2_)
--        , y2 (String.fromFloat y2_)
--        --, stroke "black"
--        ]
--        []


pointsToString : List ( Float, Float ) -> String
pointsToString =
    List.foldr
        (\( x, y ) a ->
            a
                ++ "("
                ++ String.fromFloat x
                ++ ", "
                ++ String.fromFloat y
                ++ ") "
        )
        ""
