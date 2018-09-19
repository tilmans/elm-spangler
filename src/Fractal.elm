module Fractal exposing (draw)

import Array
import Svg exposing (..)
import Svg.Attributes exposing (..)


draw : Int -> Int -> List Int -> Svg msg
draw step points repeats =
    let
        scale =
            100

        segments =
            2 * pi / toFloat points

        lines =
            List.range 0 (points - 1)
                |> List.map toFloat
                |> List.map (\m -> ( cos ((segments * (-1 * m)) + pi / 2), sin ((segments * (-1 * m)) + pi / 2) ))
                |> List.reverse
                |> List.map (\( x, y ) -> ( x * scale, y * scale * -1 ))
                |> drawPoints step

        items =
            List.foldl (\c svg -> groupItems c scale svg) lines repeats
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
            line [ x1 (String.fromFloat x1_), y1 (String.fromFloat y1_), x2 (String.fromFloat x2_), y2 (String.fromFloat y2_), stroke "black" ] []
        )
        points


wrappingGet : Int -> Array.Array ( Float, Float ) -> ( Float, Float )
wrappingGet position array =
    Array.get
        (modBy (Array.length array) position)
        array
        |> Maybe.withDefault ( 0, 0 )


groupItems : Int -> Float -> List (Svg msg) -> List (Svg msg)
groupItems count scale items =
    List.range 0 (count - 1)
        |> List.map (\i -> ( i, 360 / toFloat count * toFloat i ))
        |> List.map
            (\( i, r ) ->
                g
                    [ transform
                        ("scale(0.5 0.5) "
                            ++ "translate(0 -100) "
                            ++ "rotate("
                            ++ String.fromFloat r
                            ++ " 0 100)"
                        )
                    ]
                    items
            )


pointOnCircle : Int -> Int -> ( Float, Float )
pointOnCircle total index =
    let
        radius =
            2 * pi / toFloat total * toFloat index - pi / 2
    in
    ( cos radius, sin radius )


drawLine : Float -> Float -> Float -> Float -> Svg msg
drawLine x1_ y1_ x2_ y2_ =
    line [ x1 (String.fromFloat x1_), y1 (String.fromFloat y1_), x2 (String.fromFloat x2_), y2 (String.fromFloat y2_), stroke "black" ] []


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
