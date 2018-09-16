module Fractal exposing (draw)

import Svg exposing (..)
import Svg.Attributes exposing (..)


draw points repeats =
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
                |> (::) ( 0, 1 )
                |> List.map (\( x, y ) -> ( x * scale, y * scale * -1 ))
                |> List.foldr
                    (\( x, y ) accum ->
                        case accum.previous of
                            Nothing ->
                                { previous = Just ( x, y ), lines = accum.lines }

                            Just ( x_, y_ ) ->
                                { previous = Just ( x, y ), lines = drawLine x_ y_ x y :: accum.lines }
                    )
                    { previous = Nothing, lines = [] }

        items =
            List.foldl (\c svg -> groupItems c scale svg) lines.lines repeats
    in
    svg [ width "500", height "500", viewBox "-105 -105 210 210" ]
        (circle [ x "0", y "0", r "100", stroke "none", fill "none" ] [] :: items)


groupItems : Int -> Float -> List (Svg msg) -> List (Svg msg)
groupItems count scale items =
    List.range 0 (count - 1)
        |> List.map (pointOnCircle count)
        |> List.map (\( x, y ) -> ( x * scale, y * scale ))
        |> List.map (\( x, y ) -> g [ transform ("scale(0.5 0.5) translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")") ] items)


pointOnCircle : Int -> Int -> ( Float, Float )
pointOnCircle total index =
    let
        radius =
            2 * pi / toFloat total * toFloat index - pi / 2
    in
    ( cos radius, sin radius )


drawLine x1_ y1_ x2_ y2_ =
    line [ x1 (String.fromFloat x1_), y1 (String.fromFloat y1_), x2 (String.fromFloat x2_), y2 (String.fromFloat y2_), stroke "black" ] []
