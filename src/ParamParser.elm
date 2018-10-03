module ParamParser exposing (parse, toString)

import Array
import Parser exposing (..)
import Types exposing (Parameters, Speed(..))
import Url


parse : String -> Maybe Parameters -> Result String Parameters
parse string state =
    let
        decoded =
            Url.percentDecode string |> Maybe.withDefault ""

        segments =
            String.split "," decoded
                |> Array.fromList

        stepSegment =
            Array.get 0 segments |> Maybe.andThen String.toInt

        pointsSegment =
            Array.get 1 segments |> Maybe.andThen String.toInt

        repeatSegments =
            Array.slice 2 (Array.length segments) segments
                |> Array.map
                    (\segment ->
                        if segment == "" then
                            Nothing

                        else
                            case run parameter segment of
                                Ok ( steps, speed ) ->
                                    Just ( steps, speed )

                                Err err ->
                                    Nothing
                    )
                |> Array.filter
                    (\segment ->
                        case segment of
                            Nothing ->
                                False

                            Just value ->
                                True
                    )
                |> Array.map
                    (\segment ->
                        case segment of
                            Just value ->
                                value

                            Nothing ->
                                ( -1, Stopped )
                    )
                |> (\v ->
                        case state of
                            Nothing ->
                                Array.map
                                    (\( steps, speed ) ->
                                        ( steps, 0, speed )
                                    )
                                    v

                            Just old ->
                                Array.indexedMap
                                    (\i ( steps, speed ) ->
                                        let
                                            ( _, time, _ ) =
                                                Array.get i old.repeats |> Maybe.withDefault ( 0, 0, Stopped )
                                        in
                                        ( steps, time, speed )
                                    )
                                    v
                   )
    in
    case stepSegment of
        Nothing ->
            Err "Steps missing or wrong format"

        Just step ->
            case pointsSegment of
                Nothing ->
                    Err "Points missing or wrong format"

                Just points ->
                    Ok { steps = step, points = points, repeats = repeatSegments }


parameter : Parser ( Int, Speed )
parameter =
    succeed
        (\step speed ->
            ( step, speed )
        )
        |= int
        |= oneOf
            [ backtrackable (succeed Backward |. symbol "|" |. symbol "-" |= int |. end)
            , succeed Forward |. symbol "|" |= int |. end
            , succeed Stopped |. end
            ]


intToSpeed : Int -> Speed
intToSpeed int =
    if int > 0 then
        Forward int

    else if int < 0 then
        Backward int

    else
        Stopped


toString : Parameters -> String
toString params =
    String.fromInt params.steps
        ++ ","
        ++ String.fromInt params.points
        ++ (if Array.length params.repeats == 0 then
                ""

            else
                ","
                    ++ (Array.map
                            (\( repeats, time, speed ) ->
                                String.fromInt repeats
                                    ++ (case speed of
                                            Forward s ->
                                                "|" ++ String.fromInt s

                                            Backward s ->
                                                "|-" ++ String.fromInt s

                                            Stopped ->
                                                ""
                                       )
                            )
                            params.repeats
                            |> Array.toList
                            |> String.join ","
                       )
           )
