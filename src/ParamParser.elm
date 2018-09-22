module ParamParser exposing (Parameter, Parameters, parse)

import Parser exposing (..)


type alias Parameters =
    List Parameter


type alias Parameter =
    { steps : Int, speed : Float }


parse : String -> Parameters
parse string =
    String.split "," string
        |> List.map
            (\segment ->
                if segment == "" then
                    Nothing

                else
                    case run parameter segment of
                        Ok result ->
                            Just result

                        Err err ->
                            let
                                _ =
                                    Debug.log "Error" err
                            in
                            Nothing
            )
        |> List.filter
            (\segment ->
                case segment of
                    Nothing ->
                        False

                    Just value ->
                        True
            )
        |> List.map
            (\segment ->
                case segment of
                    Just value ->
                        value

                    Nothing ->
                        Parameter -1 -1
            )


parameter : Parser Parameter
parameter =
    succeed
        (\step speed ->
            Parameter step (speed |> Maybe.withDefault 0)
        )
        |= int
        |= oneOf [ succeed Just |. symbol "|" |= float, succeed Nothing |. end ]
