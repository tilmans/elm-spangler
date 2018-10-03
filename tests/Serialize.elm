module Serialize exposing (suite)

import Array exposing (Array)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import ParamParser exposing (..)
import Test exposing (..)
import Types exposing (..)


suite : Test
suite =
    describe "Parser tests"
        [ describe "Parse string"
            [ test "steps and points" <|
                \_ ->
                    case parse "2,2" Nothing of
                        Err err ->
                            Expect.true err False

                        Ok result ->
                            Expect.equal result
                                { steps = 2, points = 2, repeats = Array.fromList [] }
            , test "one repeat without time and speed" <|
                \_ ->
                    case parse "2,1,2" Nothing of
                        Err err ->
                            Expect.true err False

                        Ok result ->
                            Expect.equal result
                                { steps = 2, points = 1, repeats = Array.fromList [ ( 2, 0, Types.Stopped ) ] }
            , test "one repeat with speed" <|
                \_ ->
                    case parse "2,1,2|2" Nothing of
                        Err err ->
                            Expect.true err False

                        Ok result ->
                            Expect.equal result
                                { steps = 2, points = 1, repeats = Array.fromList [ ( 2, 0, Types.Forward 2 ) ] }
            , test "one repeat with speed and existing time" <|
                \_ ->
                    case
                        parse "2,1,2|2"
                            (Just
                                { steps = 2
                                , points = 1
                                , repeats = Array.fromList [ ( 0, 10, Stopped ) ]
                                }
                            )
                    of
                        Err err ->
                            Expect.true err False

                        Ok result ->
                            Expect.equal result
                                { steps = 2, points = 1, repeats = Array.fromList [ ( 2, 10, Types.Forward 2 ) ] }
            , test "one repeat with negative speed" <|
                \_ ->
                    case parse "2,1,2|-2" Nothing of
                        Err err ->
                            Expect.true err False

                        Ok result ->
                            Expect.equal result
                                { steps = 2, points = 1, repeats = Array.fromList [ ( 2, 0, Types.Backward 2 ) ] }
            , test "only one" <|
                \_ ->
                    case parse "2" Nothing of
                        Err err ->
                            Expect.true err True

                        Ok result ->
                            Expect.true "Should have failed" False
            , test "two with speed" <|
                \_ ->
                    case parse "1,2,2|2,3|3" Nothing of
                        Err err ->
                            Expect.true err False

                        Ok result ->
                            Expect.equal result
                                { steps = 1, points = 2, repeats = Array.fromList [ ( 2, 0, Types.Forward 2 ), ( 3, 0, Types.Forward 3 ) ] }
            , test "two without speed" <|
                \_ ->
                    case parse "2,3,2,3" Nothing of
                        Err err ->
                            Expect.true err False

                        Ok result ->
                            Expect.equal result
                                { steps = 2, points = 3, repeats = Array.fromList [ ( 2, 0, Types.Stopped ), ( 3, 0, Types.Stopped ) ] }
            , test "two mixed" <|
                \_ ->
                    case parse "2,3,2|3,3" Nothing of
                        Err err ->
                            Expect.true err False

                        Ok result ->
                            Expect.equal result
                                { steps = 2, points = 3, repeats = Array.fromList [ ( 2, 0, Types.Forward 3 ), ( 3, 0, Types.Stopped ) ] }
            , test "trailing comma" <|
                \_ ->
                    case parse "2,3," Nothing of
                        Err err ->
                            Expect.true err False

                        Ok result ->
                            Expect.equal result
                                { steps = 2, points = 3, repeats = Array.fromList [] }
            , test "empty" <|
                \_ ->
                    case parse "" Nothing of
                        Err err ->
                            Expect.true err True

                        Ok result ->
                            Expect.true "Should have failed" False
            ]
        , describe "Construct String"
            [ test "one with speed" <|
                \_ ->
                    Expect.equal (toString { steps = 2, points = 2, repeats = Array.fromList [ ( 2, 0, Types.Forward 2 ) ] })
                        "2,2,2|2"
            , test "one without speed" <|
                \_ ->
                    Expect.equal (toString { steps = 2, points = 2, repeats = Array.fromList [ ( 2, 0, Types.Stopped ) ] })
                        "2,2,2"
            , test "two with speed" <|
                \_ ->
                    Expect.equal (toString { steps = 1, points = 2, repeats = Array.fromList [ ( 2, 0, Types.Forward 2 ), ( 3, 0, Types.Backward 3 ) ] })
                        "1,2,2|2,3|-3"
            , test "basics" <|
                \_ ->
                    Expect.equal (toString { steps = 2, points = 3, repeats = Array.fromList [] })
                        "2,3"
            , test "two mixed" <|
                \_ ->
                    Expect.equal (toString { steps = 2, points = 2, repeats = Array.fromList [ ( 2, 0, Types.Forward 3 ), ( 4, 0, Types.Stopped ) ] })
                        "2,2,2|3,4"
            ]
        ]
