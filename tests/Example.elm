module Example exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import ParamParser exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Parser tests"
        [ describe "Parse string"
            [ test "one with speed" <|
                \_ ->
                    Expect.equal (parse "2|2")
                        [ Parameter 2 2 ]
            , test "one without speed" <|
                \_ ->
                    Expect.equal (parse "2")
                        [ Parameter 2 0 ]
            , test "two with speed" <|
                \_ ->
                    Expect.equal (parse "2|2,3|3")
                        [ Parameter 2 2, Parameter 3 3 ]
            , test "two without speed" <|
                \_ ->
                    Expect.equal (parse "2,3")
                        [ Parameter 2 0, Parameter 3 0 ]
            , test "two mixed" <|
                \_ ->
                    Expect.equal (parse "2,3|3")
                        [ Parameter 2 0, Parameter 3 3 ]
            , test "trailing comma" <|
                \_ ->
                    Expect.equal (parse "2,")
                        [ Parameter 2 0 ]
            , test "empty" <|
                \_ ->
                    Expect.equal (parse "")
                        []
            ]
        , describe "Construct String"
            [ test "one with speed" <|
                \_ ->
                    Expect.equal (toString [ Parameter 2 2 ])
                        "2|2"
            , test "one without speed" <|
                \_ ->
                    Expect.equal (toString [ Parameter 2 0 ])
                        "2"
            , test "two with speed" <|
                \_ ->
                    Expect.equal (toString [ Parameter 2 2, Parameter 3 3 ])
                        "2|2,3|3"
            , test "two without speed" <|
                \_ ->
                    Expect.equal (toString [ Parameter 2 0, Parameter 3 0 ])
                        "2,3"
            , test "two mixed" <|
                \_ ->
                    Expect.equal (toString [ Parameter 2 0, Parameter 3 3 ])
                        "2,3|3"
            , test "empty" <|
                \_ ->
                    Expect.equal (toString [])
                        ""
            ]
        ]
