module GameTest exposing (suite)

import Expect
import Game
import Random
import Test exposing (Test)


suite : Test
suite =
    Test.describe "The Game module"
        [ Test.describe "shiftRight"
            [ Test.test "shift one to the right" <|
                \_ ->
                    let
                        expected =
                            [ Game.Twelve
                            , Game.One
                            , Game.Two
                            , Game.Three
                            , Game.Four
                            , Game.Five
                            , Game.Six
                            , Game.Seven
                            , Game.Eight
                            , Game.Nine
                            , Game.Ten
                            , Game.Eleven
                            ]

                        actual =
                            Game.solvedFields
                                |> Game.init
                                |> Game.shiftRight
                                |> Game.toList
                    in
                    Expect.equal actual expected
            ]
        , Test.describe "shiftLeft"
            [ Test.test "shift one to the left" <|
                \_ ->
                    let
                        expected =
                            [ Game.Two
                            , Game.Three
                            , Game.Four
                            , Game.Five
                            , Game.Six
                            , Game.Seven
                            , Game.Eight
                            , Game.Nine
                            , Game.Ten
                            , Game.Eleven
                            , Game.Twelve
                            , Game.One
                            ]

                        actual =
                            Game.solvedFields
                                |> Game.init
                                |> Game.shiftLeft
                                |> Game.toList
                    in
                    Expect.equal actual expected
            ]
        , Test.describe "swap"
            [ Test.test "swap 1" <|
                \_ ->
                    let
                        expected =
                            [ Game.Two
                            , Game.One
                            , Game.Four
                            , Game.Three
                            , Game.Five
                            , Game.Six
                            , Game.Seven
                            , Game.Eight
                            , Game.Nine
                            , Game.Ten
                            , Game.Eleven
                            , Game.Twelve
                            ]

                        actual =
                            Game.solvedFields
                                |> Game.init
                                |> Game.swap
                                |> Game.toList
                    in
                    Expect.equal actual expected
            ]
        , Test.describe "isSolved"
            [ Test.test "should be solved" <|
                \_ ->
                    let
                        expected =
                            True

                        actual =
                            Game.solvedFields
                                |> Game.init
                                |> Game.isSolved
                    in
                    Expect.equal actual expected
            , Test.test "left shifted should be solved" <|
                \_ ->
                    let
                        expected =
                            True

                        actual =
                            Game.solvedFields
                                |> Game.init
                                |> Game.shiftLeft
                                |> Game.isSolved
                    in
                    Expect.equal actual expected
            , Test.test "swapped should not be solved" <|
                \_ ->
                    let
                        expected =
                            False

                        actual =
                            Game.solvedFields
                                |> Game.init
                                |> Game.swap
                                |> Game.isSolved
                    in
                    Expect.equal actual expected
            , Test.test "random fields should not be solved" <|
                \_ ->
                    let
                        expected =
                            False

                        actual =
                            Game.randomFields (Random.initialSeed 42)
                                |> Game.init
                                |> Game.swap
                                |> Game.isSolved
                    in
                    Expect.equal actual expected
            ]
        ]
