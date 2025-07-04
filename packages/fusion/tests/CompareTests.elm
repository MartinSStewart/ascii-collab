module CompareTests exposing (suite)

import Expect
import Fusion exposing (Value)
import Fusion.Value as Value
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3)
import Utils


suite : Test
suite =
    describe "Value.compare"
        [ fuzz valueFuzzer "Is reflexive" <|
            \value ->
                Value.compare value value
                    |> Expect.equal EQ
        , fuzz2 valueFuzzer valueFuzzer "Is symmetric" <|
            \l r ->
                Value.compare r l
                    |> Expect.equal
                        (invertOrder <| Value.compare l r)
        , fuzz3 valueFuzzer valueFuzzer valueFuzzer "Is transitive" <|
            \l m r ->
                let
                    step2 : Value -> Value -> Expect.Expectation
                    step2 a b =
                        case Value.compare r a of
                            LT ->
                                Value.compare r b
                                    |> Expect.equal LT

                            _ ->
                                Expect.pass
                in
                case Value.compare l m of
                    EQ ->
                        Expect.pass

                    LT ->
                        step2 l m

                    GT ->
                        step2 m l
        ]


invertOrder : Order -> Order
invertOrder order =
    case order of
        LT ->
            GT

        GT ->
            LT

        EQ ->
            EQ


valueFuzzer : Fuzzer Value
valueFuzzer =
    Fuzz.map Tuple.second (Utils.typeAndValueFuzzer 2)
