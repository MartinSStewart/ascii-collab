module Compiles exposing (suite)

import Expect
import Main
import Test exposing (Test, test)


suite : Test
suite =
    let
        _ =
            Main.run
    in
    test "It compiles" <| \_ -> Expect.pass
