module Image.Internal.Array2D exposing (Array2D, last, lastLength, push)

import Array exposing (Array)


type alias Array2D a =
    Array (Array a)


push : a -> Array2D a -> Array2D a
push item arr =
    Array.get (lastIndex_ arr) arr
        |> Maybe.map (\arr2 -> Array.set (lastIndex_ arr) (Array.push item arr2) arr)
        |> Maybe.withDefault arr


last : Array2D a -> Maybe a
last arr =
    Array.get (lastIndex_ arr) arr |> Maybe.andThen (\subArr -> Array.get (lastIndex_ subArr) subArr)


lastLength : Array (Array a) -> Int
lastLength arr =
    Array.get (lastIndex_ arr) arr
        |> Maybe.map Array.length
        |> Maybe.withDefault 0


lastIndex_ : Array a -> Int
lastIndex_ arr =
    Array.length arr - 1
