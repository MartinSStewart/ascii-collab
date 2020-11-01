module NonemptyExtra exposing (greedyGroupsOf, transpose, updateIf)

import List.Extra as List
import List.Nonempty exposing (Nonempty)


updateIf : (a -> Bool) -> (a -> a) -> Nonempty a -> Nonempty a
updateIf predicate update_ nonempty =
    List.Nonempty.map
        (\item ->
            if predicate item then
                update_ item

            else
                item
        )
        nonempty


{-| Greedily split list into groups of length `size`. The last group of
elements will be included regardless of whether there are enough elements in
the list to completely fill it. This is equivalent to calling
`greedyGroupsOfWithStep` with the same `size` and `step`.

    greedyGroupsOf 3 (List.range 1 10)
    --> [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ], [ 10 ] ]

-}
greedyGroupsOf : Int -> Nonempty a -> Nonempty (Nonempty a)
greedyGroupsOf size xs =
    let
        list =
            List.Nonempty.toList xs
    in
    case List.take size list |> List.Nonempty.fromList of
        Just group ->
            List.Nonempty.Nonempty group (greedyGroupsOfHelper size (List.drop size list))

        Nothing ->
            List.Nonempty.fromElement xs


greedyGroupsOfHelper : Int -> List a -> List (Nonempty a)
greedyGroupsOfHelper size xs =
    case List.take size xs |> List.Nonempty.fromList of
        Just list ->
            list :: greedyGroupsOfHelper size (List.drop size xs)

        Nothing ->
            []


transpose : Nonempty (Nonempty a) -> Nonempty (Nonempty a)
transpose nonempty =
    List.Nonempty.toList nonempty
        |> List.map List.Nonempty.toList
        |> List.transpose
        |> List.filterMap List.Nonempty.fromList
        |> List.Nonempty.fromList
        |> Maybe.withDefault nonempty



--List.Nonempty.tail nonempty
--    |> List.map List.Nonempty.toList
--    |> List.transpose
--    |> List.zip (List.Nonempty.head nonempty |> List.Nonempty.toList)
--    |> List.map (\( head, rest ) -> List.Nonempty.Nonempty head rest)
--    |> List.Nonempty.fromList
--    |> Maybe.withDefault nonempty
