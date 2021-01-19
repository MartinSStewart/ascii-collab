module NonemptyExtra exposing (find, greedyGroupsOf, maximumBy, minimumBy, transpose, updateFirst, updateIf)

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


maximumBy : (a -> comparable) -> Nonempty a -> a
maximumBy by nonempty =
    List.maximumBy by (List.Nonempty.toList nonempty) |> Maybe.withDefault (List.Nonempty.head nonempty)


minimumBy : (a -> comparable) -> Nonempty a -> a
minimumBy by nonempty =
    List.minimumBy by (List.Nonempty.toList nonempty) |> Maybe.withDefault (List.Nonempty.head nonempty)


updateFirst : (a -> Bool) -> (a -> a) -> Nonempty a -> Nonempty a
updateFirst predicate update_ nonempty =
    List.Nonempty.toList nonempty
        |> updateFirstHelper predicate update_
        |> List.Nonempty.fromList
        |> Maybe.withDefault nonempty


updateFirstHelper : (a -> Bool) -> (a -> a) -> List a -> List a
updateFirstHelper predicate update_ list =
    case list of
        head :: rest ->
            if predicate head then
                update_ head :: rest

            else
                head :: updateFirstHelper predicate update_ rest

        [] ->
            []


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


{-| Find the first element that satisfies a predicate and return
Just that element. If none match, return Nothing.

    find (\num -> num > 5) [ 2, 4, 6, 8 ]
    --> Just 6

-}
find : (a -> Bool) -> Nonempty a -> Maybe a
find predicate list =
    List.Nonempty.toList list |> List.find predicate
