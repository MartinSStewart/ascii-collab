module Fusion.ValueDict exposing
    ( ValueDict
    , empty, singleton, insert, update, remove
    , isEmpty, member, get, size
    , keys, values, toList, fromList
    , map, foldl, foldr, filter, partition
    , union, intersect, diff, merge
    )

{-|


# Dictionaries

@docs ValueDict


# Build

@docs empty, singleton, insert, update, remove


# Query

@docs isEmpty, member, get, size


# Lists

@docs keys, values, toList, fromList


# Transform

@docs map, foldl, foldr, filter, partition


# Combine

@docs union, intersect, diff, merge

-}

import Fusion exposing (Value)
import Fusion.Value as Value



-- DICTIONARIES
-- The color of a node. Leaves are considered Black.


type NColor
    = Red
    | Black


{-| A dictionary whose keys are Value
-}
type ValueDict v
    = RBNode_elm_builtin NColor Value v (ValueDict v) (ValueDict v)
    | RBEmpty_elm_builtin


{-| Create an empty dictionary.
-}
empty : ValueDict v
empty =
    RBEmpty_elm_builtin


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

    animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]

    get "Tom"   animals == Just Cat
    get "Jerry" animals == Just Mouse
    get "Spike" animals == Nothing

-}
get : Value -> ValueDict v -> Maybe v
get targetKey dict =
    case dict of
        RBEmpty_elm_builtin ->
            Nothing

        RBNode_elm_builtin _ key value left right ->
            case Value.compare targetKey key of
                LT ->
                    get targetKey left

                EQ ->
                    Just value

                GT ->
                    get targetKey right


{-| Determine if a key is in a dictionary.
-}
member : Value -> ValueDict v -> Bool
member key dict =
    case get key dict of
        Just _ ->
            True

        Nothing ->
            False


{-| Determine the number of key-value pairs in the dictionary.
-}
size : ValueDict v -> Int
size dict =
    sizeHelp 0 dict


sizeHelp : Int -> ValueDict v -> Int
sizeHelp n dict =
    case dict of
        RBEmpty_elm_builtin ->
            n

        RBNode_elm_builtin _ _ _ left right ->
            sizeHelp (sizeHelp (n + 1) right) left


{-| Determine if a dictionary is empty.

    isEmpty empty == True

-}
isEmpty : ValueDict v -> Bool
isEmpty dict =
    case dict of
        RBEmpty_elm_builtin ->
            True

        RBNode_elm_builtin _ _ _ _ _ ->
            False


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision.
-}
insert : Value -> v -> ValueDict v -> ValueDict v
insert key value dict =
    -- Root node is always Black
    case insertHelp key value dict of
        RBNode_elm_builtin Red k v l r ->
            RBNode_elm_builtin Black k v l r

        x ->
            x


insertHelp : Value -> v -> ValueDict v -> ValueDict v
insertHelp key value dict =
    case dict of
        RBEmpty_elm_builtin ->
            -- New nodes are always red. If it violates the rules, it will be fixed
            -- when balancing.
            RBNode_elm_builtin Red key value RBEmpty_elm_builtin RBEmpty_elm_builtin

        RBNode_elm_builtin nColor nKey nValue nLeft nRight ->
            case Value.compare key nKey of
                LT ->
                    balance nColor nKey nValue (insertHelp key value nLeft) nRight

                EQ ->
                    RBNode_elm_builtin nColor nKey value nLeft nRight

                GT ->
                    balance nColor nKey nValue nLeft (insertHelp key value nRight)


balance : NColor -> Value -> v -> ValueDict v -> ValueDict v -> ValueDict v
balance color key value left right =
    case right of
        RBNode_elm_builtin Red rK rV rLeft rRight ->
            case left of
                RBNode_elm_builtin Red lK lV lLeft lRight ->
                    RBNode_elm_builtin
                        Red
                        key
                        value
                        (RBNode_elm_builtin Black lK lV lLeft lRight)
                        (RBNode_elm_builtin Black rK rV rLeft rRight)

                _ ->
                    RBNode_elm_builtin color rK rV (RBNode_elm_builtin Red key value left rLeft) rRight

        _ ->
            case left of
                RBNode_elm_builtin Red lK lV (RBNode_elm_builtin Red llK llV llLeft llRight) lRight ->
                    RBNode_elm_builtin
                        Red
                        lK
                        lV
                        (RBNode_elm_builtin Black llK llV llLeft llRight)
                        (RBNode_elm_builtin Black key value lRight right)

                _ ->
                    RBNode_elm_builtin color key value left right


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.
-}
remove : Value -> ValueDict v -> ValueDict v
remove key dict =
    -- Root node is always Black
    case removeHelp key dict of
        RBNode_elm_builtin Red k v l r ->
            RBNode_elm_builtin Black k v l r

        x ->
            x


{-| The easiest thing to remove from the tree, is a red node. However, when searching for the
node to remove, we have no way of knowing if it will be red or not. This remove implementation
makes sure that the bottom node is red by moving red colors down the tree through rotation
and color flips. Any violations this will cause, can easily be fixed by balancing on the way
up again.
-}
removeHelp : Value -> ValueDict v -> ValueDict v
removeHelp targetKey dict =
    case dict of
        RBEmpty_elm_builtin ->
            RBEmpty_elm_builtin

        RBNode_elm_builtin color key value left right ->
            if Value.compare targetKey key == LT then
                case left of
                    RBNode_elm_builtin Black _ _ lLeft _ ->
                        case lLeft of
                            RBNode_elm_builtin Red _ _ _ _ ->
                                RBNode_elm_builtin color key value (removeHelp targetKey left) right

                            _ ->
                                case moveRedLeft dict of
                                    RBNode_elm_builtin nColor nKey nValue nLeft nRight ->
                                        balance nColor nKey nValue (removeHelp targetKey nLeft) nRight

                                    RBEmpty_elm_builtin ->
                                        RBEmpty_elm_builtin

                    _ ->
                        RBNode_elm_builtin color key value (removeHelp targetKey left) right

            else
                removeHelpEQGT targetKey (removeHelpPrepEQGT targetKey dict color key value left right)


removeHelpPrepEQGT : Value -> ValueDict v -> NColor -> Value -> v -> ValueDict v -> ValueDict v -> ValueDict v
removeHelpPrepEQGT _ dict color key value left right =
    case left of
        RBNode_elm_builtin Red lK lV lLeft lRight ->
            RBNode_elm_builtin
                color
                lK
                lV
                lLeft
                (RBNode_elm_builtin Red key value lRight right)

        _ ->
            case right of
                RBNode_elm_builtin Black _ _ (RBNode_elm_builtin Black _ _ _ _) _ ->
                    moveRedRight dict

                RBNode_elm_builtin Black _ _ RBEmpty_elm_builtin _ ->
                    moveRedRight dict

                _ ->
                    dict


{-| When we find the node we are looking for, we can remove by replacing the key-value
pair with the key-value pair of the left-most node on the right side (the closest pair).
-}
removeHelpEQGT : Value -> ValueDict v -> ValueDict v
removeHelpEQGT targetKey dict =
    case dict of
        RBNode_elm_builtin color key value left right ->
            if targetKey == key then
                case getMin right of
                    RBNode_elm_builtin _ minKey minValue _ _ ->
                        balance color minKey minValue left (removeMin right)

                    RBEmpty_elm_builtin ->
                        RBEmpty_elm_builtin

            else
                balance color key value left (removeHelp targetKey right)

        RBEmpty_elm_builtin ->
            RBEmpty_elm_builtin


getMin : ValueDict v -> ValueDict v
getMin dict =
    case dict of
        RBNode_elm_builtin _ _ _ ((RBNode_elm_builtin _ _ _ _ _) as left) _ ->
            getMin left

        _ ->
            dict


removeMin : ValueDict v -> ValueDict v
removeMin dict =
    case dict of
        RBNode_elm_builtin color key value ((RBNode_elm_builtin lColor _ _ lLeft _) as left) right ->
            case lColor of
                Black ->
                    case lLeft of
                        RBNode_elm_builtin Red _ _ _ _ ->
                            RBNode_elm_builtin color key value (removeMin left) right

                        _ ->
                            case moveRedLeft dict of
                                RBNode_elm_builtin nColor nKey nValue nLeft nRight ->
                                    balance nColor nKey nValue (removeMin nLeft) nRight

                                RBEmpty_elm_builtin ->
                                    RBEmpty_elm_builtin

                _ ->
                    RBNode_elm_builtin color key value (removeMin left) right

        _ ->
            RBEmpty_elm_builtin


moveRedLeft : ValueDict v -> ValueDict v
moveRedLeft dict =
    case dict of
        RBNode_elm_builtin _ k v (RBNode_elm_builtin _ lK lV lLeft lRight) (RBNode_elm_builtin _ rK rV (RBNode_elm_builtin Red rlK rlV rlL rlR) rRight) ->
            RBNode_elm_builtin
                Red
                rlK
                rlV
                (RBNode_elm_builtin Black k v (RBNode_elm_builtin Red lK lV lLeft lRight) rlL)
                (RBNode_elm_builtin Black rK rV rlR rRight)

        RBNode_elm_builtin clr k v (RBNode_elm_builtin _ lK lV lLeft lRight) (RBNode_elm_builtin _ rK rV rLeft rRight) ->
            case clr of
                Black ->
                    RBNode_elm_builtin
                        Black
                        k
                        v
                        (RBNode_elm_builtin Red lK lV lLeft lRight)
                        (RBNode_elm_builtin Red rK rV rLeft rRight)

                Red ->
                    RBNode_elm_builtin
                        Black
                        k
                        v
                        (RBNode_elm_builtin Red lK lV lLeft lRight)
                        (RBNode_elm_builtin Red rK rV rLeft rRight)

        _ ->
            dict


moveRedRight : ValueDict v -> ValueDict v
moveRedRight dict =
    case dict of
        RBNode_elm_builtin _ k v (RBNode_elm_builtin _ lK lV (RBNode_elm_builtin Red llK llV llLeft llRight) lRight) (RBNode_elm_builtin _ rK rV rLeft rRight) ->
            RBNode_elm_builtin
                Red
                lK
                lV
                (RBNode_elm_builtin Black llK llV llLeft llRight)
                (RBNode_elm_builtin Black k v lRight (RBNode_elm_builtin Red rK rV rLeft rRight))

        RBNode_elm_builtin clr k v (RBNode_elm_builtin _ lK lV lLeft lRight) (RBNode_elm_builtin _ rK rV rLeft rRight) ->
            case clr of
                Black ->
                    RBNode_elm_builtin
                        Black
                        k
                        v
                        (RBNode_elm_builtin Red lK lV lLeft lRight)
                        (RBNode_elm_builtin Red rK rV rLeft rRight)

                Red ->
                    RBNode_elm_builtin
                        Black
                        k
                        v
                        (RBNode_elm_builtin Red lK lV lLeft lRight)
                        (RBNode_elm_builtin Red rK rV rLeft rRight)

        _ ->
            dict


{-| Update the value of a dictionary for a specific key with a given function.
-}
update : Value -> (Maybe v -> Maybe v) -> ValueDict v -> ValueDict v
update targetKey alter dictionary =
    case alter (get targetKey dictionary) of
        Just value ->
            insert targetKey value dictionary

        Nothing ->
            remove targetKey dictionary


{-| Create a dictionary with one key-value pair.
-}
singleton : Value -> v -> ValueDict v
singleton key value =
    -- Root node is always Black
    RBNode_elm_builtin Black key value RBEmpty_elm_builtin RBEmpty_elm_builtin



-- COMBINE


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.
-}
union : ValueDict v -> ValueDict v -> ValueDict v
union t1 t2 =
    foldl insert t2 t1


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.
-}
intersect : ValueDict v -> ValueDict v -> ValueDict v
intersect t1 t2 =
    filter (\k _ -> member k t2) t1


{-| Keep a key-value pair when its key does not appear in the second dictionary.
-}
diff : ValueDict a -> ValueDict b -> ValueDict a
diff t1 t2 =
    foldl (\k _ t -> remove k t) t1 t2


{-| The most general way of combining two dictionaries. You provide three
accumulators for when a given key appears:

1.  Only in the left dictionary.
2.  In both dictionaries.
3.  Only in the right dictionary.

You then traverse all the keys from lowest to highest, building up whatever
you want.

-}
merge :
    (Value -> a -> result -> result)
    -> (Value -> a -> b -> result -> result)
    -> (Value -> b -> result -> result)
    -> ValueDict a
    -> ValueDict b
    -> result
    -> result
merge leftStep bothStep rightStep leftDict rightDict initialResult =
    let
        stepState : Value -> b -> ( List ( Value, a ), result ) -> ( List ( Value, a ), result )
        stepState rKey rValue ( list, result ) =
            case list of
                [] ->
                    ( list, rightStep rKey rValue result )

                ( lKey, lValue ) :: rest ->
                    case Value.compare lKey rKey of
                        LT ->
                            stepState rKey rValue ( rest, leftStep lKey lValue result )

                        GT ->
                            ( list, rightStep rKey rValue result )

                        EQ ->
                            ( rest, bothStep lKey lValue rValue result )

        ( leftovers, intermediateResult ) =
            foldl stepState ( toList leftDict, initialResult ) rightDict
    in
    List.foldl (\( k, v ) result -> leftStep k v result) intermediateResult leftovers



-- TRANSFORM


{-| Apply a function to all values in a dictionary.
-}
map : (Value -> a -> b) -> ValueDict a -> ValueDict b
map func dict =
    case dict of
        RBEmpty_elm_builtin ->
            RBEmpty_elm_builtin

        RBNode_elm_builtin color key value left right ->
            RBNode_elm_builtin color key (func key value) (map func left) (map func right)


{-| Fold over the key-value pairs in a dictionary from lowest key to highest key.

    import Dict exposing (Dict)

    getAges : Dict String User -> List String
    getAges users =
        Dict.foldl addAge [] users

    addAge : String -> User -> List String -> List String
    addAge _ user ages =
        user.age :: ages

    -- getAges users == [33,19,28]

-}
foldl : (Value -> v -> b -> b) -> b -> ValueDict v -> b
foldl func acc dict =
    case dict of
        RBEmpty_elm_builtin ->
            acc

        RBNode_elm_builtin _ key value left right ->
            foldl func (func key value (foldl func acc left)) right


{-| Fold over the key-value pairs in a dictionary from highest key to lowest key.

    import Dict exposing (Dict)

    getAges : Dict String User -> List String
    getAges users =
        Dict.foldr addAge [] users

    addAge : String -> User -> List String -> List String
    addAge _ user ages =
        user.age :: ages

    -- getAges users == [28,19,33]

-}
foldr : (Value -> v -> b -> b) -> b -> ValueDict v -> b
foldr func acc t =
    case t of
        RBEmpty_elm_builtin ->
            acc

        RBNode_elm_builtin _ key value left right ->
            foldr func (func key value (foldr func acc right)) left


{-| Keep only the key-value pairs that pass the given test.
-}
filter : (Value -> v -> Bool) -> ValueDict v -> ValueDict v
filter isGood dict =
    foldl
        (\k v d ->
            if isGood k v then
                insert k v d

            else
                d
        )
        empty
        dict


{-| Partition a dictionary according to some test. The first dictionary
contains all key-value pairs which passed the test, and the second contains
the pairs that did not.
-}
partition : (Value -> v -> Bool) -> ValueDict v -> ( ValueDict v, ValueDict v )
partition isGood dict =
    let
        add : Value -> v -> ( ValueDict v, ValueDict v ) -> ( ValueDict v, ValueDict v )
        add key value ( t1, t2 ) =
            if isGood key value then
                ( insert key value t1, t2 )

            else
                ( t1, insert key value t2 )
    in
    foldl add ( empty, empty ) dict



-- LISTS


{-| Get all of the keys in a dictionary, sorted from lowest to highest.

    keys (fromList [ ( 0, "Alice" ), ( 1, "Bob" ) ]) == [ 0, 1 ]

-}
keys : ValueDict v -> List Value
keys dict =
    foldr (\key _ keyList -> key :: keyList) [] dict


{-| Get all of the values in a dictionary, in the order of their keys.

    values (fromList [ ( 0, "Alice" ), ( 1, "Bob" ) ]) == [ "Alice", "Bob" ]

-}
values : ValueDict v -> List v
values dict =
    foldr (\_ value valueList -> value :: valueList) [] dict


{-| Convert a dictionary into an association list of key-value pairs, sorted by keys.
-}
toList : ValueDict v -> List ( Value, v )
toList dict =
    foldr (\key value list -> ( key, value ) :: list) [] dict


{-| Convert an association list into a dictionary.
-}
fromList : List ( Value, v ) -> ValueDict v
fromList assocs =
    List.foldl (\( key, value ) dict -> insert key value dict) empty assocs
