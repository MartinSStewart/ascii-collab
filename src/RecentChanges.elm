module RecentChanges exposing (RecentChanges, addChange, init, threeHoursElapsed)

import AssocList
import Helper exposing (Coord, RawCellCoord)
import List.Extra as List
import NotifyMe exposing (Frequency(..), ThreeHours)
import Quantity exposing (Quantity(..))
import Set exposing (Set)
import Units exposing (CellUnit)


type RecentChanges
    = RecentChanges
        { frequencies : AssocList.Dict Frequency (Set RawCellCoord)
        , threeHoursElapsed : Quantity Int ThreeHours
        }


init : RecentChanges
init =
    RecentChanges
        { frequencies = AssocList.empty
        , threeHoursElapsed = Quantity.zero
        }


addChange : Coord CellUnit -> RecentChanges -> RecentChanges
addChange coord (RecentChanges recentChanges) =
    RecentChanges
        { recentChanges
            | frequencies =
                AssocList.update
                    Every3Hours
                    (Maybe.withDefault Set.empty >> Set.insert (Helper.toRawCoord coord) >> Just)
                    recentChanges.frequencies
        }


addChanges : Frequency -> Set RawCellCoord -> RecentChanges -> RecentChanges
addChanges frequency coords (RecentChanges recentChanges) =
    RecentChanges
        { recentChanges
            | frequencies =
                AssocList.update
                    frequency
                    (Maybe.withDefault Set.empty >> Set.union coords >> Just)
                    recentChanges.frequencies
        }


longestDurationReady : Quantity Int ThreeHours -> Frequency
longestDurationReady counter =
    NotifyMe.frequencies
        |> List.sortBy (NotifyMe.duration >> Quantity.unwrap)
        |> List.takeWhile
            (\frequency -> Quantity.modBy (NotifyMe.duration frequency) counter |> (==) Quantity.zero)
        |> List.reverse
        |> List.head
        |> Maybe.withDefault Every3Hours


threeHoursElapsed : RecentChanges -> ( ( Frequency, Set RawCellCoord ), RecentChanges )
threeHoursElapsed (RecentChanges recentChanges) =
    let
        longestDurationReady_ =
            longestDurationReady recentChanges.threeHoursElapsed

        maybeNextLongest : Maybe Frequency
        maybeNextLongest =
            NotifyMe.frequencies
                |> List.sortBy (NotifyMe.duration >> Quantity.unwrap)
                |> List.dropWhile
                    (\frequency ->
                        NotifyMe.duration frequency
                            |> Quantity.lessThan (NotifyMe.duration longestDurationReady_)
                    )
                |> List.head

        changes =
            recentChanges.frequencies
                |> AssocList.filter
                    (\frequency _ ->
                        NotifyMe.duration frequency
                            |> Quantity.lessThanOrEqualTo (NotifyMe.duration longestDurationReady_)
                    )
                |> AssocList.values
                |> List.foldl Set.union Set.empty

        newRecentChanges =
            RecentChanges
                { recentChanges
                    | frequencies =
                        recentChanges.frequencies
                            |> AssocList.filter
                                (\frequency _ ->
                                    NotifyMe.duration frequency
                                        |> Quantity.greaterThan (NotifyMe.duration longestDurationReady_)
                                )
                    , threeHoursElapsed = Quantity.plus recentChanges.threeHoursElapsed (Quantity 1)
                }
    in
    ( ( longestDurationReady_, changes )
    , case maybeNextLongest of
        Just nextLongest ->
            addChanges nextLongest changes newRecentChanges

        Nothing ->
            newRecentChanges
    )
