module RecentChanges exposing (RecentChanges, addChange, init, threeHoursElapsed)

import AssocList
import Dict exposing (Dict)
import GridCell
import Helper exposing (Coord, RawCellCoord)
import List.Extra as List
import List.Nonempty exposing (Nonempty)
import NotifyMe exposing (Frequency(..), ThreeHours)
import Quantity exposing (Quantity(..))
import Units exposing (CellUnit)


type RecentChanges
    = RecentChanges
        { frequencies : AssocList.Dict Frequency (Dict RawCellCoord GridCell.Cell)
        , threeHoursElapsed : Quantity Int ThreeHours
        }


init : RecentChanges
init =
    RecentChanges
        { frequencies = AssocList.empty
        , threeHoursElapsed = Quantity.zero
        }


addChange : Coord CellUnit -> GridCell.Cell -> RecentChanges -> RecentChanges
addChange coord originalCell (RecentChanges recentChanges) =
    RecentChanges
        { recentChanges
            | frequencies =
                AssocList.update
                    Every3Hours
                    (Maybe.withDefault Dict.empty >> Dict.insert (Helper.toRawCoord coord) originalCell >> Just)
                    recentChanges.frequencies
        }


addChanges : Frequency -> Dict RawCellCoord GridCell.Cell -> RecentChanges -> RecentChanges
addChanges frequency coords (RecentChanges recentChanges) =
    RecentChanges
        { recentChanges
            | frequencies =
                AssocList.update
                    frequency
                    (\maybeDict -> Dict.union (Maybe.withDefault Dict.empty maybeDict) coords |> Just)
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


threeHoursElapsed : RecentChanges -> ( Nonempty ( Frequency, Dict RawCellCoord GridCell.Cell ), RecentChanges )
threeHoursElapsed (RecentChanges recentChanges) =
    let
        longestDurationReady_ =
            longestDurationReady recentChanges.threeHoursElapsed

        maybeNextLongest : Maybe Frequency
        maybeNextLongest =
            NotifyMe.frequencies
                |> Quantity.sortBy NotifyMe.duration
                |> List.dropWhile
                    (\frequency ->
                        NotifyMe.duration frequency
                            |> Quantity.lessThan (NotifyMe.duration longestDurationReady_)
                    )
                |> List.head

        changes : Nonempty ( Frequency, Dict RawCellCoord GridCell.Cell )
        changes =
            recentChanges.frequencies
                |> AssocList.filter
                    (\frequency _ ->
                        NotifyMe.duration frequency
                            |> Quantity.lessThanOrEqualTo (NotifyMe.duration longestDurationReady_)
                    )
                |> AssocList.toList
                |> Quantity.sortBy (Tuple.first >> NotifyMe.duration)
                |> List.foldl
                    (\( frequency, change ) list ->
                        case list of
                            ( _, previousChange ) :: _ ->
                                ( frequency, Dict.union change previousChange ) :: list

                            [] ->
                                ( frequency, change ) :: list
                    )
                    []
                |> List.Nonempty.fromList
                |> Maybe.withDefault (List.Nonempty.fromElement ( Every3Hours, Dict.empty ))

        newRecentChanges : RecentChanges
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
    ( changes
    , case maybeNextLongest of
        Just nextLongest ->
            addChanges
                nextLongest
                (List.Nonempty.foldl (\( _, value ) dict -> Dict.union value dict) Dict.empty changes)
                newRecentChanges

        Nothing ->
            newRecentChanges
    )
