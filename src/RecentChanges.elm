module RecentChanges exposing (RecentChanges, addChange, init, threeHoursElapsed, undoRedoChange)

import AssocList
import Dict exposing (Dict)
import Grid exposing (Grid)
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
                    (Maybe.withDefault Dict.empty
                        >> Dict.update (Helper.toRawCoord coord) (Maybe.withDefault originalCell >> Just)
                        >> Just
                    )
                    recentChanges.frequencies
        }


undoRedoChange : Dict.Dict RawCellCoord Int -> Grid -> RecentChanges -> RecentChanges
undoRedoChange changes grid recentChanges =
    addChanges
        Every3Hours
        (Dict.map
            (\key _ -> Grid.getCell (Helper.fromRawCoord key) grid |> Maybe.withDefault GridCell.empty)
            changes
        )
        recentChanges


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
        |> Quantity.sortBy NotifyMe.duration
        |> List.takeWhile
            (\frequency -> Quantity.modBy (NotifyMe.duration frequency) counter |> (==) Quantity.zero)
        |> List.reverse
        |> List.head
        |> Maybe.withDefault Every3Hours


threeHoursElapsed : RecentChanges -> ( List ( Frequency, Dict RawCellCoord GridCell.Cell ), RecentChanges )
threeHoursElapsed (RecentChanges recentChanges) =
    let
        longestFrequencyReady_ =
            longestDurationReady recentChanges.threeHoursElapsed

        maybeNextLongest : Maybe Frequency
        maybeNextLongest =
            NotifyMe.frequencies
                |> Quantity.sortBy NotifyMe.duration
                |> List.dropWhile
                    (\frequency ->
                        NotifyMe.duration frequency
                            |> Quantity.lessThanOrEqualTo (NotifyMe.duration longestFrequencyReady_)
                    )
                |> List.head

        allReadyFrequencies : AssocList.Dict Frequency (Dict RawCellCoord GridCell.Cell)
        allReadyFrequencies =
            recentChanges.frequencies
                |> AssocList.filter
                    (\frequency _ ->
                        NotifyMe.duration frequency
                            |> Quantity.lessThanOrEqualTo (NotifyMe.duration longestFrequencyReady_)
                    )

        changes : List ( Frequency, Dict RawCellCoord GridCell.Cell )
        changes =
            allReadyFrequencies
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

        newRecentChanges : RecentChanges
        newRecentChanges =
            RecentChanges
                { recentChanges
                    | frequencies =
                        recentChanges.frequencies
                            |> AssocList.filter
                                (\frequency _ ->
                                    NotifyMe.duration frequency
                                        |> Quantity.greaterThan (NotifyMe.duration longestFrequencyReady_)
                                )
                    , threeHoursElapsed = Quantity.plus recentChanges.threeHoursElapsed (Quantity 1)
                }
    in
    ( changes
    , case maybeNextLongest of
        Just nextLongest ->
            addChanges
                nextLongest
                (List.foldl (\( _, value ) dict -> Dict.union value dict) Dict.empty changes)
                newRecentChanges

        Nothing ->
            newRecentChanges
    )
