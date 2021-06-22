module Hyperlink exposing (Hyperlink, Route(..), contains, hyperlinks, routeToUrl, urlsParser)

import Array exposing (Array)
import Ascii exposing (Ascii)
import Env
import GridCell
import Helper exposing (Coord)
import List.Extra as List
import Parser exposing ((|.), (|=), Parser, Step(..))
import Quantity exposing (Quantity(..))
import Units exposing (AsciiUnit)
import Url.Builder
import UrlHelper
import User exposing (UserId)


type alias Hyperlink =
    { position : Coord Units.AsciiUnit, length : Int, route : Route }


type Route
    = External String
    | Coordinate (Coord Units.AsciiUnit)
    | NotifyMe
    | Resource String


routeToUrl : { showNotifyMe : Bool, viewPoint : Coord Units.AsciiUnit } -> Route -> String
routeToUrl currentRoute route =
    case route of
        External url ->
            url

        Coordinate viewPoint ->
            UrlHelper.encodeUrl (UrlHelper.internalRoute currentRoute.showNotifyMe viewPoint)

        NotifyMe ->
            UrlHelper.encodeUrl (UrlHelper.internalRoute True currentRoute.viewPoint)

        Resource resource ->
            Url.Builder.relative [ resource ] []


hyperlinks : Coord Units.CellUnit -> List (Maybe (Array ( Maybe UserId, Ascii ))) -> List Hyperlink
hyperlinks offset flattenedCellRow =
    let
        asciiOffset =
            Units.cellToAscii offset
    in
    List.range 0 (GridCell.cellSize - 1)
        |> List.map
            (\index ->
                List.map
                    (\maybeCell ->
                        case maybeCell of
                            Just cell ->
                                Array.slice
                                    (index * GridCell.cellSize)
                                    ((index + 1) * GridCell.cellSize)
                                    cell
                                    |> Array.toList
                                    |> List.map (Tuple.second >> Ascii.toChar)
                                    |> String.fromList

                            Nothing ->
                                String.repeat 16 " "
                    )
                    flattenedCellRow
                    |> String.concat
                    |> Parser.run (urlsParser (Helper.addTuple asciiOffset ( Quantity.zero, Quantity index )))
                    |> Result.toMaybe
                    |> Maybe.withDefault []
            )
        |> List.concat


hyperlinkWhitelist : List String
hyperlinkWhitelist =
    Env.hyperlinkWhitelist |> String.split "," |> List.map String.trim


hyperlinkFirstChar : List Char
hyperlinkFirstChar =
    hyperlinkWhitelist
        |> List.filterMap (String.uncons >> Maybe.map Tuple.first)
        |> (++) [ 'x', 'a', 'h' ]
        |> List.unique


urlsParser : Coord Units.AsciiUnit -> Parser (List Hyperlink)
urlsParser offset =
    Parser.loop [] (urlsParserHelper offset)


urlsParserHelper : Coord Units.AsciiUnit -> List Hyperlink -> Parser (Step (List Hyperlink) (List Hyperlink))
urlsParserHelper offset links =
    Parser.oneOf
        [ Parser.succeed (Done links)
            |. Parser.end
        , Parser.succeed (\newLink -> Loop (newLink :: links))
            |= urlParser offset
            |> Parser.backtrackable
        , Parser.succeed (Loop links)
            |. Parser.chompIf (\_ -> True)
            |. Parser.chompWhile (\char -> List.all ((/=) char) hyperlinkFirstChar)
        ]


contains : Coord Units.AsciiUnit -> Hyperlink -> Bool
contains coord hyperlink =
    let
        ( x, y ) =
            Helper.toRawCoord hyperlink.position

        ( cx, cy ) =
            Helper.toRawCoord coord
    in
    y == cy && x <= cx && cx < x + hyperlink.length


urlParser : Coord Units.AsciiUnit -> Parser Hyperlink
urlParser offset =
    Parser.succeed
        (\( startRow, startColumn ) route end ->
            { position = Helper.addTuple offset ( Quantity (startColumn - 1), Quantity (startRow - 1) )
            , length = end - startColumn
            , route = route
            }
        )
        |= Parser.getPosition
        |= Parser.oneOf
            [ Parser.succeed (\a b c -> a ++ b ++ c |> External)
                |= parseHttp
                |= Parser.oneOf
                    (List.map (Parser.token >> Parser.getChompedString) hyperlinkWhitelist)
                |= Parser.oneOf
                    [ Parser.succeed (++)
                        |= (Parser.chompIf ((==) '/') |> Parser.getChompedString)
                        |= (Parser.chompWhile (\char -> char /= '.' && char /= ',' && char /= ' ' && char /= '\\')
                                |> Parser.getChompedString
                           )
                    , Parser.succeed ""
                    ]
                |> Parser.backtrackable
            , Parser.succeed identity
                |. parseHttp
                |. Parser.token "ascii-collab.lamdera.app/"
                |= Parser.oneOf
                    [ Parser.succeed Coordinate
                        |. Parser.token "?"
                        |= parseCoordinate
                    , Parser.succeed NotifyMe
                        |. Parser.token UrlHelper.notifyMe
                        |. Parser.oneOf
                            [ Parser.chompIf ((==) '/')
                            , Parser.succeed ()
                            ]
                    , Parser.succeed Resource
                        |= Parser.oneOf
                            [ Parser.token "poster.png" |> Parser.getChompedString
                            , Parser.token "poster-color.png" |> Parser.getChompedString
                            , Parser.token "poster-irl.jpg" |> Parser.getChompedString
                            , Parser.token "mug-front.jpg" |> Parser.getChompedString
                            , Parser.token "mug-back.jpg" |> Parser.getChompedString
                            ]
                        |. Parser.oneOf
                            [ Parser.chompIf ((==) '/')
                            , Parser.succeed ()
                            ]
                    ]
            , Parser.succeed Coordinate
                |= parseCoordinate
            ]
        |= Parser.getCol


parseHttp : Parser String
parseHttp =
    Parser.oneOf
        [ Parser.succeed "https://"
            |. Parser.token "https://"
        , Parser.succeed "http://"
            |. Parser.token "http://"
        , Parser.succeed "https://"
        ]


parseCoordinate : Parser (Coord units)
parseCoordinate =
    Parser.succeed (\x y -> Helper.fromRawCoord ( x, y ))
        |. Parser.token "x="
        |= parseInt
        |. Parser.symbol "&y="
        |= parseInt


parseInt : Parser Int
parseInt =
    Parser.succeed
        (\isNegated value ->
            if isNegated then
                -value

            else
                value
        )
        |= Parser.oneOf
            [ Parser.succeed True
                |. Parser.token "-"
            , Parser.succeed False
            ]
        |= (Parser.chompWhile Char.isDigit
                |> Parser.getChompedString
                |> Parser.andThen
                    (\text ->
                        if String.length text < 9 then
                            case String.toInt text of
                                Just int ->
                                    Parser.succeed int

                                Nothing ->
                                    Parser.problem "Invalid int"

                        else
                            Parser.problem "Invalid int"
                    )
           )
