module Hyperlink exposing (Hyperlink, Route(..), contains, hyperlinks, routeToUrl, urlsParser)

import Array exposing (Array)
import Ascii exposing (Ascii)
import GridCell
import Helper exposing (Coord)
import List.Extra as List
import Parser exposing ((|.), (|=), Parser, Step(..))
import Quantity exposing (Quantity(..))
import Units exposing (AsciiUnit)
import UrlHelper
import User exposing (UserId)


type alias Hyperlink =
    { position : Coord Units.AsciiUnit, length : Int, route : Route }


type Route
    = External String
    | Internal (Coord AsciiUnit)


routeToUrl : Route -> String
routeToUrl route =
    case route of
        External url ->
            url

        Internal position ->
            UrlHelper.encodeUrl position


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
    [ "www.patorjk.com/software/taag"
    , "ro-box.netlify.app"
    , "the-best-color.lamdera.app"
    , "agirg.com"
    ]


urlPrefixes : List String
urlPrefixes =
    [ "http://"
    , "https://"
    , ""
    ]


hyperlinkFirstChar : List Char
hyperlinkFirstChar =
    hyperlinkWhitelist
        ++ urlPrefixes
        |> List.filterMap (String.uncons >> Maybe.map Tuple.first)
        -- Possible starting chars for coordinate urls
        |> (++) [ 'x', 'a' ]
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

        --|. Parser.chompWhile (\char -> List.all ((/=) char) hyperlinkFirstChar)
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
                    [ Parser.chompIf ((==) '/') |> Parser.getChompedString
                    , Parser.succeed ""
                    ]
                |> Parser.backtrackable
            , Parser.succeed
                (\x y -> Internal (Helper.fromRawCoord ( x, y )))
                |. Parser.oneOf
                    [ Parser.succeed ()
                        |. parseHttp
                        |. Parser.token "ascii-collab.lamdera.app/?"
                    , Parser.succeed ()
                    ]
                |. Parser.token "x="
                |= parseInt
                |. Parser.symbol "&y="
                |= parseInt
            ]
        |= Parser.getCol


parseHttp =
    Parser.oneOf
        (List.map (Parser.token >> Parser.getChompedString) urlPrefixes)


parseInt : Parser Int
parseInt =
    Parser.oneOf
        [ Parser.succeed negate
            |. Parser.symbol "-"
            |= Parser.int
        , Parser.succeed identity
            |= Parser.int
        ]
