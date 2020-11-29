module Hyperlink exposing (Hyperlink, hyperlinks, urlsParser)

import Array exposing (Array)
import Ascii exposing (Ascii)
import GridCell
import Helper exposing (Coord)
import Parser exposing ((|.), (|=), Parser, Step(..))
import Quantity exposing (Quantity(..))
import Units exposing (AsciiUnit)
import UrlHelper
import User exposing (UserId)


type alias Hyperlink =
    { position : Coord Units.AsciiUnit, length : Int, url : String }


hyperlinks : Coord Units.AsciiUnit -> List (Maybe (Array ( Maybe UserId, Ascii ))) -> List Hyperlink
hyperlinks minPoint flattenedCellRow =
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
                    |> Parser.run (urlsParser (Helper.addTuple minPoint ( Quantity.zero, Quantity index )))
                    |> Result.toMaybe
                    |> Maybe.withDefault []
            )
        |> List.concat


hyperlinkWhitelist : List String
hyperlinkWhitelist =
    [ "www.patorjk.com/software/taag"
    , "ro-box.netlify.app"
    , "the-best-color.lamdera.app"
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
        |> (::) 'x'


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


urlParser : Coord Units.AsciiUnit -> Parser Hyperlink
urlParser offset =
    Parser.succeed
        (\start url end ->
            { position = Helper.addTuple offset ( Quantity start, Quantity.zero )
            , length = end - start
            , url = url
            }
        )
        |= Parser.getCol
        |= Parser.oneOf
            [ Parser.succeed (\a b c -> a ++ b ++ c)
                |= Parser.oneOf
                    (List.map (Parser.token >> Parser.getChompedString) urlPrefixes)
                |= Parser.oneOf
                    (List.map (Parser.token >> Parser.getChompedString) hyperlinkWhitelist)
                |= Parser.oneOf
                    [ Parser.chompIf ((==) '/') |> Parser.getChompedString
                    , Parser.succeed ""
                    ]
            , Parser.succeed
                (\x y -> UrlHelper.encodeUrl (Helper.fromRawCoord ( x, y )))
                |. Parser.token "x="
                |= parseInt
                |. Parser.symbol "&y="
                |= parseInt
            ]
        |= Parser.getCol


parseInt : Parser Int
parseInt =
    Parser.oneOf
        [ Parser.succeed negate
            |. Parser.symbol "-"
            |= Parser.int
        , Parser.succeed identity
            |= Parser.int
        ]
