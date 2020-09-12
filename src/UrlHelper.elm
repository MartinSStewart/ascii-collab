module UrlHelper exposing (..)

import Helper exposing (Coord)
import Quantity exposing (Quantity(..))
import Units exposing (AsciiUnit)
import Url.Builder
import Url.Parser exposing ((<?>))
import Url.Parser.Query


coordQueryParser : Url.Parser.Query.Parser (Coord AsciiUnit)
coordQueryParser =
    Url.Parser.Query.map2
        (\maybeX maybeY ->
            ( Maybe.withDefault 0 maybeX, Maybe.withDefault 0 maybeY ) |> Helper.fromRawCoord
        )
        (Url.Parser.Query.int "x")
        (Url.Parser.Query.int "y")


urlParser : Url.Parser.Parser (Coord AsciiUnit -> b) b
urlParser =
    Url.Parser.top <?> coordQueryParser


encodeUrl : Coord AsciiUnit -> String
encodeUrl ( Quantity x, Quantity y ) =
    Url.Builder.relative [] [ Url.Builder.int "x" x, Url.Builder.int "y" y ]
