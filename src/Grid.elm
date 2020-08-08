module Grid exposing (AsciiUnit, CellUnit, Grid, Vertex, empty, mesh, setAscii)

import Ascii exposing (Ascii)
import Dict exposing (Dict)
import GridCell exposing (Cell)
import Math.Vector2 exposing (Vec2)
import Quantity exposing (Quantity(..))
import Serialize
import User exposing (UserId)
import WebGL


type Grid
    = Grid (Dict ( Int, Int ) { cell : Cell, mesh : Maybe (WebGL.Mesh Vertex) })


type alias Vertex =
    { position : Vec2, texturePosition : Vec2 }


type AsciiUnit
    = AsciiUnit Never


type CellUnit
    = CellUnit Never


empty : Grid
empty =
    Grid Dict.empty


asciiToCellCoord : ( Quantity Int AsciiUnit, Quantity Int AsciiUnit ) -> ( Quantity Int CellUnit, Quantity Int CellUnit )
asciiToCellCoord ( Quantity x, Quantity y ) =
    ( x // GridCell.cellSize |> Quantity, y // GridCell.cellSize |> Quantity )


asciiToLocalCoord : ( Quantity Int AsciiUnit, Quantity Int AsciiUnit ) -> Int
asciiToLocalCoord ( Quantity x, Quantity y ) =
    modBy GridCell.cellSize x + modBy GridCell.cellSize y * GridCell.cellSize


setAscii : UserId -> ( Quantity Int AsciiUnit, Quantity Int AsciiUnit ) -> Ascii -> Grid -> Grid
setAscii userId asciiCoord ascii grid =
    let
        cellCoord =
            asciiToCellCoord asciiCoord
    in
    getCell cellCoord grid
        |> Maybe.withDefault GridCell.empty
        |> GridCell.addChange userId (asciiToLocalCoord asciiCoord) ascii
        |> (\cell -> setCell cellCoord cell grid)


getCell : ( Quantity Int CellUnit, Quantity Int CellUnit ) -> Grid -> Maybe Cell
getCell ( Quantity x, Quantity y ) (Grid grid) =
    Dict.get ( x, y ) grid |> Maybe.map .cell


setCell : ( Quantity Int CellUnit, Quantity Int CellUnit ) -> Cell -> Grid -> Grid
setCell ( Quantity x, Quantity y ) value (Grid grid) =
    Dict.insert ( x, y ) { cell = value, mesh = Nothing } grid |> Grid


cellCodec : Serialize.Codec Ascii.CodecError (List Ascii)
cellCodec =
    Serialize.list Ascii.codec


baseMesh : { boxes : List (List Vec2), indices : List ( Int, Int, Int ) }
baseMesh =
    let
        ( Quantity w, Quantity h ) =
            Ascii.size
    in
    List.range 0 (GridCell.cellSize * GridCell.cellSize - 1)
        |> List.foldl
            (\index { boxes, indices } ->
                let
                    box =
                        asciiBox (modBy GridCell.cellSize index |> (*) w) (index // GridCell.cellSize |> (*) h) (index * 4)
                in
                { boxes = boxes ++ [ box.vertices ], indices = indices ++ box.indices }
            )
            { boxes = [], indices = [] }


asciiBox : Int -> Int -> Int -> { vertices : List Vec2, indices : List ( Int, Int, Int ) }
asciiBox offsetX offsetY indexOffset =
    let
        ( Quantity w, Quantity h ) =
            Ascii.size
    in
    { vertices =
        [ Math.Vector2.vec2 (toFloat offsetX) (toFloat offsetY)
        , Math.Vector2.vec2 (toFloat (offsetX + w)) (toFloat offsetY)
        , Math.Vector2.vec2 (toFloat (offsetX + w)) (toFloat (offsetY + h))
        , Math.Vector2.vec2 (toFloat offsetX) (toFloat (offsetY + h))
        ]
    , indices = [ ( indexOffset + 3, indexOffset + 1, indexOffset ), ( indexOffset + 2, indexOffset + 1, indexOffset + 3 ) ]
    }


mesh : List Ascii -> WebGL.Mesh { position : Vec2, texturePosition : Vec2 }
mesh asciiValues =
    List.map2
        (\ascii box ->
            let
                { topLeft, bottomRight } =
                    Ascii.texturePosition ascii
            in
            List.map2 (\v uv -> { position = v, texturePosition = uv })
                box
                [ topLeft
                , Math.Vector2.vec2 (Math.Vector2.getX bottomRight) (Math.Vector2.getY topLeft)
                , bottomRight
                , Math.Vector2.vec2 (Math.Vector2.getX topLeft) (Math.Vector2.getY bottomRight)
                ]
        )
        asciiValues
        baseMesh.boxes
        |> List.concat
        |> (\vertices -> WebGL.indexedTriangles vertices baseMesh.indices)
