module Grid exposing (Grid, ServerGrid, Vertex, addChange, addChange_, asciiBox, changeCount, empty, empty_, mesh, meshes, toGrid)

import Array
import Ascii exposing (Ascii)
import Dict exposing (Dict)
import GridCell exposing (Cell)
import Helper exposing (Coord)
import List.Extra as List
import List.Nonempty exposing (Nonempty(..))
import Math.Vector2 exposing (Vec2)
import Pixels
import Quantity exposing (Quantity(..))
import Serialize
import Units
import User exposing (UserId)
import WebGL


type Grid
    = Grid (Dict ( Int, Int ) { cell : Cell, mesh : Maybe (WebGL.Mesh Vertex) })


type ServerGrid
    = ServerGrid (Dict ( Int, Int ) Cell)


toGrid : ServerGrid -> Grid
toGrid (ServerGrid serverGrid) =
    Dict.map
        (\( x, y ) cell ->
            { cell = cell
            , mesh = GridCell.flatten cell |> Array.toList |> mesh ( Units.cellUnit x, Units.cellUnit y ) |> Just
            }
        )
        serverGrid
        |> Grid


type alias Vertex =
    { position : Vec2, texturePosition : Vec2 }


empty : Grid
empty =
    Grid Dict.empty


empty_ : ServerGrid
empty_ =
    ServerGrid Dict.empty


asciiToCellCoord : Coord Units.AsciiUnit -> Coord Units.CellUnit
asciiToCellCoord ( Quantity x, Quantity y ) =
    let
        offset =
            1000000
    in
    ( (x + (GridCell.cellSize * offset)) // GridCell.cellSize - offset |> Quantity
    , (y + (GridCell.cellSize * offset)) // GridCell.cellSize - offset |> Quantity
    )


asciiToLocalCoord : Coord Units.AsciiUnit -> Int
asciiToLocalCoord ( Quantity x, Quantity y ) =
    modBy GridCell.cellSize x + modBy GridCell.cellSize y * GridCell.cellSize


addChange : UserId -> Coord Units.AsciiUnit -> Nonempty (List Ascii) -> Grid -> Grid
addChange userId asciiCoord lines grid =
    List.Nonempty.toList lines
        |> List.indexedMap Tuple.pair
        |> List.filterMap
            (\( yOffset, line ) ->
                case List.Nonempty.fromList line of
                    Just line_ ->
                        splitUpLine asciiCoord (Units.asciiUnit yOffset) line_ |> Just

                    Nothing ->
                        Nothing
            )
        |> List.concat
        |> List.gatherEqualsBy (Tuple.first >> asciiToCellCoord)
        |> List.foldl
            (\( head, rest ) state -> addLines userId (Nonempty head rest) state)
            grid


changeCount : Coord Units.CellUnit -> ServerGrid -> Int
changeCount ( Quantity x, Quantity y ) (ServerGrid grid) =
    case Dict.get ( x, y ) grid of
        Just cell ->
            GridCell.changeCount cell

        Nothing ->
            0


addChange_ : UserId -> Coord Units.CellUnit -> Int -> Nonempty Ascii -> ServerGrid -> ServerGrid
addChange_ userId ( Quantity x, Quantity y ) localPosition change (ServerGrid grid) =
    Dict.get ( x, y ) grid
        |> Maybe.withDefault GridCell.empty
        |> GridCell.addLine userId localPosition change
        |> (\cell -> Dict.insert ( x, y ) cell grid)
        |> ServerGrid


splitUpLine :
    Coord Units.AsciiUnit
    -> Quantity Int Units.AsciiUnit
    -> Nonempty Ascii
    -> List ( Coord Units.AsciiUnit, Nonempty Ascii )
splitUpLine asciiCoord offsetY line =
    let
        (Quantity.Quantity x) =
            Tuple.first asciiCoord

        splitIndex =
            GridCell.cellSize - modBy GridCell.cellSize x

        ( head, rest ) =
            List.splitAt splitIndex (List.Nonempty.toList line)

        restCoord index =
            Helper.addTuple asciiCoord ( Units.asciiUnit <| splitIndex + GridCell.cellSize * index, offsetY )
    in
    List.greedyGroupsOf GridCell.cellSize rest
        |> List.indexedMap (\index value -> ( restCoord index, value ))
        |> (::) ( Helper.addTuple asciiCoord ( Quantity.zero, offsetY ), head )
        |> List.filterMap (\( position, value ) -> List.Nonempty.fromList value |> Maybe.map (Tuple.pair position))


addLines :
    UserId
    -> Nonempty ( Coord Units.AsciiUnit, Nonempty Ascii )
    -> Grid
    -> Grid
addLines userId lines state =
    let
        cellCoord =
            List.Nonempty.head lines |> Tuple.first |> asciiToCellCoord
    in
    List.Nonempty.foldl
        (\( position_, cellLine ) cell ->
            GridCell.addLine userId (asciiToLocalCoord position_) cellLine cell
        )
        (getCell cellCoord state |> Maybe.withDefault GridCell.empty)
        lines
        |> (\cell -> setCell cellCoord cell state)


getCell : Coord Units.CellUnit -> Grid -> Maybe Cell
getCell ( Quantity x, Quantity y ) (Grid grid) =
    Dict.get ( x, y ) grid |> Maybe.map .cell


setCell : Coord Units.CellUnit -> Cell -> Grid -> Grid
setCell (( Quantity x, Quantity y ) as position) value (Grid grid) =
    Dict.insert ( x, y ) { cell = value, mesh = GridCell.flatten value |> Array.toList |> mesh position |> Just } grid |> Grid


cellCodec : Serialize.Codec Ascii.CodecError (List Ascii)
cellCodec =
    Serialize.list Ascii.codec


baseMesh : { boxes : List (List Vec2), indices : List ( Int, Int, Int ) }
baseMesh =
    let
        ( Quantity w, Quantity h ) =
            Ascii.size
    in
    List.range 0 (GridCell.cellSize * GridCell.cellSize)
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


mesh : Coord Units.CellUnit -> List Ascii -> WebGL.Mesh { position : Vec2, texturePosition : Vec2 }
mesh ( Quantity.Quantity x, Quantity.Quantity y ) asciiValues =
    List.map2
        (\ascii box ->
            let
                { topLeft, bottomRight } =
                    Ascii.texturePosition ascii

                ( w, h ) =
                    Ascii.size
            in
            List.map2
                (\v uv ->
                    { position =
                        Math.Vector2.add
                            (Math.Vector2.vec2
                                (x * GridCell.cellSize * Pixels.inPixels w |> toFloat)
                                (y * GridCell.cellSize * Pixels.inPixels h |> toFloat)
                            )
                            v
                    , texturePosition = uv
                    }
                )
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


meshes : Grid -> List (WebGL.Mesh Vertex)
meshes (Grid grid) =
    Dict.toList grid |> List.filterMap (Tuple.second >> .mesh)
