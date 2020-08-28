module Grid exposing (Change, Grid, LocalChange, Vertex, addChange, allCells, allCellsDict, asciiBox, asciiToCellAndLocalCoord, changeCount, empty, from, getCell, localChangeToChange, mesh, setUndoPoints, textToChange, undoPoint)

import Ascii exposing (Ascii)
import Dict exposing (Dict)
import GridCell exposing (Cell)
import Helper exposing (Coord)
import List.Extra as List
import List.Nonempty exposing (Nonempty(..))
import Math.Vector2 exposing (Vec2)
import Pixels
import Quantity exposing (Quantity(..))
import Units exposing (CellUnit)
import User exposing (UserId)
import WebGL


type Grid
    = Grid (Dict ( Int, Int ) Cell)


empty : Grid
empty =
    Grid Dict.empty


from : Dict ( Int, Int ) Cell -> Grid
from =
    Grid


asciiToCellAndLocalCoord : Coord Units.AsciiUnit -> ( Coord Units.CellUnit, Int )
asciiToCellAndLocalCoord ( Quantity x, Quantity y ) =
    let
        offset =
            1000000
    in
    ( ( (x + (GridCell.cellSize * offset)) // GridCell.cellSize - offset |> Quantity
      , (y + (GridCell.cellSize * offset)) // GridCell.cellSize - offset |> Quantity
      )
    , modBy GridCell.cellSize x + modBy GridCell.cellSize y * GridCell.cellSize
    )


type alias Change =
    { cellPosition : Coord Units.CellUnit, localPosition : Int, change : Nonempty Ascii, userId : UserId }


type alias LocalChange =
    { cellPosition : Coord Units.CellUnit, localPosition : Int, change : Nonempty Ascii }


localChangeToChange : UserId -> LocalChange -> Change
localChangeToChange userId change_ =
    { cellPosition = change_.cellPosition
    , localPosition = change_.localPosition
    , change = change_.change
    , userId = userId
    }


textToChange : Coord Units.AsciiUnit -> Nonempty (List Ascii) -> Nonempty LocalChange
textToChange asciiCoord lines =
    List.Nonempty.toList lines
        |> List.indexedMap Tuple.pair
        |> List.filterMap
            (\( yOffset, line ) ->
                case List.Nonempty.fromList line of
                    Just line_ ->
                        splitUpLine asciiCoord (Units.asciiUnit yOffset) line_
                            |> List.map
                                (\( pos, change ) ->
                                    let
                                        ( cellPosition, localPosition ) =
                                            asciiToCellAndLocalCoord pos
                                    in
                                    { cellPosition = cellPosition
                                    , localPosition = localPosition
                                    , change = change
                                    }
                                )
                            |> Just

                    Nothing ->
                        Nothing
            )
        |> List.concat
        |> List.Nonempty.fromList
        -- This should never happen
        |> Maybe.withDefault
            (List.Nonempty.fromElement
                { cellPosition = ( Units.cellUnit 0, Units.cellUnit 0 )
                , localPosition = 0
                , change = List.Nonempty.fromElement Ascii.default
                }
            )


setUndoPoints : UserId -> Dict ( Int, Int ) Int -> Grid -> Grid
setUndoPoints userId undoPoints (Grid grid) =
    Dict.map
        (\coord cell ->
            let
                undoPoint_ =
                    Dict.get coord undoPoints |> Maybe.withDefault 0
            in
            GridCell.setUndoPoint userId undoPoint_ cell
        )
        grid
        |> Grid


undoPoint : UserId -> Grid -> Dict ( Int, Int ) Int
undoPoint userId (Grid grid) =
    Dict.toList grid
        |> List.filterMap (\( coord, cell ) -> GridCell.undoPoint userId cell |> Maybe.map (Tuple.pair coord))
        |> Dict.fromList


changeCount : Coord Units.CellUnit -> Grid -> Int
changeCount ( Quantity x, Quantity y ) (Grid grid) =
    case Dict.get ( x, y ) grid of
        Just cell ->
            GridCell.changeCount cell

        Nothing ->
            0


addChange : Change -> Grid -> Grid
addChange change grid =
    let
        cell : Cell
        cell =
            getCell change.cellPosition grid
                |> Maybe.withDefault GridCell.empty
    in
    GridCell.addLine change.userId change.localPosition change.change cell
        |> (\cell_ -> setCell change.cellPosition cell_ grid)


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


allCells : Grid -> List ( Coord CellUnit, Cell )
allCells (Grid grid) =
    Dict.toList grid |> List.map (Tuple.mapFirst (\( x, y ) -> ( Units.cellUnit x, Units.cellUnit y )))


allCellsDict : Grid -> Dict ( Int, Int ) Cell
allCellsDict (Grid grid) =
    grid


getCell : Coord Units.CellUnit -> Grid -> Maybe Cell
getCell ( Quantity x, Quantity y ) (Grid grid) =
    Dict.get ( x, y ) grid


setCell : Coord Units.CellUnit -> Cell -> Grid -> Grid
setCell ( Quantity x, Quantity y ) value (Grid grid) =
    Dict.insert ( x, y ) value grid |> Grid


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


type alias Vertex =
    { position : Vec2, texturePosition : Vec2, userId : Float }


mesh : Coord Units.CellUnit -> List ( Maybe UserId, Ascii ) -> WebGL.Mesh Vertex
mesh ( Quantity.Quantity x, Quantity.Quantity y ) asciiValues =
    List.map2
        (\( userId, ascii ) box ->
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
                    , userId = userId |> Maybe.map User.rawId |> Maybe.withDefault -1 |> toFloat
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
