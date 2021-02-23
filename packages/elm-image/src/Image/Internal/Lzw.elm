module Image.Internal.Lzw exposing (decodeGifList, decoder, encodeGifList)

import Bytes.Decode as D exposing (Step(..))
import Dict
import Image.Internal.BitReader as BitReader
import Image.Internal.BitWriter as BitWriter


decoder : Int -> Int -> Int -> D.Decoder (List Int)
decoder size firstCodeSize count_firstBlock =
    D.bytes count_firstBlock
        |> D.andThen
            (\bytes ->
                let
                    ( cc, eoi, table ) =
                        initDecodeTable size
                in
                BitReader.readBits firstCodeSize 0
                    |> BitReader.andThen
                        (\reset ->
                            if reset /= cc then
                                BitReader.error "No reset Bit present"

                            else
                                BitReader.readBits firstCodeSize 0
                                    |> BitReader.andThen
                                        (\first ->
                                            let
                                                value =
                                                    Dict.get first table
                                                        |> Maybe.withDefault []

                                                acc =
                                                    { eoi = eoi
                                                    , table = table
                                                    , indexStream = value
                                                    , indexBuffer = value
                                                    , read = firstCodeSize
                                                    }
                                            in
                                            BitReader.loop acc bitDecoder
                                        )
                        )
                    |> BitReader.decode bytes
                    |> Result.toMaybe
                    |> Maybe.map D.succeed
                    |> Maybe.withDefault D.fail
            )


bitDecoder ({ eoi, table, indexStream, read, indexBuffer } as acc) =
    BitReader.readBits read 0
        |> BitReader.map
            (\code ->
                if code /= eoi && (2 ^ read - 1) < 4095 then
                    case Dict.get code table of
                        Just v ->
                            let
                                k =
                                    v
                                        |> List.head
                                        |> Maybe.withDefault -404

                                tableValue =
                                    indexBuffer ++ [ k ]

                                tableKey =
                                    Dict.size table

                                newRead =
                                    if tableKey >= 2 ^ read - 1 then
                                        read + 1

                                    else
                                        read
                            in
                            Loop
                                { acc
                                    | table = Dict.insert tableKey tableValue table
                                    , indexStream = indexStream ++ v
                                    , indexBuffer = v
                                    , read = newRead
                                }

                        Nothing ->
                            let
                                k =
                                    indexBuffer
                                        |> List.head
                                        |> Maybe.withDefault -404

                                tableValue =
                                    indexBuffer ++ [ k ]

                                newRead =
                                    if code >= 2 ^ read - 1 then
                                        read + 1

                                    else
                                        read
                            in
                            Loop
                                { acc
                                    | table = Dict.insert code tableValue table
                                    , indexStream = indexStream ++ tableValue
                                    , indexBuffer = tableValue
                                    , read = newRead
                                }

                else
                    Done indexStream
            )


decodeGifList size data =
    let
        ( cc, eoi, table ) =
            initDecodeTable size
    in
    case data of
        _ :: first :: rest ->
            let
                value =
                    Dict.get first table |> Maybe.withDefault []
            in
            decodeGifList_ eoi table value rest value

        _ ->
            []


decodeGifList_ eoi table indexStream codeStream code__1 =
    case codeStream of
        code :: rest ->
            if code == eoi then
                indexStream

            else
                case Dict.get code table of
                    Just v ->
                        let
                            k =
                                v
                                    |> List.head
                                    |> Maybe.withDefault -404

                            tableValue =
                                code__1 ++ [ k ]

                            tableKey =
                                Dict.size table
                        in
                        decodeGifList_ eoi (Dict.insert tableKey tableValue table) (indexStream ++ v) rest v

                    Nothing ->
                        let
                            k =
                                code__1
                                    |> List.head
                                    |> Maybe.withDefault -404

                            tableValue =
                                code__1 ++ [ k ]
                        in
                        decodeGifList_ eoi (Dict.insert code tableValue table) (indexStream ++ tableValue) rest tableValue

        _ ->
            indexStream


initDecodeTable : Int -> ( Int, Int, Dict.Dict Int (List Int) )
initDecodeTable size =
    let
        cc =
            size + 1

        eoi =
            size + 2

        table =
            List.range 0 size
                |> List.foldl (\k -> Dict.insert k [ k ]) Dict.empty
                |> Dict.insert cc [ cc ]
                |> Dict.insert eoi [ eoi ]
    in
    ( cc, eoi, table )


encodeGifList_ eoi table indexStream codeStream indexBuffer =
    --https://www.matthewflickinger.com/lab/whatsinagif/lzw_image_data.asp
    case indexStream of
        k :: rest ->
            let
                key =
                    indexBuffer ++ "," ++ String.fromInt k
            in
            case Dict.get key table of
                Nothing ->
                    let
                        newTable =
                            Dict.insert key (Dict.size table) table

                        newCodeStream =
                            (Dict.get indexBuffer table |> Maybe.withDefault -404) :: codeStream
                    in
                    encodeGifList_ eoi newTable rest newCodeStream (String.fromInt k)

                Just _ ->
                    encodeGifList_ eoi table rest codeStream key

        [] ->
            eoi
                :: (Dict.get indexBuffer table |> Maybe.withDefault -404)
                :: codeStream
                |> List.reverse


encodeGifList size data =
    let
        cc =
            size + 1

        eoi =
            size + 2

        table =
            List.range 0 size
                |> List.foldl (\k -> Dict.insert (String.fromInt k) k) Dict.empty
                |> Dict.insert (String.fromInt cc) cc
                |> Dict.insert (String.fromInt eoi) eoi
    in
    case data of
        [] ->
            []

        i :: rest ->
            encodeGifList_ eoi table rest [ cc ] (String.fromInt i)
