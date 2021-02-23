module Image.Internal.BitReader exposing
    ( BitReader(..)
    , State
    , andMap
    , andThen
    , decode
    , embed
    , error
    , exactly
    , flush
    , getBit
    , loop
    , map
    , map2
    , map3
    , map4
    , readBits
    , readMoreBits
    , runDecoder
    , succeed
    )

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Bytes.Encode as Encode


{-|

  - tag is where we read from
  - bitsAvailable is the number of unread bits on the tag
  - reserve is where we write to from the buffer, to always be able to read a unsignedInt32
  - reserveAvailable is the number of unread bits on the reserve

-}
type alias State =
    { tag : Int
    , bitsAvailable : Int
    , reserve : Int
    , reserveAvailable : Int
    , buffer : Bytes
    }


readMoreBits : State -> Result String State
readMoreBits state =
    let
        freeSpaceOnTag =
            32 - state.bitsAvailable
    in
    if freeSpaceOnTag <= state.reserveAvailable && state.reserveAvailable > 0 then
        Ok (moveFromReserve freeSpaceOnTag state)

    else if Bytes.width state.buffer == 0 then
        Ok (moveFromReserve state.reserveAvailable state)

    else
        let
            ( width, additionallyAvailable, decoder ) =
                case Bytes.width state.buffer of
                    0 ->
                        ( 0, 0, Decode.succeed 0 )

                    1 ->
                        ( 1, 8, Decode.unsignedInt8 )

                    2 ->
                        ( 2, 16, Decode.unsignedInt16 LE )

                    3 ->
                        ( 3, 24, unsignedInt24 LE )

                    _ ->
                        ( 4, 32, Decode.unsignedInt32 LE )

            state1 =
                moveFromReserve state.reserveAvailable state
        in
        case runDecoder width decoder state1 of
            Err e ->
                Err e

            Ok ( newReserve, newBuffer ) ->
                readMoreBits
                    { tag = state1.tag
                    , bitsAvailable = state1.bitsAvailable
                    , reserve = newReserve
                    , reserveAvailable = additionallyAvailable
                    , buffer = newBuffer
                    }


moveFromReserve : Int -> State -> State
moveFromReserve nbits state =
    let
        masked =
            if nbits == 32 then
                state.reserve
                    |> Bitwise.shiftLeftBy state.bitsAvailable

            else
                Bitwise.and (Bitwise.shiftLeftBy nbits 1 - 1) state.reserve
                    |> Bitwise.shiftLeftBy state.bitsAvailable
    in
    { tag = Bitwise.or masked state.tag
    , bitsAvailable = state.bitsAvailable + nbits
    , reserve = Bitwise.shiftRightZfBy nbits state.reserve
    , reserveAvailable = state.reserveAvailable - nbits
    , buffer = state.buffer
    }


{-| Flush read bits back into the buffer
-}
flush : State -> State
flush state =
    { buffer = flushHelp state
    , tag = 0
    , bitsAvailable = 0
    , reserve = 0
    , reserveAvailable = 0
    }


flushHelp : State -> Bytes
flushHelp state0 =
    let
        availableSpace =
            32 - state0.bitsAvailable

        state =
            moveFromReserve (min availableSpace state0.reserveAvailable) state0

        tagEncoder =
            if state.bitsAvailable > 24 then
                [ Encode.unsignedInt32 LE state.tag ]

            else if state.bitsAvailable > 16 then
                [ Encode.unsignedInt16 LE state.tag, Encode.unsignedInt8 (Bitwise.shiftRightBy 16 state.tag) ]

            else if state.bitsAvailable > 8 then
                [ Encode.unsignedInt16 LE state.tag ]

            else if state.bitsAvailable > 1 then
                [ Encode.unsignedInt8 state.tag ]

            else
                []

        reserveEncoder =
            if state.reserveAvailable > 24 then
                [ Encode.unsignedInt32 LE state.reserve ]

            else if state.reserveAvailable > 16 then
                [ Encode.unsignedInt16 LE state.reserve, Encode.unsignedInt8 (Bitwise.shiftRightBy 16 state.reserve) ]

            else if state.reserveAvailable > 8 then
                [ Encode.unsignedInt16 LE state.reserve ]

            else if state.reserveAvailable > 1 then
                [ Encode.unsignedInt8 state.reserve ]

            else
                []
    in
    Encode.sequence (tagEncoder ++ reserveEncoder ++ [ Encode.bytes state.buffer ])
        |> Encode.encode


decode : Bytes -> BitReader a -> Result String a
decode bytes (BitReader reader) =
    let
        initialState =
            { buffer = bytes
            , tag = 0
            , bitsAvailable = 0
            , reserve = 0
            , reserveAvailable = 0
            }
    in
    case reader initialState of
        Ok ( value, _ ) ->
            Ok value

        Err e ->
            Err e


type BitReader b
    = BitReader (State -> Result String ( b, State ))


succeed : a -> BitReader a
succeed x =
    BitReader (\s -> Ok ( x, s ))


error : String -> BitReader a
error e =
    BitReader (\s -> Err e)


embed : (State -> Result String ( b, State )) -> BitReader b
embed =
    BitReader


map : (a -> b) -> BitReader a -> BitReader b
map f (BitReader g) =
    BitReader
        (\s ->
            case g s of
                Ok ( value, newState ) ->
                    Ok ( f value, newState )

                Err e ->
                    Err e
        )


andMap : BitReader a -> BitReader (a -> b) -> BitReader b
andMap a f =
    map2 (<|) f a


map2 :
    (a -> b -> output)
    -> BitReader a
    -> BitReader b
    -> BitReader output
map2 f (BitReader fa) (BitReader fb) =
    BitReader <|
        \state ->
            case fa state of
                Err e ->
                    Err e

                Ok ( a, newState ) ->
                    case fb newState of
                        Err e ->
                            Err e

                        Ok ( b, newerState ) ->
                            Ok ( f a b, newerState )


map3 :
    (a -> b -> c -> output)
    -> BitReader a
    -> BitReader b
    -> BitReader c
    -> BitReader output
map3 f a b c =
    succeed f
        |> andMap a
        |> andMap b
        |> andMap c


map4 :
    (a -> b -> c -> d -> output)
    -> BitReader a
    -> BitReader b
    -> BitReader c
    -> BitReader d
    -> BitReader output
map4 f a b c d =
    succeed f
        |> andMap a
        |> andMap b
        |> andMap c
        |> andMap d


andThen : (a -> BitReader b) -> BitReader a -> BitReader b
andThen f (BitReader g) =
    BitReader <|
        \s ->
            case g s of
                Ok ( value, newState ) ->
                    let
                        (BitReader h) =
                            f value
                    in
                    h newState

                Err e ->
                    Err e


readBits : Int -> Int -> BitReader Int
readBits numberOfBits base =
    -- TODO be faster for multiples of 8, especially when bitposition = 0
    BitReader <|
        \state ->
            if numberOfBits == 0 then
                Ok ( base, state )

            else
                case
                    if state.bitsAvailable < numberOfBits then
                        readMoreBits state

                    else
                        Ok state
                of
                    Err e ->
                        Err e

                    Ok d ->
                        let
                            val =
                                Bitwise.and d.tag (Bitwise.shiftRightZfBy (16 - numberOfBits) 0xFFFF)

                            newTag =
                                Bitwise.shiftRightZfBy numberOfBits d.tag
                        in
                        Ok
                            ( val + base
                            , { tag = newTag
                              , bitsAvailable = d.bitsAvailable - numberOfBits
                              , reserve = d.reserve
                              , reserveAvailable = d.reserveAvailable
                              , buffer = d.buffer
                              }
                            )


getBit : BitReader Int
getBit =
    readBits 1 0


unsignedInt24 endianness =
    case endianness of
        LE ->
            Decode.map2 (\b2 b1 -> Bitwise.or (Bitwise.shiftLeftBy 16 b1) b2) (Decode.unsignedInt16 endianness) Decode.unsignedInt8

        BE ->
            Decode.map2 (\b1 b2 -> Bitwise.or (Bitwise.shiftLeftBy 16 b1) b2) (Decode.unsignedInt16 endianness) Decode.unsignedInt8


runDecoder : Int -> Decode.Decoder a -> State -> Result String ( a, Bytes )
runDecoder width valueDecoder state =
    let
        decoder =
            Decode.map2 Tuple.pair valueDecoder (Decode.bytes (Bytes.width state.buffer - width))
    in
    case Decode.decode decoder state.buffer of
        Just value ->
            Ok value

        Nothing ->
            Err "BitReader.runDecoder: Unexpected end of Bytes"


loop : state -> (state -> BitReader (Step state a)) -> BitReader a
loop state callback =
    BitReader (loopHelp state callback)


loopHelp : state -> (state -> BitReader (Step state a)) -> State -> Result String ( a, State )
loopHelp accum callback state =
    let
        (BitReader decoder) =
            callback accum
    in
    case decoder state of
        Err e ->
            Err e

        Ok ( Loop newAccum, newState ) ->
            loopHelp newAccum callback newState

        Ok ( Done result, newState ) ->
            Ok ( result, newState )


exactly : Int -> BitReader a -> BitReader (List a)
exactly tableCount decoder =
    let
        helper ( n, xs ) =
            if n <= 0 then
                succeed (Done (List.reverse xs))

            else
                map (\x -> Loop ( n - 1, x :: xs )) decoder
    in
    loop ( tableCount, [] ) helper
