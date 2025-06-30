module VendoredBase64 exposing (fromBytes, toBytes)

{-| Vendored from <https://package.elm-lang.org/packages/danfishgold/base64-bytes/1.1.0/>


# Conversion

@docs fromBytes, toBytes

-}

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode
import Bytes.Encode as Encode exposing (Encoder)


{-| Convert a Base64 string to bytes.
If you want more control over the process, you should use [`encoder`](#encoder).

This function fails (returns `Nothing`) if you give it an invalid Base64 sequence.

-}
toBytes : String -> Maybe Bytes
toBytes string =
    Maybe.map Encode.encode (encoder string)


encoder : String -> Maybe Encode.Encoder
encoder string =
    encodeChunks string []
        |> Maybe.map (List.reverse >> Encode.sequence)


{-| Big picture:

  - read 4 base64 characters
  - convert them to 3 bytes (24 bits)
  - encode these bytes

-}
encodeChunks : String -> List Encoder -> Maybe (List Encoder)
encodeChunks input accum =
    {- Performance Note

       slice and toList is just as fast as (possibly a little faster than) repeated `String.uncons`,
       but this code is much more readable
    -}
    case String.toList (String.left 4 input) of
        [] ->
            Just accum

        [ a, b, c, d ] ->
            case encodeCharacters a b c d of
                Just enc ->
                    encodeChunks (String.dropLeft 4 input) (enc :: accum)

                Nothing ->
                    Nothing

        [ a, b, c ] ->
            case encodeCharacters a b c '=' of
                Nothing ->
                    Nothing

                Just enc ->
                    Just (enc :: accum)

        [ a, b ] ->
            case encodeCharacters a b '=' '=' of
                Nothing ->
                    Nothing

                Just enc ->
                    Just (enc :: accum)

        _ ->
            Nothing


{-| Convert 4 characters to 24 bits (as an Encoder)
-}
encodeCharacters : Char -> Char -> Char -> Char -> Maybe Encoder
encodeCharacters a b c d =
    {- Performance notes

       We use bitshifts and other bitwise operators here. They are much faster than the alternatives.
       This may not normally matter but this function is called a lot so even small increases
       in efficiency are noticable

       Secondly, we combine two `uint8` into one `uint16 BE`. This has no direct speed benefit
       (elm/bytes uses a DataView, which only natively supports adding/reading uint8)
       but having fewer list items decreases # of encoding steps and allocation,
       and is therefore faster.
    -}
    if isValidChar a && isValidChar b then
        let
            n1 =
                unsafeConvertChar a

            n2 =
                unsafeConvertChar b
        in
        -- `=` is the padding character, and must be special-cased
        -- only the `c` and `d` char are allowed to be padding
        case d of
            '=' ->
                case c of
                    '=' ->
                        let
                            n =
                                Bitwise.or
                                    (Bitwise.shiftLeftBy 18 n1)
                                    (Bitwise.shiftLeftBy 12 n2)

                            b1 =
                                -- masking higher bits is not needed, Encode.unsignedInt8 ignores higher bits
                                Bitwise.shiftRightBy 16 n
                        in
                        Just (Encode.unsignedInt8 b1)

                    _ ->
                        if isValidChar c then
                            let
                                n3 =
                                    unsafeConvertChar c

                                n =
                                    Bitwise.or
                                        (Bitwise.or (Bitwise.shiftLeftBy 18 n1) (Bitwise.shiftLeftBy 12 n2))
                                        (Bitwise.shiftLeftBy 6 n3)

                                combined =
                                    Bitwise.shiftRightBy 8 n
                            in
                            Just (Encode.unsignedInt16 BE combined)

                        else
                            Nothing

            _ ->
                if isValidChar c && isValidChar d then
                    let
                        n3 =
                            unsafeConvertChar c

                        n4 =
                            unsafeConvertChar d

                        n =
                            Bitwise.or
                                (Bitwise.or (Bitwise.shiftLeftBy 18 n1) (Bitwise.shiftLeftBy 12 n2))
                                (Bitwise.or (Bitwise.shiftLeftBy 6 n3) n4)

                        b3 =
                            -- Masking the higher bits is not needed: Encode.unsignedInt8 ignores higher bits
                            n

                        combined =
                            Bitwise.shiftRightBy 8 n
                    in
                    Just
                        (Encode.sequence
                            [ Encode.unsignedInt16 BE combined
                            , Encode.unsignedInt8 b3
                            ]
                        )

                else
                    Nothing

    else
        Nothing


{-| is the character a base64 digit?

The base16 digits are: A-Z, a-z, 0-1, '+' and '/'

-}
isValidChar : Char -> Bool
isValidChar c =
    if Char.isAlphaNum c then
        True

    else
        case c of
            '+' ->
                True

            '/' ->
                True

            _ ->
                False


{-| Convert a base64 character/digit to its index

See also [Wikipedia](https://en.wikipedia.org/wiki/Base64#Base64_table)

-}
unsafeConvertChar : Char -> Int
unsafeConvertChar char =
    {- Performance Note

       Working with the key directly is faster than using e.g. `Char.isAlpha` and `Char.isUpper`
    -}
    let
        key =
            Char.toCode char
    in
    if key >= 65 && key <= 90 then
        -- A-Z
        key - 65

    else if key >= 97 && key <= 122 then
        -- a-z
        (key - 97) + 26

    else if key >= 48 && key <= 57 then
        -- 0-9
        (key - 48) + 26 + 26

    else
        case char of
            '+' ->
                62

            '/' ->
                63

            _ ->
                -1


{-| Convert bytes to a Base64 string.
If you want more control over the process, you should use [`decoder`](#decoder).

This function should never return `Nothing`, but it uses
[`Bytes.Decode.decode`](https://package.elm-lang.org/packages/elm/bytes/latest/Bytes-Decode#decode),
which returns a `Maybe String`.

-}
fromBytes : Bytes -> Maybe String
fromBytes bytes =
    Decode.decode (decoder (Bytes.width bytes)) bytes


decoder : Int -> Decode.Decoder String
decoder width =
    Decode.loop { remaining = width, string = "" } loopHelp



-- INTERNALS


{-| Base64 uses 6 bits per digit (because 2^6 == 64)
and can nicely store 4 digits in 24 bits, which are 3 bytes.

The decoding process is thus roughly:

  - read a 3-byte chunk
  - extract the 4 6-bit segments
  - convert those segments into characters

But the input does not need to have a multiple of 4 characters,
so at the end of the string some characters can be omitted.
This means there may be 2 or 1 bytes remaining at the end. We have to cover those cases!

-}
loopHelp : { remaining : Int, string : String } -> Decode.Decoder (Decode.Step { remaining : Int, string : String } String)
loopHelp { remaining, string } =
    if remaining >= 18 then
        -- Note: this case is heavily optimized.
        -- For the general idea of what this function does, the `remaining >= 3` case is more illustrative.
        decode18Bytes
            |> Decode.map
                (\result ->
                    Decode.Loop
                        { remaining = remaining - 18
                        , string = string ++ result
                        }
                )

    else if remaining >= 3 then
        let
            helper a b c =
                let
                    combined =
                        Bitwise.or (Bitwise.or (Bitwise.shiftLeftBy 16 a) (Bitwise.shiftLeftBy 8 b)) c
                in
                Decode.Loop
                    { remaining = remaining - 3
                    , string = string ++ bitsToChars combined 0
                    }
        in
        Decode.map3 helper
            Decode.unsignedInt8
            Decode.unsignedInt8
            Decode.unsignedInt8

    else if remaining == 0 then
        Decode.succeed (Decode.Done string)

    else if remaining == 2 then
        let
            helper a b =
                let
                    combined =
                        Bitwise.or (Bitwise.shiftLeftBy 16 a) (Bitwise.shiftLeftBy 8 b)
                in
                Decode.Done (string ++ bitsToChars combined 1)
        in
        Decode.map2 helper
            Decode.unsignedInt8
            Decode.unsignedInt8

    else
        -- remaining == 1
        Decode.map (\a -> Decode.Done (string ++ bitsToChars (Bitwise.shiftLeftBy 16 a) 2))
            Decode.unsignedInt8


{-| Mask that can be used to get the lowest 6 bits of a binary number
-}
lowest6BitsMask : Int
lowest6BitsMask =
    63


{-| Turn the decoded bits (at most 24, can be fewer because of padding) into 4 base64 characters.

(- - - - - - - -)(- - - - - - - -)(- - - - - - - -)
(- - - - - -|- - - - - -|- - - - - -|- - - - - -)

-}
bitsToChars : Int -> Int -> String
bitsToChars bits missing =
    {- Performance Notes

       `String.cons` proved to be the fastest way of combining characters into a string
       see also https://github.com/danfishgold/base64-bytes/pull/3#discussion_r342321940

       The input is 24 bits, which we have to partition into 4 6-bit segments. We achieve this by
       shifting to the right by (a multiple of) 6 to remove unwanted bits on the right, then `Bitwise.and`
       with `0b111111` (which is 2^6 - 1 or 63) (so, 6 1s) to remove unwanted bits on the left.

    -}
    let
        -- any 6-bit number is a valid base64 digit, so this is actually safe
        p =
            unsafeToChar (Bitwise.shiftRightZfBy 18 bits)

        q =
            unsafeToChar (Bitwise.and (Bitwise.shiftRightZfBy 12 bits) lowest6BitsMask)

        r =
            unsafeToChar (Bitwise.and (Bitwise.shiftRightZfBy 6 bits) lowest6BitsMask)

        s =
            unsafeToChar (Bitwise.and bits lowest6BitsMask)
    in
    case missing of
        -- case `0` is the most common, so put it first.
        0 ->
            String.cons p (String.cons q (String.cons r (String.fromChar s)))

        1 ->
            String.cons p (String.cons q (String.cons r "="))

        2 ->
            String.cons p (String.cons q "==")

        _ ->
            ""


{-| Base64 index to character/digit
-}
unsafeToChar : Int -> Char
unsafeToChar n =
    if n <= 25 then
        -- uppercase characters
        Char.fromCode (65 + n)

    else if n <= 51 then
        -- lowercase characters
        Char.fromCode (97 + (n - 26))

    else if n <= 61 then
        -- digit characters
        Char.fromCode (48 + (n - 52))

    else
        -- special cases
        case n of
            62 ->
                '+'

            63 ->
                '/'

            _ ->
                '\u{0000}'



-- OPTIMIZED VERSION


u32BE : Decode.Decoder Int
u32BE =
    Decode.unsignedInt32 Bytes.BE


u16BE : Decode.Decoder Int
u16BE =
    Decode.unsignedInt16 Bytes.BE


{-| A specialized version reading 18 bytes at once
To get a better understanding of what this code does, read the `remainder >= 3` case above.

This tries to take the biggest step possible within a `Decode.loop` iteration.
The idea is similar to loop-unrolling in languages like c: avoiding jumps gives better performance

But `Decode.loop` also requires that the accumulator is wrapped in a `Step a b`, i.e. it allocates a `Decode.Loop _`
for every iteration. Allocation is expensive in tight loops like this one. So there is a double reason to limit the number
of iterations: avoiding jumps and avoiding allocation.

Given that `Decode.map5` is the highest one defined by `elm/bytes` and we need a multiple of 3 bytes,
`4 * 4 + 2 = 18` is the best we can do.

-}
decode18Bytes : Decode.Decoder String
decode18Bytes =
    Decode.map5 decode18Help
        u32BE
        u32BE
        u32BE
        u32BE
        u16BE


{-| Get 18 bytes (4 times 32-bit, one 16-bit) and split them into 3-byte chunks.

Then convert the 3-byte chunks to characters and produce a string.

-}
decode18Help : Int -> Int -> Int -> Int -> Int -> String
decode18Help a b c d e =
    let
        combined1 =
            Bitwise.shiftRightZfBy 8 a

        combined2 =
            Bitwise.or
                (Bitwise.and 0xFF a |> Bitwise.shiftLeftBy 16)
                (Bitwise.shiftRightZfBy 16 b)

        combined3 =
            Bitwise.or
                (Bitwise.and 0xFFFF b |> Bitwise.shiftLeftBy 8)
                (Bitwise.shiftRightZfBy 24 c)

        combined4 =
            Bitwise.and 0x00FFFFFF c

        combined5 =
            Bitwise.shiftRightZfBy 8 d

        combined6 =
            Bitwise.or
                (Bitwise.and 0xFF d |> Bitwise.shiftLeftBy 16)
                e
    in
    -- the order is counter-intuitive because `String.cons` is used in bitsToCharSpecialized.
    ""
        |> bitsToCharSpecialized combined6 combined5 combined4
        |> bitsToCharSpecialized combined3 combined2 combined1


{-| A specialized version of bitsToChar that handles 3 24-bit integers at once.

This was done to limit the number of function calls. When doing bitwise manipulations (which are very efficient), the
overhead of function calls -- something we normally don't really think about -- starts to matter.

-}
bitsToCharSpecialized : Int -> Int -> Int -> String -> String
bitsToCharSpecialized bits1 bits2 bits3 accum =
    let
        -- BITS 1
        p =
            unsafeToChar (Bitwise.shiftRightZfBy 18 bits1)

        q =
            unsafeToChar (Bitwise.and (Bitwise.shiftRightZfBy 12 bits1) lowest6BitsMask)

        r =
            unsafeToChar (Bitwise.and (Bitwise.shiftRightZfBy 6 bits1) lowest6BitsMask)

        s =
            unsafeToChar (Bitwise.and bits1 lowest6BitsMask)

        -- BITS 2
        a =
            unsafeToChar (Bitwise.shiftRightZfBy 18 bits2)

        b =
            unsafeToChar (Bitwise.and (Bitwise.shiftRightZfBy 12 bits2) lowest6BitsMask)

        c =
            unsafeToChar (Bitwise.and (Bitwise.shiftRightZfBy 6 bits2) lowest6BitsMask)

        d =
            unsafeToChar (Bitwise.and bits2 lowest6BitsMask)

        -- BITS 3
        x =
            unsafeToChar (Bitwise.shiftRightZfBy 18 bits3)

        y =
            unsafeToChar (Bitwise.and (Bitwise.shiftRightZfBy 12 bits3) lowest6BitsMask)

        z =
            unsafeToChar (Bitwise.and (Bitwise.shiftRightZfBy 6 bits3) lowest6BitsMask)

        w =
            unsafeToChar (Bitwise.and bits3 lowest6BitsMask)
    in
    -- Performance: This is the fastest way to create a string from characters.
    -- see also https://github.com/danfishgold/base64-bytes/pull/3#discussion_r342321940
    -- cons adds on the left, so characters are added in reverse order.
    accum
        |> String.cons s
        |> String.cons r
        |> String.cons q
        |> String.cons p
        |> String.cons d
        |> String.cons c
        |> String.cons b
        |> String.cons a
        |> String.cons w
        |> String.cons z
        |> String.cons y
        |> String.cons x
