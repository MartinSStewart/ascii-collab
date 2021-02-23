module Image.Magic exposing (mirror)

{-|

@docs mirror

-}

import Image.Internal.ImageData as ImageData exposing (Image(..), Order(..))


{-| -}
mirror : Bool -> Bool -> Image -> Image
mirror horizontally vertically image =
    case ( image, horizontally, vertically ) of
        ( List2d meta list2d, True, True ) ->
            List.foldl (\l acc -> List.reverse l :: acc) [] list2d
                |> List2d meta

        ( List2d meta list2d, True, False ) ->
            List.map (\l -> List.reverse l) list2d
                |> List2d meta

        ( List2d meta list2d, False, True ) ->
            List.reverse list2d
                |> List2d meta

        ( _, False, False ) ->
            image

        ( im, _, _ ) ->
            -- TODO Make rest flips more effective
            List2d (ImageData.getInfo im) (ImageData.toList2d im)
                |> mirror horizontally vertically



{-
   use it as
       Flip True True (Crop 10 10 100 100 (Image Image))

-}


type Filter
    = Flip Bool Bool Filter
    | Crop Int Int Int Int Filter
    | Image Image
