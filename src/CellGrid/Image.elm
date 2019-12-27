module CellGrid.Image exposing
    ( asBmpUri, asPngUri
    , asBmpBytes, asPngBytes
    )

{-| Render a cellgrid as a bmp or png image


## Data URI

[Data URIs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/Data_URIs) using the base64 encoding are a way to create an image from binary data in the browser.
This makes it possible to generate an image in pure elm and display it using HTML.

    import CellGrid exposing (CellGrid, Dimensions)
    import CellGrid.Image
    import Color exposing (Color)
    import Html
    import Html.Attributes exposing (height, src, style, width)

    grid : CellGrid Color
    grid =
        CellGrid.repeat (Dimensions 2 2) Color.red

    main : Html msg
    main =
        Html.img
            [ src (CellGrid.Image.asBmpUri grid)
            , width 300
            , height 300
            , style "image-rendering" "pixelated"
            ]

@docs asBmpUri, asPngUri


## Bytes

@docs asBmpBytes, asPngBytes

-}

import Base64
import Bytes exposing (Bytes)
import CellGrid exposing (CellGrid)
import Color exposing (Color)
import Image exposing (Image)
import Image.Color


{-| Convert to a `Image` from `justgook/elm-image`.
-}
toImage : CellGrid Color -> Image
toImage grid =
    Image.Color.fromList2d (CellGrid.toLists grid)


{-| Encode a `CellGrid` as a bmp image.
-}
asBmpBytes : CellGrid Color -> Bytes
asBmpBytes =
    Image.toBmp << toImage


{-| Encode a `CellGrid` as a png image.
-}
asPngBytes : CellGrid Color -> Bytes
asPngBytes =
    Image.toPng << toImage


{-| Encode a `CellGrid` as a bmp URI.

    import CellGrid exposing (CellGrid, Dimensions)
    import Color exposing (Color)

    grid : CellGrid Color
    grid =
        CellGrid.repeat (Dimensions 2 2) (Color.red)

    asBmpUri grid
        --> "data:image/bmp;base64,Qk1GAAAAAAAAADYAAAAoAAAAAgAAAAIAAAABABgAAAAAABAAAAATCwAAEwsAAAAAAAAAAAAAAADMAADMAAAAAMwAAMwAAA=="

-}
asBmpUri : CellGrid Color -> String
asBmpUri grid =
    let
        encoded =
            Base64.fromBytes (asBmpBytes grid)
                |> Maybe.withDefault ""
    in
    "data:image/bmp;base64," ++ encoded


{-| Encode a `CellGrid` as a png URI.

    import CellGrid exposing (CellGrid, Dimensions)
    import Color exposing (Color)

    grid : CellGrid Color
    grid =
        CellGrid.repeat (Dimensions 2 2) (Color.red)

    asPngUri grid
        --> "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAIAAAACCAYAAABytg0kAAAAGklEQVR4nD3GsQEAAADBsPr/NT9hkikyhHkKJwYDmXkNWg4AAAAASUVORK5CYII="

-}
asPngUri : CellGrid Color -> String
asPngUri grid =
    let
        encoded =
            Base64.fromBytes (asPngBytes grid)
                |> Maybe.withDefault ""
    in
    "data:image/png;base64," ++ encoded
