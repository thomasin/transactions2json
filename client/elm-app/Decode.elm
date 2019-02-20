module Decode exposing (decodePdfList)

import Dict as Dict
import Json.Decode as Decode exposing (Decoder, float, keyValuePairs, list, string)
import Json.Decode.Pipeline exposing (required)
import Types exposing (..)



--


decodePdfList : Decoder (List RawPdf)
decodePdfList =
    list decodePdf


decodePdf : Decoder RawPdf
decodePdf =
    Decode.succeed RawPdf
        |> required "fileName" string
        |> required "pages" (Decode.map (List.reverse << keysToInt) (keyValuePairs decodePage))



-- private


keysToInt : List ( String, RawPage ) -> List ( Int, RawPage )
keysToInt =
    List.map (\( k, p ) -> ( ( Maybe.withDefault 0 << String.toInt ) k, p ) )


decodePage : Decoder RawPage
decodePage =
    list decodeTextItem


decodeTextItem : Decoder TextItem
decodeTextItem =
    Decode.succeed TextItem
        |> required "text" string
        |> required "width" float
        |> required "height" float
        |> required "x" float
        |> required "y" float
