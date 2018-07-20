module Decode exposing (decodePdfList)

import Json.Decode as Decode exposing (Decoder, string, float, list, keyValuePairs)
import Json.Decode.Pipeline exposing (required, decode)
import Dict as Dict

import Types exposing (..)

--


decodePdfList : Decoder ( List RawPdf )
decodePdfList =
  list decodePdf


decodePdf : Decoder RawPdf
decodePdf =
  Decode.map ( List.reverse << keysToInt ) ( keyValuePairs decodePage )


-- private


keysToInt : List ( String, RawPage ) -> List ( Int, RawPage )
keysToInt =
  List.map (\(k, p) -> ( ( Result.withDefault 0 << String.toInt ) k, p ) )


decodePage : Decoder RawPage
decodePage = list decodeTextItem


decodeTextItem : Decoder TextItem
decodeTextItem =
  decode TextItem
    |> required "text" string
    |> required "width" float 
    |> required "height" float
    |> required "x" float
    |> required "y" float