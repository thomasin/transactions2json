module Encode exposing (asCsv, asJson)

import Dict as Dict
import Json.Encode as E exposing (Value, bool, float, list, null, object, string)
import Types exposing (..)
import List
import Utils as U


--


asJson : FileName -> Transactions -> Value
asJson fileName transactions =
    exportObject fileName ( list transactionToJson transactions )


asCsv : FileName -> List Header -> Transactions -> Value
asCsv fileName headers transactions =
    exportObject fileName ( string ( transactionsToCsv transactions headers ) )


exportObject : FileName -> Value -> Value
exportObject fileName transactions =
    object
        [ ( "fileName", string ( unwrapFileName fileName ) )
        , ( "transactions", transactions )
        ]


-- private


transactionToJson : Transaction -> Value
transactionToJson transaction =
    Dict.toList transaction.columns
    |> List.map transactionFieldToJson
    |> object


transactionFieldToJson : ( String, FieldConfig ) -> ( String, Value )
transactionFieldToJson ( fieldName, { field } ) =
    case field of
        StringField s ->
            ( fieldName, string s )

        DateField s ->
            ( fieldName, string s )

        TextField ls ->
            ( fieldName, list string ls )

        FloatField f ->
            ( fieldName, float f )

        MoneyField f ->
            ( fieldName, float f )

        BlankField ->
            ( fieldName, null )


encodeMaybeFloat : Maybe Float -> Value
encodeMaybeFloat maybe =
    case maybe of
        Nothing ->
            null

        Just f ->
            float f


transactionsToCsv : Transactions -> List Header -> String
transactionsToCsv transactions headers =
    List.map transactionToCsvRow transactions
    |> (::) ( headersToCsvRow headers )
    |> String.join "\u{000D}\n"


headersToCsvRow : List Header -> String
headersToCsvRow headers =
    List.map U.displayHeaderName headers
    |> String.join ","


transactionToCsvRow : Transaction -> String
transactionToCsvRow t =
    Dict.toList t.columns
    |> List.map transactionFieldToCsv
    |> String.join ","


transactionFieldToCsv : ( String, FieldConfig ) -> String
transactionFieldToCsv ( fieldName, { field } ) =
    case field of
        StringField s ->
            dQ s

        DateField s ->
            dQ s

        TextField ls ->
            dQ ( String.join " " ls )

        FloatField f ->
            String.fromFloat f

        MoneyField s ->
            String.fromFloat s

        BlankField ->
            dQ ""



-- double quote string


dQ : String -> String
dQ str =
    (++) str "\""
    |> (++) "\""
