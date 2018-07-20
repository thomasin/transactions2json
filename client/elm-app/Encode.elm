module Encode exposing (asJson, asCsv)

import Json.Encode exposing (Value, object, string, list, float, null)

import Types exposing (..)

--


asJson : FileName -> TransactionValidity -> Value
asJson fileName transactions =
  unwrapTransactions
    (\t -> object
        [ ( "fileName", string fileName )
        , ( "transactions", encodeListWith encodeTransaction t )
        ]
    ) transactions


asCsv : FileName -> TransactionValidity -> Value
asCsv fileName transactions =
  unwrapTransactions
    (\t -> object
        [ ( "fileName", string fileName )
        , ( "transactions", string ( transactionsToCsvs t ) )
        ]
    ) transactions



-- private


encodeTransaction : Transaction -> Value
encodeTransaction transaction =
  object
    [ ( "date", string transaction.date )
    , ( "description", encodeListWith string transaction.description )
    , ( "debit", encodeMaybeFloat transaction.debit )
    , ( "credit", encodeMaybeFloat transaction.credit )
    , ( "credit", encodeMaybeFloat transaction.credit )
    , ( "balance", float transaction.balance )
    ]


encodeMaybeFloat : Maybe Float -> Value
encodeMaybeFloat maybe =
  case maybe of
    Nothing -> null
    Just f -> float f


encodeListWith : ( a -> Value ) -> List a -> Value
encodeListWith func l =
  ( list << List.map func ) l


transactionsToCsvs transactions =
  List.map transactionToCsvRow transactions
  |> (::) ( "Date,Description,Debit,Credit,Balance" )
  |> String.join "\r\n"


transactionToCsvRow t =
  String.join ","
    [ dQ t.date
    , dQ ( String.join " " t.description )
    , ( toString << Maybe.withDefault 0.0 ) t.debit
    , ( toString << Maybe.withDefault 0.0 ) t.credit
    , toString t.balance
    ]

-- double quote string
dQ str =
  (++) str "\""
  |> (++) "\""