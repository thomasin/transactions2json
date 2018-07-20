module Types exposing (..)

import Dict exposing (Dict)


--


isNothing : Maybe a -> Bool
isNothing maybe =
  case maybe of
    Nothing -> True
    Just _ -> False


roundTo : Int -> Float -> Float
roundTo places value =
    let
        factor =
            toFloat (10 ^ places)
    in
        ((value * factor)
            |> round
            |> toFloat
        )
            / factor


unwrapTransactions : ( Transactions -> a ) -> TransactionValidity -> a
unwrapTransactions func transactions =
  case transactions of
    Valid t -> func t
    Invalid _ t -> func t


type alias Model =
  { data : DataStatus
  }

type Msg
  = DomLoaded 
  | PdfsLoaded ( Result String ( List RawPdf ) )
  | DeleteTransaction TransactionId
  | FormAction FormAction


type FormAction
  = PrepareForDownload
  | SetFileName FileName
  | SetFileType String


type DataStatus
  = NoData
  | TransactionsFound Form TransactionValidity


type alias Transactions = List Transaction

type alias Form =
  { fileName : FileName
  , fileType : FileType
  }


type FileType
  = Json
  | Csv


type TransactionValidity
  = Valid Transactions
  | Invalid Transaction Transactions


type alias Headers =
  List TextItem


type alias RawPdf =
  List ( Int, RawPage )


type alias RawPage =
  List TextItem


type alias TextItem =
  { text : String
  , width : Float
  , height : Float
  , x : Float
  , y : Float
  }

type alias Transaction =
  { id : TransactionId
  , date : String
  , description : List String
  , debit : Maybe Float
  , credit : Maybe Float
  , balance : Float
  }

type alias FileName = String
type alias TransactionId = Int

