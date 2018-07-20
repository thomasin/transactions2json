port module Main exposing (..)

import Json.Decode
import Json.Encode

import Html

import Decode
import Encode
import Types exposing (..)
import Parser
import View


--


port domLoaded : String -> Cmd msg

port pdfsLoaded : ( Json.Decode.Value -> msg ) -> Sub msg

port downloadJson : Json.Encode.Value -> Cmd msg

port downloadCsv : Json.Encode.Value -> Cmd msg


--


main =
  Html.program 
    { init = init
    , update = update 
    , view = View.view
    , subscriptions = subscriptions
    }


init : ( Model, Cmd Msg )
init = ( initialModel, domLoaded "" )


initialModel = 
  { data = NoData
  }


initialForm =
  { fileName = ""
  , fileType = Csv
  }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of 
    DomLoaded -> ( model, domLoaded "" )

    PdfsLoaded ( Ok pdfList ) ->
      List.map Parser.parsePages pdfList
      |> List.concat
      |> setModel model

    DeleteTransaction transactionId ->
      case model.data of
        NoData -> ( model, Cmd.none )
        TransactionsFound form transactions ->
          ( { model | data = TransactionsFound form ( deleteTransaction transactionId transactions )  }, Cmd.none )

    PdfsLoaded ( Err err ) ->
      ( model, Cmd.none )

    FormAction subMsg -> updateForm model subMsg



updateForm model msg =
  case model.data of
    NoData -> ( model, Cmd.none )
    TransactionsFound form transactions ->
      case msg of
        SetFileName fileName ->
          ( { model | data = TransactionsFound { form | fileName = fileName } transactions }, Cmd.none )

        SetFileType fileType ->
          ( { model | data = TransactionsFound { form | fileType = ( findFileType fileType ) } transactions }, Cmd.none )

        PrepareForDownload ->
          ( model, prepareForDownload form transactions )


prepareForDownload { fileType, fileName } transactions =
  case fileType of
    Json -> downloadJson ( Encode.asJson fileName transactions )
    Csv -> downloadCsv ( Encode.asCsv fileName transactions )


setModel model transactions =
  ( { model 
    | data = TransactionsFound initialForm ( getTransactionValidity transactions ) }
  , Cmd.none
  )


findFileType fileType =
  case fileType of
    "json" -> Json
    "csv" -> Csv 
    _ -> Csv


getStartingBalance : Transactions -> Float
getStartingBalance transactions =
  ( Maybe.withDefault 0.0 << Maybe.map .balance << List.head ) transactions


transactionsValid : Float -> Transactions -> ( Transactions -> TransactionValidity )
transactionsValid prevBalance transactions =
  case transactions of
    [] -> Valid

    t :: ts ->
      let
        debit = Maybe.withDefault 0.0 t.debit
        credit = Maybe.withDefault 0.0 t.credit
        balanceShouldBe = roundTo 2 ( prevBalance - debit + credit )
      in
        if balanceShouldBe == t.balance then
          transactionsValid t.balance ts
        else   
          Invalid t


getTransactionValidity transactions =
  transactionsValid ( getStartingBalance transactions ) ( List.drop 1 transactions )
  |> (\v -> v transactions )
          


deleteTransaction : TransactionId -> TransactionValidity -> TransactionValidity
deleteTransaction transactionId transactions =
  unwrapTransactions ( filterOutTransaction transactionId ) transactions
  |> getTransactionValidity


filterOutTransaction : TransactionId -> Transactions -> Transactions
filterOutTransaction transactionId transactions =
  List.filter ( not << (==) transactionId << .id ) transactions


subscriptions : Model -> Sub Msg
subscriptions model =
  pdfsLoaded ( PdfsLoaded << Json.Decode.decodeValue Decode.decodePdfList )
