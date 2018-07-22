module Parser exposing (parsePages)

import List.Extra as List
import String.Extra as String

import Types exposing (..)


--


type alias Row = List TextItem
type alias Rows = List Row
type alias Strings = List String


headers = [ "Date", "Transaction", "Debit", "Credit", "Balance" ]


detailedHeaders =
  [ HeaderConfig "Id" IndexColumn
  , HeaderConfig "Date" StringColumn
  , HeaderConfig "Transaction" StringColumn
  , HeaderConfig "Debit" MoneyColumn
  , HeaderConfig "Credit" MoneyColumn
  , HeaderConfig "Balance" MoneyColumn
  ]


headerNames = List.map .headerName detailedHeaders


type ColumnType
  = StringColumn
  | MoneyColumn
  | IndexColumn


type alias HeaderConfig a =
  { headerName : String
  , columnType : ColumnType
  , key : ( a -> Strings )
  }


type alias XTransaction =
  { id : Strings
  , date : Strings
  , description : Strings
  , debit : Strings
  , credit : Strings
  , balance : Strings
  }


stringColumn headers headerName row =
  getStrings headers headerName row


moneyColumn headers headerName row =
  getStrings headers headerName row
  |> String.join ""
  |> stringToFloat
  |> Maybe.withDefault 0.0
  |> toString
  |> (\s -> (::) s [])


parsePages : RawPdf -> Transactions
parsePages rawData =
  List.map parsePage rawData
  |> List.concat


-- private


parsePage : ( Int, RawPage ) -> Transactions
parsePage ( _, page ) =
  parseHeaders page
  |> parseRows page


parseHeaders : RawPage -> Row
parseHeaders page =
  List.filter (\i -> List.member i.text headerNames ) page


parseRows : RawPage -> Headers -> Transactions
parseRows page headers =
  truncatePage headers page
  |> collectRows headers


truncatePage : Headers -> RawPage -> RawPage
truncatePage headers page =
  List.dropWhile (\t -> not ( List.member t headers ) ) page
  |> List.dropWhile (\t -> ( List.member t headers ) )


collectRows : Headers -> RawPage -> Transactions
collectRows headers page =
  List.foldl ( createTransactions headers ) ( [], [] ) page
  |> appendFinalTransaction headers
  |> List.reverse


appendFinalTransaction : Headers -> ( Row, Transactions ) -> Transactions
appendFinalTransaction headers ( currentRow, previousTransactions ) =
  appendNewTransaction headers currentRow previousTransactions


-- row into transaction


turnIntoTransaction : Headers -> Int -> Row -> Maybe Transaction
turnIntoTransaction headers index row =
  let
    transaction = getTransaction headers index row

  in
    if transactionIsValid transaction then
      Just { transaction | balance = Maybe.withDefault 0.00 transaction.balance }
    else
      Nothing


getTransaction headers index row =
  { id = index
  , date = getString headers "Date" row
  , description = getStrings headers "Transaction" row
  , debit = getFloat headers "Debit" row
  , credit = getFloat headers "Credit" row
  , balance = getBalance headers "Balance" row
  }


getFloat : Headers -> String -> Row -> Maybe Float
getFloat headers headerName row =
  findHeader headerName headers
  |> Maybe.andThen ( extractFloat << getFirstInColumn row )


getString : Headers -> String -> Row -> String
getString headers headerName row =
  findHeader headerName headers
  |> Maybe.map ( extractStrings << getAllInColumn row )
  |> Maybe.map ( String.join " " )
  |> Maybe.withDefault ""


getStrings : Headers -> String -> Row -> List String
getStrings headers headerName row =
  findHeader headerName headers
  |> Maybe.map ( extractStrings << getAllInColumn row )
  |> Maybe.withDefault []


getBalance : Headers -> String -> Row -> Maybe Float
getBalance headers headerName row =
  findHeader headerName headers
  |> Maybe.andThen ( getFirstInColumn row )
  |> Maybe.andThen ( extractBalance row )


transactionIsValid t =
  not ( String.isEmpty t.date )
  && ( not ( isNothing t.debit && isNothing t.credit && isNothing t.balance ) )


extractBalance : Row -> TextItem -> Maybe Float
extractBalance row balanceType =
  getPreviousItem row balanceType
  |> Maybe.andThen ( stringToFloat << .text )
  |> Maybe.andThen ( setAmountType ( .text balanceType ) )
      

setAmountType : String -> Float -> Maybe Float
setAmountType balanceType amount =
  case balanceType of
    "DR" -> Just -amount
    "CR" -> Just amount
    _ -> Nothing


getPreviousItem : Row -> TextItem -> Maybe TextItem
getPreviousItem row textItem =
  List.sortBy .x row
  |> List.reverse
  |> List.dropWhile ( not << (==) textItem )
  |> List.drop 1
  |> List.head


stringToFloat : String -> Maybe Float
stringToFloat str =
  ( Result.toMaybe << String.toFloat << String.replace "," "" << String.replace "$" "" ) str


getAllInColumn : Row -> TextItem -> Row
getAllInColumn row header =
  List.filter (\t -> sameColumn t header ) row


getFirstInColumn : Row -> TextItem -> Maybe TextItem
getFirstInColumn row header =
  getAllInColumn row header
  |> List.head


extractStrings : Row -> List String
extractStrings textItems =
  List.map .text textItems
  |> List.reverse


extractFloat : Maybe TextItem -> Maybe Float
extractFloat maybeTextItem =
  Maybe.andThen ( stringToFloat << .text ) maybeTextItem


-- create rows


isNewTransaction : Headers -> TextItem -> Bool
isNewTransaction headers textItem =
  case firstColumn headers of
    Nothing -> False
    Just header ->   
      sameColumn textItem header


firstColumn : Headers -> Maybe TextItem
firstColumn headers =
  List.sortBy .x headers
  |> List.head


findHeader : String -> Row -> Maybe TextItem
findHeader headerName =
  List.find ( (==) headerName << .text )


sameColumn : TextItem -> TextItem -> Bool
sameColumn textItem header =
      withinOneUnit header.x textItem.x 
  ||  withinOneUnit ( rightBorder header ) ( rightBorder textItem )


withinOneUnit : Float -> Float -> Bool
withinOneUnit float1 float2 =
  if float1 > float2 then
    ( float1 - float2 ) < 1
  else
    ( float2 - float1 ) < 1


createTransactions : Headers -> TextItem -> ( Row, Transactions ) -> ( Row, Transactions ) 
createTransactions headers textItem ( currentRow, previousTransactions ) =
  if ( isNewTransaction headers textItem ) then
    ( [ textItem ], appendNewTransaction headers currentRow previousTransactions )
  else
    ( (::) textItem currentRow, previousTransactions )


appendNewTransaction : Headers -> Row -> Transactions -> Transactions
appendNewTransaction headers currentRow previousTransactions =
  let
    previousIndex = ( Maybe.withDefault 0 << Maybe.map .id << List.head ) previousTransactions
      
  in    
    case turnIntoTransaction headers ( previousIndex + 1 ) currentRow of
      Nothing -> previousTransactions
      Just transaction -> 
        (::) transaction previousTransactions


rightBorder : TextItem -> Float
rightBorder { width, x } = x + width
