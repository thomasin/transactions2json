module PdfParser exposing (parsePdfs)

import Dict as Dict
import List.Extra as ListE
import String.Extra as StringE
import Types exposing (..)
import Utils as U
import Date as Date
import Money as Money

--


type alias Row =
    List TextItem


type alias Rows =
    List Row


type alias Strings =
    List String


parsePdfs : Options -> List RawPdf -> Transactions
parsePdfs options pdfList =
    List.map ( parsePages options << List.reverse << .pages ) pdfList
    |> List.concat
    |> List.indexedMap setTransactionId


parsePages : Options -> List ( Int, RawPage ) -> Transactions
parsePages options rawData =
    pageParsing ( EmptyHeaderRow [] ) [] options rawData
    |> List.reverse
    |> List.concat


pageParsing : HeaderRow -> List Transactions -> Options -> List ( Int, RawPage ) -> List Transactions
pageParsing previousHeaderRow previousTransactions options pages =
  case pages of
    [] -> previousTransactions
    p :: ps ->
      let
        headerRow = getHeadersToUse previousHeaderRow ( parseHeaders options.headers p )
        transactions = (::) ( parsePage options p headerRow ) previousTransactions
      in
        pageParsing headerRow transactions options ps


getHeadersToUse prevHeaders currHeaders =
  case currHeaders of
    EmptyHeaderRow _ ->
      case prevHeaders of
        EmptyHeaderRow _ -> currHeaders
        FullHeaderRow _ _ -> prevHeaders

    FullHeaderRow _ _ -> currHeaders



setTransactionId : Int -> Transaction -> Transaction
setTransactionId index transaction =
    { transaction | id = TransactionId index }


-- private


parsePage : Options -> ( Int, RawPage ) -> HeaderRow -> Transactions
parsePage options ( _, page ) headers =
    parseRows options page ( unwrapParserHeaders headers )


parseHeaders : HeaderOptions -> ( Int, RawPage ) -> HeaderRow
parseHeaders headerOptions ( _, page ) =
  List.foldl ( collectHeaderRows page ) [] ( unwrapHeaders headerOptions )
  |> List.head
  |> Maybe.withDefault ( EmptyHeaderRow [] )



collectHeaderRows : RawPage -> Header -> HeaderRows -> HeaderRows
collectHeaderRows page header headerRows =
  generateHeaderResults page header
  |> (\headerParsingResult ->
    case headerRows of
      [] -> generateFirstHeaderRows headerParsingResult
      _ -> matchRows headerRows headerParsingResult
  )


generateFirstHeaderRows : HeaderParsingResult -> HeaderRows
generateFirstHeaderRows headerParsingResult =
  case headerParsingResult of
    NoMatchingHeadersFound header ->
      [ EmptyHeaderRow [ UnMatchedHeader header ] ]

    MatchingHeadersFound header textItemList ->
      List.map
        (\textItem -> FullHeaderRow ( .y textItem, bottomBorder textItem ) [ MatchedHeader header textItem ] )
        textItemList


type alias ParserHeaders = List ParserHeader


type ParserHeader
  = UnMatchedHeader Header
  | MatchedHeader Header TextItem


getHeaderName parserHeader =
  case parserHeader of
    UnMatchedHeader header -> header.name
    MatchedHeader header _ -> header.name


matchRows : HeaderRows -> HeaderParsingResult -> HeaderRows
matchRows headerRows headerParsingResult =
  case headerParsingResult of
    NoMatchingHeadersFound header ->
      List.map ( appendNotFoundHeader header ) headerRows

    MatchingHeadersFound header textItems ->
      List.foldl ( tryMatchHeader header textItems ) [] headerRows


appendNotFoundHeader : Header -> HeaderRow -> HeaderRow
appendNotFoundHeader header headerRow =
  mapToParserHeaders
    ( (::) ( UnMatchedHeader header ) )
    headerRow


tryMatchHeader : Header -> List TextItem -> HeaderRow -> HeaderRows -> HeaderRows
tryMatchHeader header textItems headerRow headerRows =
  case headerRow of
    EmptyHeaderRow parserHeaders ->
      (++) (
        List.map (\textItem ->
          appendToHeaderRow 
            header 
            textItem 
            ( textItem.y, bottomBorder textItem ) 
            parserHeaders
        ) textItems 
      ) headerRows

    FullHeaderRow range parserHeaders ->
      ListE.find ( sameRow range ) textItems
      |> (\result ->
        case result of
          Nothing -> headerRows
          Just textItem -> (::) ( appendToHeaderRow header textItem range parserHeaders ) headerRows
      )


sameRow : Range -> TextItem -> Bool
sameRow range textItem =
  rangesOverlap 0 range ( textItem.y, bottomBorder textItem )


appendToHeaderRow : Header -> TextItem -> Range -> ParserHeaders -> HeaderRow
appendToHeaderRow header textItem ( oldTopBorder, oldBottomBorder ) parserHeaders =
  FullHeaderRow
    ( ( min ( .y textItem ) oldTopBorder ), ( max ( bottomBorder textItem ) oldBottomBorder ) )
    ( (::) ( MatchedHeader header textItem ) parserHeaders )


generateHeaderResults : RawPage -> Header -> HeaderParsingResult
generateHeaderResults page header =
    let
      matchingTextItems = List.filter (\textItem -> (==) header.name textItem.text ) page

    in
      case matchingTextItems of
        [] -> NoMatchingHeadersFound header
        textItemList -> MatchingHeadersFound header textItemList


type HeaderParsingResult
  = NoMatchingHeadersFound Header
  | MatchingHeadersFound Header ( List TextItem )


type alias FoundHeaders = Dict.Dict HeaderId HeaderParsingResult

type HeaderRow
  = EmptyHeaderRow ParserHeaders
  | FullHeaderRow Range ParserHeaders


unwrapParserHeaders headerRow =
  case headerRow of
    EmptyHeaderRow parserHeaders -> parserHeaders
    FullHeaderRow _ parserHeaders -> parserHeaders


mapToParserHeaders func headerRow =
  case headerRow of
    EmptyHeaderRow parserHeaders ->
      EmptyHeaderRow ( func parserHeaders )

    FullHeaderRow range parserHeaders ->
      FullHeaderRow range ( func parserHeaders )


type alias HeaderRows = List HeaderRow

type alias Range = ( Float, Float )


parseRows : Options -> RawPage -> ParserHeaders -> Transactions
parseRows options page foundHeaders =
  collectRows options foundHeaders page


truncatePage : ParserHeaders -> RawPage -> RawPage
truncatePage headers page =
    ListE.dropWhile (not << textItemAHeader headers) page
    |> ListE.dropWhile (textItemAHeader headers)


textItemAHeader : ParserHeaders -> TextItem -> Bool
textItemAHeader headers textItem =
    List.filterMap (\parserHeader ->
      case parserHeader of
        UnMatchedHeader _ -> Nothing
        MatchedHeader _ headerTextItem -> Just headerTextItem
    ) headers
    |> List.member textItem


collectRows : Options -> ParserHeaders -> RawPage -> Transactions
collectRows options foundHeaders page =
    List.foldl ( createTransactions options foundHeaders ) ( [], [] ) page
    |> appendFinalTransaction options foundHeaders
    |> List.reverse


appendFinalTransaction : Options -> ParserHeaders -> ( Row, Transactions ) -> Transactions
appendFinalTransaction options foundHeaders ( currentRow, previousTransactions ) =
    appendNewTransaction options foundHeaders currentRow previousTransactions


-- row into transaction


turnIntoTransaction : Options -> ParserHeaders -> Row -> TransactionId -> Maybe Transaction
turnIntoTransaction options headers row index =
  createTransaction index row Dict.empty headers


createTransaction : TransactionId -> Row -> Columns -> ParserHeaders -> Maybe Transaction
createTransaction index row dict parserHeaders =
  case parserHeaders of
    [] -> Just { id = index, columns = dict }

    h :: hs ->
      case parseField row h of
        Nothing -> Nothing
        Just fieldConfig ->
          createTransaction
            index
            row
            ( Dict.insert ( getHeaderName h ) fieldConfig dict ) hs


-- what is the diff between these 3 types?
type FieldValidity a
  = InvalidField
  | FieldDoesntExist
  | ValidField a


parseField : Row -> ParserHeader -> Maybe FieldConfig
parseField row parserHeader =
  case parserHeader of
    UnMatchedHeader header ->
      if header.canBeBlank then
        Just ( FieldConfig header.id BlankField )
      else
        Nothing

    MatchedHeader header headerTextItem ->
      case fieldFromColumn headerTextItem header row of
        FieldDoesntExist ->
          if header.canBeBlank then
            Just ( FieldConfig header.id BlankField )
          else
            Nothing

        InvalidField ->
          Nothing

        ValidField field ->
          Just ( FieldConfig header.id field )


fieldFromColumn : TextItem -> Header -> Row -> FieldValidity Field
fieldFromColumn headerTextItem headerConfig row =
  case headerConfig.columnType of
      StringColumn -> createStringField headerConfig headerTextItem row
      DateColumn -> createDateField headerConfig headerTextItem row
      LongTextColumn -> createTextField headerConfig headerTextItem row
      FloatColumn -> createFloatField headerConfig headerTextItem row
      MoneyColumn -> createMoneyField headerConfig headerTextItem row


createFloatField : Header -> TextItem -> Row -> FieldValidity Field
createFloatField headerConfig headerTextItem row =
  case ( getFloat headerConfig headerTextItem row ) of
    Nothing -> FieldDoesntExist
    Just float -> ValidField ( FloatField float )


createMoneyField : Header -> TextItem -> Row -> FieldValidity Field
createMoneyField headerConfig headerTextItem row =
  case ( getStrings headerConfig headerTextItem row ) of
    [] -> FieldDoesntExist
    strings ->
      String.join " " strings
      |> Money.fromString
      |> Maybe.map ( ValidField << MoneyField )
      |> Maybe.withDefault InvalidField


createTextField headerConfig headerTextItem row =
  case ( getStrings headerConfig headerTextItem row ) of
    [] -> FieldDoesntExist
    strings -> ValidField ( TextField strings )


getFloat : Header -> TextItem -> Row -> Maybe Float
getFloat headerConfig headerTextItem row =
  getStrings headerConfig headerTextItem row
  |> List.filterMap stringToFloat
  |> List.head


createDateField headerConfig headerTextItem row =
  case ( getDate headerConfig headerTextItem row ) of
    Nothing -> FieldDoesntExist
    Just str ->
      if Date.isValid str then
        ValidField ( DateField str )
      else
        InvalidField


getDate : Header -> TextItem -> Row -> Maybe String
getDate headerConfig headerTextItem row =
  getString headerConfig headerTextItem row


getString : Header -> TextItem -> Row -> Maybe String
getString headerConfig headerTextItem row =
  getStrings headerConfig headerTextItem row
  |> List.head


createStringField : Header -> TextItem -> Row -> FieldValidity Field
createStringField headerConfig headerTextItem row =
  case ( getString headerConfig headerTextItem row ) of
    Nothing -> FieldDoesntExist
    Just str -> ValidField ( StringField str )


getStrings : Header -> TextItem -> Row -> List String
getStrings headerConfig headerTextItem row =
  ( extractStrings << getAllInColumn headerConfig row ) headerTextItem


getPreviousItem : Row -> TextItem -> Maybe TextItem
getPreviousItem row textItem =
  List.sortBy .x row
  |> List.reverse
  |> ListE.dropWhile (not << (==) textItem)
  |> List.drop 1
  |> List.head

negativeIndicators = ["-", "DR"]
positiveIndicators = ["+", "CR"]
moneyIndicators = ["$"]

stringToFloat : String -> Maybe Float
stringToFloat str =
  ( String.toFloat << StringE.replace "," "" << StringE.replace "$" "" ) str


getAllInColumn : Header -> Row -> TextItem -> Row
getAllInColumn headerConfig row headerTextItem =
  List.filter (\t -> sameColumn t headerConfig headerTextItem) row


getFirstInColumn : Header -> Row -> TextItem -> Maybe TextItem
getFirstInColumn headerConfig row headerTextItem =
  getAllInColumn headerConfig row headerTextItem
  |> List.head


extractStrings : Row -> List String
extractStrings textItems =
  List.map .text textItems
  |> List.reverse


extractFloat : Maybe TextItem -> Maybe Float
extractFloat maybeTextItem =
  Maybe.andThen ( stringToFloat << .text ) maybeTextItem



-- create rows


isNewTransaction : ParserHeaders -> TextItem -> Bool
isNewTransaction foundHeaders textItem =
  case firstColumn foundHeaders of
      Nothing ->
          False

      Just ( headerConfig, headerTextItem ) ->
          sameColumn textItem headerConfig headerTextItem


firstColumn : ParserHeaders -> Maybe ( Header, TextItem )
firstColumn headers =
  List.filterMap ( \parserHeader ->
    case parserHeader of
      UnMatchedHeader _ -> Nothing
      MatchedHeader header textItem -> Just ( header, textItem )
  ) headers
  |> List.sortBy ( .x << Tuple.second )
  |> List.head


findHeader : String -> Row -> Maybe TextItem
findHeader headerName =
  ListE.find ( (==) headerName << .text )


sameColumn : TextItem -> Header -> TextItem -> Bool
sameColumn textItem headerConfig headerTextItem =
  rangesOverlap
    ( Maybe.withDefault 1 ( unwrapColumnPrecision headerConfig.columnPrecision ) )
    ( headerTextItem.x, ( rightBorder headerTextItem ) )
    ( textItem.x, rightBorder textItem )


rangesOverlap : Float -> Range -> Range -> Bool
rangesOverlap n ( headerLeft, headerRight ) ( textItemLeft, textItemRight ) =
  ( ( headerLeft - n ) <= ( textItemRight ) )
  && ( ( textItemLeft ) <= ( headerRight + n ) )


createTransactions : Options -> ParserHeaders -> TextItem -> ( Row, Transactions ) -> ( Row, Transactions )
createTransactions options foundHeaders textItem ( currentRow, previousTransactions ) =
    case ( currentRow, previousTransactions ) of
        ( [], [] ) ->
            ( [ textItem ], [] )

        _ ->
            if isNewTransaction foundHeaders textItem then
                ( [ textItem ], appendNewTransaction options foundHeaders currentRow previousTransactions )

            else
                ( (::) textItem currentRow, previousTransactions )


appendNewTransaction : Options -> ParserHeaders -> Row -> Transactions -> Transactions
appendNewTransaction options foundHeaders currentRow previousTransactions =
    turnIntoTransaction options foundHeaders currentRow (TransactionId 0)
        |> Maybe.map (\t -> (::) t previousTransactions)
        |> Maybe.withDefault previousTransactions


rightBorder : TextItem -> Float
rightBorder { width, x } =
  x + width


bottomBorder : TextItem -> Float
bottomBorder { height, y } =
  y + height


getPreviousTransactionId : Transactions -> TransactionId
getPreviousTransactionId =
    Maybe.withDefault (TransactionId 0) << Maybe.map .id << List.head


stepTransactionId : TransactionId -> TransactionId
stepTransactionId (TransactionId transactionId) =
    TransactionId <| (+) transactionId 1
