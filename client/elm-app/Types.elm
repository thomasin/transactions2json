module Types exposing (ColumnPrecision(..), ColumnType(..), Columns, DataStatus(..), DisplayAction(..), ExportAction(..), ExportOptions, Field(..), FieldConfig, FileName(..), FileType(..), Header, HeaderId(..), HeaderOptions(..), Headers, Model, Msg(..), Options, OptionsAction(..), PdfAction(..), RawPage, RawPdf, SidebarAction(..), SidebarState(..), TextItem, Transaction, TransactionId(..), Transactions, unwrapColumnPrecision, unwrapCurrentHeader, unwrapHeaderId, unwrapHeaders, unwrapFileName)

import Dict as Dict
import OrderedList exposing (OrderedList)
import Json.Decode as Decode



--



type alias Model =
    { data : DataStatus
    , options : Options
    , pdfs : OrderedList RawPdf
    , sidebarState : SidebarState
    }


type SidebarState
    = ParsingTab
    | ColumnsTab


type Msg
    = DomLoaded
    | PdfsLoaded ( Result Decode.Error ( List RawPdf ) )
    | DeleteTransaction TransactionId
    | ExportAction ExportAction
    | OptionsAction OptionsAction
    | DisplayAction DisplayAction
    | SidebarAction SidebarAction
    | PdfAction PdfAction


type PdfAction
    = MoveUp String
    | MoveDown String
    | DeletePdf String


type SidebarAction
    = ShowParsingTab
    | ShowColumnsTab


type OptionsAction
    = SetColumnPrecision HeaderId String
    | AddHeader
    | DeleteHeader HeaderId
    | SetColumnType HeaderId ColumnType
    | SetCanBeBlank HeaderId Bool


type DisplayAction
    = SetColumnWidth HeaderId (Maybe Int)
    | SetDisplayAs HeaderId String
    | SetCurrentHeader String
    | ToggleHeader HeaderId


type ExportAction
    = PrepareForExport
    | SetFileName FileName
    | SetFileType String


type DataStatus
    = NoData
    | TransactionsFound ExportOptions Transactions


type alias Transactions =
    List Transaction


type alias ExportOptions =
    { fileName : FileName
    , fileType : FileType
    }


type alias Options =
    { headers : HeaderOptions
    }


type HeaderOptions
    = HeaderOptions String ( List Header )


type alias Header =
    { id : HeaderId
    , name : String
    , displayAs : Maybe String
    , columnType : ColumnType
    , columnWidth : Maybe Int
    , columnPrecision : ColumnPrecision
    , canBeBlank : Bool
    , isOpen : Bool
    }


unwrapCurrentHeader : HeaderOptions -> String
unwrapCurrentHeader ( HeaderOptions currentHeader _ ) =
    currentHeader


unwrapHeaders : HeaderOptions -> List Header
unwrapHeaders ( HeaderOptions _ headerList ) =
    headerList


type ColumnPrecision
    = ColumnPrecision ( Maybe Float )


unwrapColumnPrecision : ColumnPrecision -> Maybe Float
unwrapColumnPrecision ( ColumnPrecision maybeFloat ) =
    maybeFloat


type FileType
    = Json
    | Csv


type alias Headers =
    List TextItem


type alias RawPdf =
    { fileName : String
    , pages : List ( Int, RawPage )
    }


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
    , columns : Columns
    }


type alias Columns =
    Dict.Dict String FieldConfig


type alias FieldConfig =
    { headerId : HeaderId
    , field : Field
    }


type HeaderId
    = HeaderId Int


unwrapHeaderId : HeaderId -> Int
unwrapHeaderId ( HeaderId int ) =
    int


type Field
    = StringField String
    | DateField String
    | FloatField Float
    | MoneyField Float
    | TextField (List String)
    | BlankField


type FileName
    = FileName String


unwrapFileName : FileName -> String
unwrapFileName ( FileName fileName ) =
    fileName


type TransactionId
    = TransactionId Int


type ColumnType
    = StringColumn
    | DateColumn
    | LongTextColumn
    | FloatColumn
    | MoneyColumn

