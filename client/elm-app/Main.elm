port module Main exposing (main)

import Decode
import Encode
import Browser
import Json.Decode
import Json.Encode
import Options as Options
import OrderedList as OrderedList
import PdfParser
import Set as Set exposing (Set)
import Types exposing (..)
import View
import Utils as U



--


port domLoaded : String -> Cmd msg


port pdfsLoaded : (Json.Decode.Value -> msg) -> Sub msg


port downloadJson : Json.Encode.Value -> Cmd msg


port downloadCsv : Json.Encode.Value -> Cmd msg


--


main =
    Browser.document
        { init = init
        , update = update
        , view = View.view
        , subscriptions = subscriptions
        }


init : {} -> ( Model, Cmd Msg )
init _ =
    ( initialModel, domLoaded "" )


initialModel =
    { data = NoData
    , options = initialOptions
    , pdfs = OrderedList.create []
    , sidebarState = ParsingTab
    }


initialOptions =
    { headers = HeaderOptions "" []
    }


initialExportOptions =
    { fileName = FileName ""
    , fileType = Csv
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DomLoaded ->
            ( model, domLoaded "" )

        PdfsLoaded (Ok pdfList) ->
            PdfParser.parsePdfs model.options pdfList
            |> loadTransactions { model | pdfs = OrderedList.create pdfList }

        DeleteTransaction transactionId ->
            case model.data of
                NoData ->
                    ( model, Cmd.none )

                TransactionsFound exportOptions transactions ->
                    ( { model | data = TransactionsFound exportOptions ( U.filterOut transactionId transactions ) }, Cmd.none )

        PdfsLoaded (Err err) ->
            ( model, Cmd.none )

        DisplayAction subMsg ->
            Options.displayUpdate model.options subMsg
            |> (\o -> ( { model | options = o }, Cmd.none ) )

        OptionsAction subMsg ->
            let
                options =
                    Options.update model.options subMsg
            in
            PdfParser.parsePdfs options ( OrderedList.unwrap model.pdfs )
            |> loadTransactions ( setOptions model options )

        ExportAction subMsg ->
            updateExportOptions model subMsg

        SidebarAction subMsg ->
            case subMsg of
                ShowParsingTab ->
                    ( { model | sidebarState = ParsingTab }, Cmd.none )

                ShowColumnsTab ->
                    ( { model | sidebarState = ColumnsTab }, Cmd.none )

        PdfAction subMsg ->
            let
                pdfs =
                    case subMsg of
                        MoveUp fileName ->
                            OrderedList.moveUp (\p -> p.fileName == fileName ) model.pdfs

                        MoveDown fileName ->
                            OrderedList.moveDown (\p -> p.fileName == fileName ) model.pdfs

                        DeletePdf fileName ->
                            OrderedList.delete (\p -> p.fileName == fileName ) model.pdfs
            in
            PdfParser.parsePdfs model.options (OrderedList.unwrap pdfs)
            |> loadTransactions { model | pdfs = pdfs }


setOptions model options =
    { model | options = options }


updateExportOptions model msg =
    case model.data of
        NoData ->
            ( model, Cmd.none )

        TransactionsFound exportOptions transactions ->
            case msg of
                SetFileName fileName ->
                    ( { model | data = TransactionsFound { exportOptions | fileName = fileName } transactions }, Cmd.none )

                SetFileType fileType ->
                    ( { model | data = TransactionsFound { exportOptions | fileType = findFileType fileType } transactions }, Cmd.none )

                PrepareForExport ->
                    ( model, prepareForDownload exportOptions ( unwrapHeaders model.options.headers ) transactions )


prepareForDownload { fileType, fileName } headerList transactions =
    case fileType of
        Json ->
            downloadJson ( Encode.asJson fileName transactions )

        Csv ->
            downloadCsv ( Encode.asCsv fileName headerList transactions )


loadTransactions model transactions =
    ( { model
      | data = TransactionsFound initialExportOptions transactions
      }
    , Cmd.none
    )


findFileType fileType =
    case fileType of
        "json" ->
            Json

        "csv" ->
            Csv

        _ ->
            Csv


subscriptions : Model -> Sub Msg
subscriptions model =
    pdfsLoaded (PdfsLoaded << Json.Decode.decodeValue Decode.decodePdfList)
