module View exposing (view)

import Dict as Dict
import Html exposing (Html, text)
import Html.Attributes exposing (checked, class, id, placeholder, type_, value)
import Html.Events as Html exposing (onCheck, onClick, onInput, onSubmit)
import Sidebar as Sidebar
import Types exposing (..)
import Browser exposing (Document)



--


view : Model -> Document Msg
view model =
    Document
        "..."
        [ Html.div [ class "columns layout-columns" ]
            [ Html.div [ class "column is-3 sidebar" ] [ Sidebar.view model ]
            , Html.div [ class "column" ]
                (((++) [ dropZone ] << transactionsList) model)
            ]
        ]



-- private


transactionsList : Model -> List ( Html Msg )
transactionsList { data, options } =
    case data of
        NoData ->
            [ section [] ]

        TransactionsFound _ transactions ->
            [ section [ downloadForm ]
            , section
                (   List.map ( displayTransaction ( unwrapHeaders options.headers ) ) transactions
                    |> (++) [ displayTitle, displayHeaders options.headers ]
                )
            ]


section : List (Html msg) -> Html msg
section content =
    Html.section [ class "section" ]
        [ Html.div [ class "container is-fluid" ]
            content
        ]



-- dropzone


dropZone : Html msg
dropZone =
    section
        [ Html.div [ class "dropzone", id "dropzone" ]
            [ Html.span [ class "jam jam-upload icon-upload" ] [] ]
        ]



-- download form


downloadForm : Html Msg
downloadForm =
    Html.form [ onSubmit (ExportAction PrepareForExport) ]
        [ Html.div [ class "field has-addons" ]
            [ Html.div [ class "control has-icons-left is-expanded" ] fileNameInput
            , Html.div [ class "control" ] [ fileExtension ]
            , Html.div [ class "control" ] [ downloadButton ]
            ]
        ]


fileExtension : Html Msg
fileExtension =
    Html.span [ class "select" ]
        [ Html.select [ onInput (ExportAction << SetFileType) ]
            [ Html.option [ value "csv" ] [ text ".csv" ]
            , Html.option [ value "json" ] [ text ".json" ]
            ]
        ]


fileNameInput : List (Html Msg)
fileNameInput =
    [ Html.input [ class "input", placeholder "File name", onInput (ExportAction << SetFileName << FileName) ] []
    , Html.span [ class "icon is-small is-left jam jam-files" ] []
    ]


downloadButton : Html msg
downloadButton =
    Html.button [ class "button is-success" ] [ text "Download file" ]


displayTitle : Html msg
displayTitle =
    Html.div [ class "has-text-centered" ]
        [ Html.h4 [ class "title is-4 has-text-centered" ] [ text "Transactions" ]
        ]


displayHeaders : HeaderOptions -> Html msg
displayHeaders headerOptions =
    Html.div [ class "columns columns-header" ]
        (List.map displayHeader (unwrapHeaders headerOptions))


displayHeader : Header -> Html msg
displayHeader header =
    case header.columnType of
        FloatColumn ->
            headerColumn ( Maybe.withDefault header.name header.displayAs ) ((++) "has-text-right" (getColumnWidth header.columnWidth))

        MoneyColumn ->
            headerColumn ( Maybe.withDefault header.name header.displayAs ) ((++) "has-text-right" (getColumnWidth header.columnWidth))

        _ ->
            headerColumn ( Maybe.withDefault header.name header.displayAs ) (getColumnWidth header.columnWidth)


getColumnWidth : Maybe Int -> String
getColumnWidth columnWidth =
    (++) ((Maybe.withDefault " " << Maybe.map String.fromInt) columnWidth) " "
        |> (++) " is-"


findHeader headerId headers =
    List.filter (\h -> (==) h.id headerId) headers
        |> List.head


deleteHeader : Html msg
deleteHeader =
    Html.div [ class "column is-narrow" ]
        [ Html.div [ class "header-label has-background-danger has-text-centered" ]
            [ Html.span [ class "jam jam-trash" ] [] ]
        ]


headerColumn : String -> String -> Html msg
headerColumn columnLabel columnOptions =
    Html.div [ class ((++) "column " columnOptions) ]
        [ Html.p [ class "header-label has-background-primary" ]
            [ text columnLabel ]
        ]



-- transaction list


displayTransaction : List Header -> Transaction -> Html Msg
displayTransaction headers transaction =
    Html.div [ class "columns columns-transaction" ]
        (Dict.toList transaction.columns
            |> List.sortBy (unwrapHeaderId << .headerId << Tuple.second)
            |> List.map (displayTransactionField headers)
        )



--( List.map displayTransactionField << List.sortBy ( unwrapHeaderId << .headerId << Tuple.second ) << Dict.toList ) transaction.columns )


displayTransactionField : List Header -> ( String, FieldConfig ) -> Html msg
displayTransactionField headers ( _, { headerId, field } ) =
    let
        options =
            generateDisplayOptions <| findHeader headerId headers
    in
    displayColumn options <|
        case field of
            StringField string ->
                stringColumn string

            DateField string ->
                stringColumn string

            TextField strings ->
                textColumn strings

            FloatField maybeFloat ->
                floatColumn maybeFloat

            MoneyField float ->
                floatColumn float

            BlankField ->
                stringColumn "--"


displayColumn options column =
    column options


generateDisplayOptions maybeHeader =
    case maybeHeader of
        Nothing ->
            defaultDisplayOptions

        Just header ->
            { columnWidth = getColumnWidth header.columnWidth
            , typeSpecific =
                case header.columnType of
                    StringColumn ->
                        " "

                    FloatColumn ->
                        " has-text-right "

                    DateColumn ->
                        " is-bold "

                    LongTextColumn ->
                        " "

                    MoneyColumn ->
                        " has-text-right"
            }


defaultDisplayOptions =
    { columnWidth = " "
    , typeSpecific = ""
    }


type alias DisplayOptions =
    { columnWidth : String
    , typeSpecific : String
    }



--boolColumn : TransactionId -> Bool -> ( TransactionId -> Msg ) -> Html Msg
--boolColumn transactionId bool msg =
--  Html.div [ class "column has-background-light is-1 has-text-centered" ]
--    [ Html.input [ type_ "checkbox", checked bool, onClick ( msg transactionId ) ] []
--    ]


floatColumn : Float -> DisplayOptions -> Html msg
floatColumn float displayOptions =
    genericColumn displayOptions
        [ text ( String.fromFloat float ) ]


stringColumn : String -> DisplayOptions -> Html msg
stringColumn string displayOptions =
    genericColumn displayOptions
        [ text string ]


textColumn : List String -> DisplayOptions -> Html msg
textColumn strings displayOptions =
    genericColumn displayOptions
        (List.map (\str -> Html.div [] [ text str ]) strings)


deleteColumn : TransactionId -> Html Msg
deleteColumn transactionId =
    Html.div
        [ class "column column-delete is-narrow has-text-centered"
        , onClick (DeleteTransaction transactionId)
        ]
        [ Html.span [ class "jam jam-close" ] [] ]


genericColumn displayOptions content =
    Html.div
        [ class ((++) "column has-background-light" <| (++) displayOptions.typeSpecific displayOptions.columnWidth) ]
        content
