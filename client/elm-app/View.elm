module View exposing (view)

import Html exposing (Html, text)
import Html.Attributes exposing (class, id, placeholder, value)
import Html.Events as Html exposing (onSubmit, onInput, onClick)

import Types exposing (..)

--


view model =
  Html.div []
  ( ( (++) [ dropZone ] << transactionsList ) model.data )


-- private


transactionsList : DataStatus -> List ( Html Msg )
transactionsList maybeData =
  case maybeData of
    NoData -> [ section [] ]
    TransactionsFound _ transactions ->
      [ section [ downloadForm ]
      , section
          ( ( unwrapTransactions ( List.map displayTransaction ) transactions )
            |> (++) [ displayTitle transactions, displayHeaders ]
          )
      ]

section : List ( Html msg ) -> Html msg
section content =
  Html.section [ class "section" ]
    [ Html.div [ class "container" ]
      content
    ]


-- dropzone


dropZone =  
  section
    [ Html.div [ class "dropzone", id "dropzone" ]
      [ Html.span [ class "jam jam-upload icon-upload" ] [] ]
    ]


-- download form


downloadForm =
  Html.form [ onSubmit ( FormAction PrepareForDownload ) ]
    [ Html.div [ class "field has-addons" ]
      [ Html.div [ class "control has-icons-left is-expanded" ] fileNameInput
      , Html.div [ class "control" ] [ fileExtension ]
      , Html.div [ class "control" ] [ downloadButton ]
      ]
    ]


fileExtension =
  Html.span [ class "select" ]
    [ Html.select [ onInput ( FormAction << SetFileType ) ]
      [ Html.option [ value "csv" ] [ text ".csv" ]
      , Html.option [ value "json" ] [ text ".json" ]
      ]
    ]


fileNameInput =
  [ Html.input [ class "input", placeholder "File name", onInput ( FormAction << SetFileName ) ] []
  , Html.span [ class "icon is-small is-left jam jam-files" ] []
  ]


downloadButton =
  Html.button [ class "button is-success" ] [ text "Download file" ]


-- transaction table title


displayTitle : TransactionValidity -> Html msg
displayTitle wrappedTransactions =
  Html.div [ class "has-text-centered" ]
    [ Html.h4 [ class "title is-4 has-text-centered" ] [ text "Transactions" ]
    , validityTag wrappedTransactions
    ]


validityTag wrappedTransactions =
  case wrappedTransactions of
    Valid _ -> Html.span [ class "tag tag-valid is-success" ] [ text "Valid" ]
    Invalid incorrectTransaction transactions ->  
      Html.span [ class "tag tag-valid is-danger" ] [ text (( (++) "Transaction on " << (++) incorrectTransaction.date ) " is incorrect" ) ]
  

-- transaction headers


displayHeaders : Html msg
displayHeaders =
  Html.div [ class "columns columns-header" ]
    [ headerColumn "Date" "is-2"
    , headerColumn "Description" ""
    , headerColumn "Debit" "is-1 has-text-right"
    , headerColumn "Credit" "is-1 has-text-right"
    , headerColumn "Balance" "is-1 has-text-right"
    , deleteHeader
    ]


deleteHeader =
  Html.div [ class "column is-narrow" ]
    [ Html.div [ class "header-label has-background-danger has-text-centered" ]
      [ Html.span [ class "jam jam-trash" ] [] ]
    ]


headerColumn columnLabel columnOptions =
  Html.div [ class ( (++) "column " columnOptions ) ]
    [ Html.p [ class "header-label has-background-primary" ] 
      [ text columnLabel ] ]


-- transaction list


displayTransaction : Transaction -> Html Msg
displayTransaction transaction =
  Html.div [ class "columns columns-transaction" ]
    [ Html.div [ class "column has-background-light is-2 is-bold" ] [ text transaction.date ]
    , Html.div [ class "column has-background-light" ]
      ( List.map (\str -> Html.div [] [ text str ] ) transaction.description )
    , floatColumn transaction.debit
    , floatColumn transaction.credit
    , floatColumn ( Just transaction.balance )
    , deleteColumn transaction.id
    ]


floatColumn : Maybe Float -> Html msg
floatColumn float =
  Html.div [ class "column has-background-light is-1 has-text-right" ]
    [ text ( ( Maybe.withDefault "--" << Maybe.map toString ) float ) ]


deleteColumn : Int -> Html Msg
deleteColumn transactionId =
  Html.div 
  [ class "column column-delete is-narrow has-text-centered"
  , onClick ( DeleteTransaction transactionId ) 
  ] [ Html.span [ class "jam jam-close" ] [] ]  
