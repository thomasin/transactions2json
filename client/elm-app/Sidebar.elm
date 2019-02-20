module Sidebar exposing (view)

import Html as Html exposing (Html, text)
import Html.Attributes exposing (class, placeholder, type_, value, checked, selected)
import Html.Events exposing (onClick, onInput, onSubmit, onCheck)
import OrderedList as OrderedList
import Types exposing (..)



-- sidebar


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [ class "tabs is-small" ]
            [ Html.ul []
                ( List.map ( showSidebarNavItem model.sidebarState )
                    [ ( "Parsing", ParsingTab, ShowParsingTab )
                    , ( "Columns", ColumnsTab, ShowColumnsTab )
                    ]
                )
            ]
        , sidebarWrapper model
        ]


showSidebarNavItem : SidebarState -> ( String, SidebarState, SidebarAction ) -> Html Msg
showSidebarNavItem sidebarState ( linkText, value, msg ) =
  let
    isActive =
        if sidebarState == value then
            "is-active"

        else
            ""
  in
    Html.li [ class isActive ]
        [ Html.a [ onClick ( SidebarAction msg ) ]
          [ text linkText ]
        ]


sidebarWrapper : Model -> Html Msg
sidebarWrapper model =
    Html.aside [ class "menu" ]
        ( case model.sidebarState of
            ParsingTab ->
                displayParsingTab model.options ( OrderedList.unwrap model.pdfs )

            ColumnsTab ->
                displayColumnsTab model.options
        )


displayParsingTab : Options -> List RawPdf -> List (Html Msg)
displayParsingTab options pdfs =
    [ Html.p [ class "menu-label" ] [ text "Pdf Parsing" ]
    , Html.ul [ class "menu-list" ] [ displayPdfs pdfs ]
    ]


displayPdfs : List RawPdf -> Html Msg
displayPdfs pdfs =
    Html.div [ class "field" ]
        ( (::) ( displayLabel "Loaded pdfs" )
          ( List.map displayPdfOptions pdfs )
        )


displayPdfOptions : RawPdf -> Html Msg
displayPdfOptions pdf =
  Html.div [ class "field has-addons" ]
      [ Html.div [ class "control is-expanded" ]
        [ Html.div [ class "pdf-row is-size-7" ]
          [ text pdf.fileName ]
        ]
      , displayPdfActionButton pdf MoveUp "is-primary" "jam-arrow-up"
      , displayPdfActionButton pdf MoveDown "is-primary" "jam-arrow-down"
      , displayPdfActionButton pdf DeletePdf "is-danger" "jam-close"
      ]


displayPdfActionButton : RawPdf -> ( String -> PdfAction ) -> String -> String -> Html Msg
displayPdfActionButton pdf actionMsg bgColour icon =
  Html.div [ class "control" ]
    [ Html.button 
      [ class ( (++) "button is-small " bgColour )
      , onClick ( PdfAction ( actionMsg pdf.fileName ) ) 
      ] [ Html.span [ class ( (++) "jam " icon ) ] [] ]
    ]


displayColumnsTab : Options -> List (Html Msg)
displayColumnsTab options =
  [ Html.p [ class "menu-label" ] [ text "Columns" ]
  , Html.ul [ class "menu-list" ]
    [ displayHeaders options.headers ]
  ]


displayHeaders : HeaderOptions -> Html Msg
displayHeaders headers =
  Html.form [ onSubmit ( OptionsAction AddHeader ) ]
    [ Html.div [ class "field" ]
      [ displayLabel "Add columns"
      , Html.div [ class "field has-addons" ]
        [ Html.div [ class "control is-expanded" ]
          [ inputElement
              ( unwrapCurrentHeader headers )
              ( DisplayAction << SetCurrentHeader )
              "Must match headers in pdf"
          ]
        , Html.div [ class "control" ]
          [ Html.button [ class "button is-primary is-small", type_ "submit" ]
            [ text "+" ]
          ]
        ]
      ]
    , Html.div [ class "" ]
        ( List.map headerBox ( unwrapHeaders headers ) )
    ]


inputElement : String -> ( String -> Msg ) -> String -> Html Msg
inputElement fill msg ph =
    Html.input
        [ class "input is-small"
        , type_ "text"
        , value fill
        , onInput msg
        , placeholder ph
        ] []


displayInputField : String -> String -> ( String -> Msg ) -> String -> Html Msg
displayInputField labelText fromString msg placeholder =
  Html.div [ class "field" ]
    [ displayLabel labelText
    , Html.div [ class "control" ]
      [ inputElement fromString msg placeholder ]
    ]


displaySelectField : String -> Html Msg -> Html Msg
displaySelectField labelText dropdown =
  Html.div [ class "field" ]
    [ displayLabel labelText
    , Html.div [ class "control is-expanded" ]
        [ Html.div [ class "select is-small is-fullwidth" ]
            [ dropdown ]
        ]
    ]


displayCheckbox labelText checkboxState msg =
  Html.div [ class "field" ]
    [ Html.label [ class "label is-small checkbox" ]
      [ Html.input [ type_ "checkbox", checked checkboxState, onCheck msg ]
        []
      , Html.span [] [ text labelText ]
      ]
    ]


displayLabel : String -> Html Msg
displayLabel labelText =
    Html.label [ class "label is-small" ] [ text labelText ]


stringFromMaybeInt : Maybe Int -> String
stringFromMaybeInt =
  ( Maybe.withDefault "" << Maybe.map String.fromInt )


stringFromMaybeFloat : Maybe Float -> String
stringFromMaybeFloat =
  ( Maybe.withDefault "" << Maybe.map String.fromFloat )


stringFromMaybeString : Maybe String -> String
stringFromMaybeString =
  Maybe.withDefault ""


headerBox : Header -> Html Msg
headerBox header =
    Html.div [ class ( "card header-card " ++ ( if header.isOpen then "" else " header-is-closed" ) ) ]
        [ Html.div [ class "card-content" ]
            [ Html.div [ class "menu-label", onClick ( ( DisplayAction << ToggleHeader ) header.id ) ] [ text header.name ]
            , displayInputField "Display as"
                ( stringFromMaybeString header.displayAs )
                ( DisplayAction << SetDisplayAs header.id )
                "If left blank will be the same as in the pdf"
            , displaySelectField "Column type" ( headerTypeDropdown header.id header.columnType )
            , displayInputField "Column width"
                ( stringFromMaybeInt header.columnWidth )
                ( DisplayAction << SetColumnWidth header.id << String.toInt )
                "If left blank will fill up remaining space"
            , displayInputField "Parsing column precision"
                ( ( stringFromMaybeFloat << unwrapColumnPrecision ) header.columnPrecision )
                ( OptionsAction << SetColumnPrecision header.id )
                "If left blank will default to 1"
            , displayCheckbox "Column can be blank"
                header.canBeBlank
                ( OptionsAction << SetCanBeBlank header.id )
        , Html.div [ class "card-footer" ]
            [ Html.div [ class "card-footer-item" ]
                [ Html.button
                  [ class "button is-light is-small"
                  , onClick ( OptionsAction ( DeleteHeader header.id ) )
                  ] [ text "Delete column" ]
                ]
            ]
        ]
      ]


headerTypeValues : List ( ColumnType, String )
headerTypeValues =
    [ ( StringColumn, "Short String" )
    , ( DateColumn, "Date" )
    , ( LongTextColumn, "Long text" )
    , ( FloatColumn, "Float" )
    , ( MoneyColumn, "Money" )
    ]


headerTypeDropdown : HeaderId -> ColumnType -> Html Msg
headerTypeDropdown headerId currentlySelected =
    let
        optionFromTuple ( value, label ) =
            Html.option [ selected ( value == currentlySelected ) ] [ text label ]

        options =
            List.map optionFromTuple headerTypeValues

        valueFromLabel label =
            List.filter (\( v, l ) -> (==) l label) headerTypeValues
                |> List.head
                |> Maybe.map Tuple.first
                |> Maybe.withDefault StringColumn
    in
    Html.select [ onInput ( OptionsAction << SetColumnType headerId << valueFromLabel ) ]
        options
