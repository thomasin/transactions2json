module Options exposing (displayUpdate, update)

import Types exposing (..)
import Utils as U


update options msg =
    case msg of
        SetColumnPrecision headerId inputString ->
            String.toFloat inputString
            |> setColumnPrecision headerId
            |> setHeaders options

        AddHeader ->
            addHeader
            |> setHeaders options

        DeleteHeader headerId ->
            deleteHeader headerId
            |> setHeaders options

        SetColumnType headerId columnType ->
            setColumnType headerId columnType
            |> setHeaders options

        SetCanBeBlank headerId canBeBlank ->
            setCanBeBlank headerId canBeBlank
            |> setHeaders options


displayUpdate options msg =
    case msg of
        SetColumnWidth headerId maybeColumnWidth ->
            setColumnWidth headerId maybeColumnWidth
            |> setHeaders options

        SetCurrentHeader inputString ->
            setCurrentHeader inputString
            |> setHeaders options

        SetDisplayAs headerId string ->
            case String.trim string of
                "" ->
                    setDisplayAs headerId Nothing
                    |> setHeaders options
                _ ->
                    setDisplayAs headerId ( Just string )
                    |> setHeaders options

        ToggleHeader headerId ->
            setOpenHeader headerId
            |> setHeaders options


setColumnPrecision headerId maybeFloat currentHeader headerList =
    HeaderOptions
        currentHeader
        ( U.applyToId headerId (\h -> { h | columnPrecision = ColumnPrecision maybeFloat } ) headerList )


setColumnWidth headerId maybeColumnWidth currentHeader headerList =
    HeaderOptions
        currentHeader
        ( U.applyToId headerId (\h -> { h | columnWidth = maybeColumnWidth } ) headerList )


setCanBeBlank headerId canBeBlank currentHeader headerList =
    HeaderOptions
        currentHeader
        ( U.applyToId headerId (\h -> { h | canBeBlank = canBeBlank } ) headerList )


setDisplayAs headerId maybeDisplayAs currentHeader headerList =
    HeaderOptions
        currentHeader
        ( U.applyToId headerId (\h -> { h | displayAs = maybeDisplayAs } ) headerList )


setHeaders options f =
    case options.headers of
        HeaderOptions currentHeader headerList ->
            { options | headers = f currentHeader headerList }


setCurrentHeader inputString currentHeader headerList =
    HeaderOptions inputString headerList


setColumnType headerId columnType currentHeader headerList =
    HeaderOptions
        currentHeader
        ( U.applyToId headerId (\h -> { h | columnType = columnType } ) headerList )


setOpenHeader headerId currentHeader headerList =
    HeaderOptions
        currentHeader
        ( openHeader headerId headerList )


addHeader currentHeader headerList =
    if List.member currentHeader ( List.map .name headerList) || ( String.trim currentHeader ) == "" then
        HeaderOptions currentHeader headerList

    else
        let
            newHeader = stringIntoHeader currentHeader headerList
 
        in    
            HeaderOptions "" ( openHeader newHeader.id <| (++) headerList [ newHeader ] )


openHeader : HeaderId -> List Header -> List Header
openHeader headerId headerList =
    let
        checkOpen header = if header.id == headerId then
                { header | isOpen = not header.isOpen }
            else
                { header | isOpen = False }
            
    in    
        List.map checkOpen headerList


stringIntoHeader string headers =
    { id = stepHeaderId headers
    , name = string
    , displayAs = Just string
    , columnType = StringColumn
    , columnWidth = Nothing
    , columnPrecision = ColumnPrecision Nothing
    , canBeBlank = True
    , isOpen = False
    }


stepHeaderId : List Header -> HeaderId
stepHeaderId headers =
    List.reverse headers
    |> List.head
    |> Maybe.map .id
    |> Maybe.map (\n -> unwrapHeaderId n + 1 )
    |> Maybe.map HeaderId
    |> Maybe.withDefault ( HeaderId 0 )


deleteHeader headerId currentHeader headerList =
    HeaderOptions currentHeader ( U.filterOut headerId headerList )
