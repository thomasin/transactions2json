module Date exposing (isValid)

import Iso8601 as Iso
import Parser exposing (..)
import List.Extra as List


--


isValid : String -> Bool
isValid string =
  case ( Iso.toTime string ) of
    Err _ ->
        case ( run parser ( String.trim string ) ) of
            Err errMsg ->
                let
                    jnjkn = Debug.log "dateParsing" ( Debug.toString { date = string, err = errMsg } )
                in
                    False

            Ok _ -> True
    Ok _ -> True


--


type alias Date =
    { day : Day
    , month : Month
    , year : Maybe Year
    }


type Day = Day Int
type Year = Year Int
type Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec


allMonths = [ Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec ]


parser : Parser Date
parser =
    succeed identity
    |= dateParser
    |> andThen validateDate


dateParser : Parser Date
dateParser =
    succeed Date
    |= dayParser
    |. dateSeparator
    |= monthParser
    |. dateSeparator
    |= yearParser


validateDate : Date -> Parser Date
validateDate { day, month, year } =
    if validDay day month then
        succeed ( Date day month year )
    else
        problem "day isnt valid"


dateSeparator =
    oneOf
        [ symbol "/"
        , symbol "-"
        , spaces
        ]


dayParser : Parser Day
dayParser =
    leadingZeroInt ( succeed << Day )


monthParser : Parser Month
monthParser =
    oneOf
        [ leadingZeroInt checkIsIntMonth
        , stringMonthParser
        ]


stringMonthParser : Parser Month
stringMonthParser =
    chompWhile Char.isAlpha
    |> getChompedString
    |> andThen checkIsStringMonth


leadingZeroInt : ( Int -> Parser a ) -> Parser a
leadingZeroInt f =
    let
        stringToInt i = case String.toInt i of
            Just int -> succeed int
            Nothing -> problem "couldnt turn string into int"

    in     
        chompWhile Char.isDigit
        |> getChompedString
        |> andThen stringToInt
        |> andThen f


checkIsIntMonth : Int -> Parser Month
checkIsIntMonth int =  
    case ( intToMaybeMonth int ) of
        Just month -> succeed month
        Nothing -> problem "int is not a valid month"


checkIsStringMonth : String -> Parser Month
checkIsStringMonth string =
    case ( monthFromString string ) of
        Just month -> succeed month
        Nothing -> problem ( "string (" ++ string ++ ") is not a month" )


yearParser : Parser ( Maybe Year )
yearParser =
    oneOf
        [ int |> andThen checkIsYear
        , succeed Nothing
        ]


checkIsYear : Int -> Parser ( Maybe Year )
checkIsYear num =
    if ( num < 3000 ) then 
        succeed ( Just ( Year num ) )
    else
        problem "number is way too high (-:"


intToMaybeMonth : Int -> Maybe Month
intToMaybeMonth int =
  case int of
    1 -> Just Jan
    2 -> Just Feb
    3 -> Just Mar
    4 -> Just Apr
    5 -> Just May
    6 -> Just Jun
    7 -> Just Jul
    8 -> Just Aug
    9 -> Just Sep
    10 -> Just Oct
    11 -> Just Nov
    12 -> Just Dec
    _ -> Nothing


validDay : Day -> Month -> Bool
validDay ( Day dayNum ) month =
  dayNum >= 1 && dayNum <= ( daysInMonth month )


stringsForMonth : Month -> List String
stringsForMonth month =
    case month of
        Jan -> [ "january", "jan" ]
        Feb -> [ "february, feb" ]
        Mar -> [ "march", "mar" ]
        Apr -> [ "april", "apr" ]
        May -> [ "may" ]
        Jun -> [ "june", "jun" ]
        Jul -> [ "july", "jul" ]
        Aug -> [ "august", "aug" ]
        Sep -> [ "september", "sep" ]
        Oct -> [ "october", "oct" ]
        Nov -> [ "november", "nov" ]
        Dec -> [ "december", "dec" ]


monthFromString : String -> Maybe Month
monthFromString string =
    List.find ( List.member ( String.toLower string ) << stringsForMonth ) allMonths


daysInMonth : Month -> Int
daysInMonth month =
  case month of
    Jan -> 31
    Feb -> 29
    Mar -> 31
    Apr -> 30
    May -> 31
    Jun -> 30
    Jul -> 31
    Aug -> 31
    Sep -> 30
    Oct -> 31
    Nov -> 30
    Dec -> 31
