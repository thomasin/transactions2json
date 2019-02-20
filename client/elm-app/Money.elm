module Money exposing (fromString)

import Parser exposing (..)

--

type Money = Money Float

positiveIndicators = ["+", "CR"]
negativeIndicators = ["-", "DR"]
currencyIndicators = ["$"]
moneySymbols = positiveIndicators ++ negativeIndicators ++ currencyIndicators

fromString string =
  stripString string
  |> Debug.log "moeny parse result"
  |> Result.toMaybe
  |> Maybe.map ( signFloat string )

stripString string =
  run moneyParser ( String.trim <| String.replace "," "" string )

moneyParser : Parser Float
moneyParser =
  succeed identity
  |. oneOf ( (++) ( List.map symbol moneySymbols ) [ spaces ] )
  |. oneOf ( (++) ( List.map symbol moneySymbols ) [ spaces ] )
  |= float
  |. oneOf ( (++) ( List.map symbol moneySymbols ) [ spaces ] )

signFloat string float =
  if List.any (\e -> String.contains e string) negativeIndicators then
    float * -1
  else
    float
