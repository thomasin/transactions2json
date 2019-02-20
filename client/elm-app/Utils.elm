module Utils exposing (filterOut, displayHeaderName, applyToId, isNothing, roundTo)


import Types exposing (..)


filterOut : a -> List { b | id : a } -> List { b | id : a }
filterOut id list =
    List.filter (\i -> not ((==) i.id id)) list


displayHeaderName : Header -> String
displayHeaderName { name, displayAs } =
  Maybe.withDefault name displayAs


applyToId : a -> ( { b | id : a } -> { b | id : a } ) -> List { b | id : a } -> List { b | id : a } 
applyToId id func list =
    let
        setter x =
            if x.id == id then
                func x
            else
                x
    in
        List.map setter list


isNothing : Maybe a -> Bool
isNothing maybe =
    case maybe of
        Nothing -> True
        Just _ -> False


roundTo : Int -> Float -> Float
roundTo places value =
    let
        factor = toFloat ( 10 ^ places )

    in
    (   ( value * factor)
        |> round
        |> toFloat
    ) / factor