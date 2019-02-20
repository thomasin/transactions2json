module OrderedList exposing (OrderedList, create, delete, moveDown, moveUp, unwrap)

import List.Extra as List


type OrderedList a
    = OrderedList (List ( OrderedListId, a ))


create : List a -> OrderedList a
create list =
    List.indexedMap (\i x -> ( OrderedListId i, x )) list
        |> OrderedList


unwrap : OrderedList a -> List a
unwrap (OrderedList orderedList) =
    List.map Tuple.second orderedList


delete : (a -> Bool) -> OrderedList a -> OrderedList a
delete f (OrderedList orderedList) =
    List.filter (noMatch f) orderedList
        |> reOrder


moveUp f (OrderedList ol) =
    let
        ( h, t ) =
            spanNotF f ol

        itemToMoveUp =
            intoList <| List.head t

        itemToMoveDown =
            intoList <| List.last h

        head =
            Maybe.withDefault [] <| List.init h

        tail =
            List.drop 1 t
    in
    reOrder (head ++ itemToMoveUp ++ itemToMoveDown ++ tail)


moveDown : (a -> Bool) -> OrderedList a -> OrderedList a
moveDown f (OrderedList ol) =
    let
        ( h, t ) =
            spanNotF f ol

        itemToMoveDown =
            intoList <| List.head t

        itemToMoveUp =
            intoList <| List.getAt 1 t

        tail =
            List.drop 2 t
    in
    reOrder (h ++ itemToMoveUp ++ itemToMoveDown ++ tail)


unwrapId : OrderedListId -> Int
unwrapId (OrderedListId int) =
    int


type OrderedListId
    = OrderedListId Int


intoList : Maybe a -> List a
intoList maybeItem =
    case maybeItem of
        Nothing ->
            []

        Just item ->
            [ item ]


reOrder : List ( OrderedListId, a ) -> OrderedList a
reOrder list =
    OrderedList (List.indexedMap (\i ( _, x ) -> ( OrderedListId i, x )) list)


spanNotF : (a -> Bool) -> List ( OrderedListId, a ) -> ( List ( OrderedListId, a ), List ( OrderedListId, a ) )
spanNotF f l =
    List.span (noMatch f) l


noMatch : (a -> Bool) -> ( OrderedListId, a ) -> Bool
noMatch f ( _, x ) =
    not (f x)
