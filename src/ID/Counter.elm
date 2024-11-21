module ID.Counter exposing
    ( Counter
    , new
    , newID
    , newID2
    , newID3
    , newID4
    , accountForIDs
    , accountForItemIDs
    , accountForDictIDs
    , giveIDs
    , sanitizeIDs
    , encode
    , decoder
    )

import Internal exposing(ID(..), unpack)
import ID.Dict
import ID.Set
import Dict


import Json.Encode as E
import Json.Decode as D


type Counter id
    = IDCounter Int


new : a -> Counter (ID a)
new _ =
    IDCounter 0


accountForIDs : List (ID a) -> Counter (ID a) -> Counter (ID a)
accountForIDs =
    accountForItemIDs identity


accountForItemIDs : (item -> ID a) -> List item -> Counter (ID a) -> Counter (ID a)
accountForItemIDs getID items ((IDCounter currentValue) as counter) =
    case List.maximum (List.map (getID >> unpack) items) of
        Just maxID ->
            if currentValue <= maxID then
                IDCounter (maxID + 10)

            else
                counter

        Nothing ->
            counter


accountForDictIDs : ID.Dict.Dict (ID a) value -> Counter (ID a) -> Counter (ID a)
accountForDictIDs ( Internal.IDDict dict ) ((IDCounter currentValue) as counter) =
    case List.maximum ( Dict.keys dict ) of
        Just maxID ->
            if currentValue <= maxID then
                IDCounter (maxID + 10)

            else
                counter

        Nothing ->
            counter


sanitizeIDs : (val -> ID a) -> (val -> ID a -> val) -> List val -> Counter (ID a) -> ( List val, Counter (ID a) )
sanitizeIDs getter setter items counter =
    let
        foldFunc item ( ls, count, usedIDs ) =
            if ID.Set.member (getter item) usedIDs then
                case newID count of
                    ( id, newCounter ) ->
                        ( setter item id :: ls
                        , newCounter
                        , ID.Set.insert id usedIDs
                        )

            else
                ( item :: ls
                , count
                , ID.Set.insert (getter item) usedIDs
                )

        ( newItems, newCounter_, _ ) =
            List.foldr foldFunc ( [], accountForItemIDs getter items counter, ID.Set.empty ) items
    in
    ( newItems, newCounter_ )


newID : Counter (ID a) -> ( ID a, Counter (ID a) )
newID (IDCounter nextID) =
    ( ID nextID, IDCounter (nextID + 1) )


newID2 : Counter (ID a) -> ( ID a, ID a, Counter (ID a) )
newID2 (IDCounter nextID) =
    ( ID nextID
    , ID (nextID + 1)
    , IDCounter (nextID + 2)
    )


newID3 : Counter (ID a) -> ( { id1 : ID a, id2 : ID a, id3 : ID a }, Counter (ID a) )
newID3 (IDCounter nextID) =
    ( { id1 = ID nextID
      , id2 = ID (nextID + 1)
      , id3 = ID (nextID + 2)
      }
    , IDCounter (nextID + 3)
    )


newID4 : Counter (ID a) -> ( { id1 : ID a, id2 : ID a, id3 : ID a, id4 : ID a }, Counter (ID a) )
newID4 (IDCounter nextID) =
    ( { id1 = ID nextID
      , id2 = ID (nextID + 1)
      , id3 = ID (nextID + 2)
      , id4 = ID (nextID + 3)
      }
    , IDCounter (nextID + 4)
    )


giveIDs : (ID a -> b -> c) -> Counter (ID a) -> List b -> ( List c, Counter (ID a) )
giveIDs func (IDCounter nextID) items =
    ( List.indexedMap (\i item -> func (ID (nextID + i)) item) items, IDCounter (nextID + List.length items) )


encode : Counter id -> E.Value
encode (IDCounter value) =
    E.int value


decoder : D.Decoder ( Counter id )
decoder =
    D.map IDCounter D.int
