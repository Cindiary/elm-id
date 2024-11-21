module ID.Counter exposing
    ( Counter
    , new
    , newID
    , newID2
    , newID3
    , newID4
    , giveIDs
    , accountForIDs
    , accountForItemIDs
    , accountForDictIDs
    , sanitizeIDs
    , encode
    , decoder
    )

{-|

@docs Counter
@docs new, newID, newID2, newID3, newID4, giveIDs
@docs accountForIDs, accountForItemIDs, accountForDictIDs
@docs sanitizeIDs

# Json
@docs encode, decoder

-}

import Internal exposing(ID(..), Counter(..), unpack)
import ID.Dict
import ID.Set
import Dict


import Json.Encode as E
import Json.Decode as D



{-|
Counter type which is used to create [`ID`](ID#ID) values
-}
type alias Counter id = Internal.Counter id


{-| Create a new counter -}
new : a -> Counter (ID a)
new =
    Internal.newCounter


{-|  Creates a new [`ID`](ID#ID) and advanced the counter, make sure to use the new counter to create any new IDs -}
newID : Counter (ID a) -> ( ID a, Counter (ID a) )
newID =
    Internal.newID


{-| Same as [`newID`](ID.Counter#newID) but creates 2 IDs instead of 1 -}
newID2 : Counter (ID a) -> ( ID a, ID a, Counter (ID a) )
newID2 (IDCounter nextID) =
    ( ID nextID
    , ID (nextID + 1)
    , IDCounter (nextID + 2)
    )


{-| Same as [`newID`](ID.Counter#newID) but creates 3 IDs instead of 1 -}
newID3 : Counter (ID a) -> ( { id1 : ID a, id2 : ID a, id3 : ID a }, Counter (ID a) )
newID3 (IDCounter nextID) =
    ( { id1 = ID nextID
      , id2 = ID (nextID + 1)
      , id3 = ID (nextID + 2)
      }
    , IDCounter (nextID + 3)
    )


{-| Same as [`newID`](ID.Counter#newID) but creates 4 IDs instead of 1 -}
newID4 : Counter (ID a) -> ( { id1 : ID a, id2 : ID a, id3 : ID a, id4 : ID a }, Counter (ID a) )
newID4 (IDCounter nextID) =
    ( { id1 = ID nextID
      , id2 = ID (nextID + 1)
      , id3 = ID (nextID + 2)
      , id4 = ID (nextID + 3)
      }
    , IDCounter (nextID + 4)
    )


{-| Assign IDs to a number of values, make sure to use the new counter to create any new IDs -}
giveIDs : (ID a -> b -> c) -> Counter (ID a) -> List b -> ( List c, Counter (ID a) )
giveIDs func (IDCounter nextID) items =
    ( List.indexedMap (\i item -> func (ID (nextID + i)) item) items, IDCounter (nextID + List.length items) )


{-|
Account for a list of IDs.
This should not be necessary if you always make sure to use the counter result from [`newID`](ID.Counter#newID), but can be used to sanitize the counter after encoding and decoding a counter and values
-}
accountForIDs : List (ID a) -> Counter (ID a) -> Counter (ID a)
accountForIDs ids counter =
    ids
    |> List.map unpack
    |> accountForIDvalues counter


{-|
Same as [`accountForIDs`](ID.Counter#accountForIDs) but can be used for a list of values containing an [`ID`](ID#ID)

    newCounter = accountForItemIDs .id users counter
-}
accountForItemIDs : (item -> ID a) -> List item -> Counter (ID a) -> Counter (ID a)
accountForItemIDs getID items counter =
    items
    |> List.map (getID >> unpack)
    |> accountForIDvalues counter


{-| Same as [`accountForIDs`](ID.Counter#accountForIDs) but can be used with a [`Dict`](ID.Dict#Dict) -}
accountForDictIDs : Internal.Dict counter (ID a) value -> Counter (ID a) -> Counter (ID a)
accountForDictIDs ( Internal.IDDict _ dict ) counter =
    Dict.keys dict
    |> accountForIDvalues counter


accountForIDvalues : Counter (ID a) -> List Int -> Counter (ID a)
accountForIDvalues ((IDCounter currentValue) as counter) values =
    case List.maximum values of
        Just maxID ->
            if currentValue <= maxID then
                IDCounter (maxID + 10)
            else
                counter

        Nothing ->
            counter


{-| Account for all used IDs in the counter (as in [`accountForIDs`](ID.Counter#accountForIDs)) and give new IDs to any items that have a duplicate [`ID`](ID#ID) -}
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


{-| Encode 'Counter' to a JSON value -}
encode : Counter id -> E.Value
encode (IDCounter value) =
    E.int value


{-| JSON Decoder for 'Counter' -}
decoder : D.Decoder ( Counter id )
decoder =
    D.map IDCounter D.int
