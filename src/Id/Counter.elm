module Id.Counter exposing
    ( Counter
    , new
    , newId
    , newId2
    , newId3
    , newId4
    , giveIds
    , accountForId
    , accountForIds
    , accountForItemIds
    , sanitizeIds
    , encode
    , decoder
    )

{-|

@docs Counter
@docs new, newId, newId2, newId3, newId4, giveIds
@docs accountForId, accountForIds, accountForItemIds
@docs sanitizeIds

# Json
@docs encode, decoder

-}

import Internal exposing(Id(..), Counter(..), unpackId)
import Set


import Json.Encode as E
import Json.Decode as D



{-|
Counter type which is used to create [`Id`](Id#Id) values
-}
type alias Counter id = Internal.Counter id


{-| Create a new counter -}
new : a -> Counter (Id a)
new _ =
    IdCounter 1


{-|  Creates a new [`Id`](Id#Id) and advanced the counter, make sure to use the new counter to create any new Ids -}
newId : Counter (Id a) -> ( Id a, Counter (Id a) )
newId (IdCounter nextId) =
    ( Id nextId, IdCounter (nextId + 1) )


{-| Same as [`newId`](Id.Counter#newId) but creates 2 Ids instead of 1 -}
newId2 : Counter (Id a) -> ( Id a, Id a, Counter (Id a) )
newId2 (IdCounter nextId) =
    ( Id nextId
    , Id (nextId + 1)
    , IdCounter (nextId + 2)
    )


{-| Same as [`newId`](Id.Counter#newId) but creates 3 Ids instead of 1 -}
newId3 : Counter (Id a) -> ( { id1 : Id a, id2 : Id a, id3 : Id a }, Counter (Id a) )
newId3 (IdCounter nextId) =
    ( { id1 = Id nextId
      , id2 = Id (nextId + 1)
      , id3 = Id (nextId + 2)
      }
    , IdCounter (nextId + 3)
    )


{-| Same as [`newId`](Id.Counter#newId) but creates 4 Ids instead of 1 -}
newId4 : Counter (Id a) -> ( { id1 : Id a, id2 : Id a, id3 : Id a, id4 : Id a }, Counter (Id a) )
newId4 (IdCounter nextId) =
    ( { id1 = Id nextId
      , id2 = Id (nextId + 1)
      , id3 = Id (nextId + 2)
      , id4 = Id (nextId + 3)
      }
    , IdCounter (nextId + 4)
    )


{-| Assign Ids to a number of values, make sure to use the new counter to create any new Ids -}
giveIds : (Id a -> b -> c) -> Counter (Id a) -> List b -> ( List c, Counter (Id a) )
giveIds func (IdCounter nextId) items =
    ( List.indexedMap (\i item -> func (Id (nextId + i)) item) items, IdCounter (nextId + List.length items) )


{-|
Account for a single Ids.
This should not be necessary if you always make sure to use the counter result from [`newId`](Id.Counter#newId), but can be used to sanitize the counter after encoding and decoding a counter and values
-}
accountForId : Id a -> Counter (Id a) -> Counter (Id a)
accountForId id ( IdCounter nextId as counter ) =
    if unpackId id >= nextId then
        IdCounter ( unpackId id + 10 )
    else
        counter


idMax : Id a -> Id a -> Id a
idMax id1 id2 =
    if unpackId id1 > unpackId id2 then
        id1
    else
        id2


{-|
Account for a list of Ids.
This should not be necessary if you always make sure to use the counter result from [`newId`](Id.Counter#newId), but can be used to sanitize the counter after encoding and decoding a counter and values
-}
accountForIds : List (Id a) -> Counter (Id a) -> Counter (Id a)
accountForIds ids counter =
    case ids of
        id :: ls ->
            let
                maxId = List.foldl idMax id ls
            in
            accountForId maxId counter

        [] ->
            counter


{-|
Same as [`accountForIds`](Id.Counter#accountForIds) but can be used for a list of values containing an [`Id`](Id#Id)

    newCounter = accountForItemIds .id users counter
-}
accountForItemIds : (item -> Id a) -> List item -> Counter (Id a) -> Counter (Id a)
accountForItemIds getId items counter =
    case items of
        item :: ls ->
            let
                maxId = List.foldl ( getId >> idMax ) ( getId item ) ls
            in
            accountForId maxId counter
        
        [] -> 
            counter


{-| Account for all used Ids in the counter (as in [`accountForIds`](Id.Counter#accountForIds)) and give new Ids to any items that have a duplicate [`Id`](Id#Id) -}
sanitizeIds : (val -> Id a) -> (val -> Id a -> val) -> List val -> Counter (Id a) -> ( List val, Counter (Id a) )
sanitizeIds getter setter items counter =
    let
        foldFunc item ( ls, count, usedIds ) =
            let
                idValue = unpackId (getter item)
            in
            if Set.member idValue usedIds then
                case newId count of
                    ( id, newCounter ) ->
                        ( setter item id :: ls
                        , newCounter
                        , Set.insert (unpackId id) usedIds
                        )

            else
                ( item :: ls
                , count
                , Set.insert idValue usedIds
                )

        ( newItems, newCounter_, _ ) =
            List.foldr foldFunc ( [], accountForItemIds getter items counter, Set.empty ) items
    in
    ( newItems, newCounter_ )


{-| Encode 'Counter' to a JSON value -}
encode : Counter id -> E.Value
encode (IdCounter value) =
    E.int value


{-| JSON Decoder for 'Counter' -}
decoder : D.Decoder ( Counter id )
decoder =
    D.map IdCounter D.int
