module Id.CounterList exposing
    ( CounterList
    , empty
    , singleton
    , fromList
    , fromIdList
    , getCounter
    , toList
    , isEmpty
    , length
    , member
    , get
    , getFrom
    , add
    , addInto
    , addAt
    , addList
    , insert
    , insertAt
    , move
    , remove
    , update
    , map
    , filter
    , filterMap
    , foldl
    , foldr
    , ids
    , values
    , partition
    , diff
    , intersect
    , append
    , unzip
    , encode
    , decoder
    )

{-|
@docs CounterList

Most of the functions in this module are a alias for the same function in [`Id.List`](Id.List), which can handle both 'Id.List' and 'CounterList'

@docs empty, singleton, fromList, fromIdList, getCounter, toList
@docs isEmpty, length
@docs member, get, getFrom
@docs add, addInto, addAt, addList
@docs insert, insertAt, move
@docs remove, update
@docs map, filter, filterMap, foldl, foldr
@docs ids, values
@docs partition, diff, intersect, append
@docs unzip

# Json
@docs encode, decoder
-}

import Internal exposing (Id(..), Counter(..), accountForId)

import Json.Encode as E
import Json.Decode as D

import Id.List
import Id.Counter

import Parser as P exposing((|.), (|=))


{-| A list with a id counter -}
type CounterList id value = CounterList Int ( Id.List.List id value )


pack : Int -> Id.List.List id value -> CounterList id value
pack =
    CounterList


packInto : CounterList id a -> Id.List.List id value -> CounterList id value
packInto ( CounterList counter _ ) list =
    CounterList counter list


{-| Create a empty `CounterList` -}
empty : Counter ( Id a ) -> CounterList (Id a) value
empty ( IdCounter counter ) =
    pack counter []


{-| Create a 'CounterList' with a single Id-value pair -}
singleton : Counter (Id a) -> value -> ( Id a, CounterList (Id a) value )
singleton counter value =
    add value ( empty counter )


{-| Create a new `CounterList` from a list of values and assign an [`Id`](Id#Id) to every value -}
fromList : Counter ( Id a ) -> List value -> CounterList (Id a) value
fromList ( IdCounter counterValue ) list =
    pack
    ( counterValue + List.length list )
    ( List.indexedMap ( \index value -> ( Id ( counterValue + index ), value ) ) list )


{-|
Create a new `CounterList` from a list of Id-value pairs.
This removes pairs with duplicate Ids
-}
fromIdList : Counter ( Id a ) -> Id.List.List ( Id a ) value -> CounterList (Id a) value
fromIdList counter list =
    let
        ( IdCounter sanitizedCounter ) = Id.List.accountForIds list counter
    in
    pack sanitizedCounter ( Id.List.removeDuplicateIds list )


getCounter_ : CounterList id value -> Int
getCounter_ ( CounterList value _ ) =
    value


{-| Get the counter from a counter list -}
getCounter : a -> CounterList ( Id a ) value -> Counter ( Id a )
getCounter _ list =
    IdCounter ( getCounter_ list )


{-| Convert a CounterList into a [`Id.List`](Id.List#List). -}
toList : CounterList id value -> Id.List.List id value
toList ( CounterList _ list ) =
    list


{-| Add a value to the List and assign it a new [`Id`](Id#Id). -}
add : value -> CounterList (Id a) value -> ( Id a, CounterList (Id a) value )
add value ( CounterList idValue list ) =
    let id = Id idValue in
    ( id, pack ( idValue + 1 ) ( ( id, value ) :: list ) )


{-| Same as as [`add`](Id.List#add) but only returns the List -}
addInto : value -> CounterList (Id a) value -> CounterList (Id a) value
addInto value ( CounterList idValue list ) =
    pack ( idValue + 1 ) ( ( Id idValue, value ) :: list )


{-| Add a list of values to the List and assign them [`Id`](Id#Id)s -}
addList : List value -> CounterList (Id a) value -> CounterList (Id a) value
addList vals list =
    List.foldr addInto list vals


{-|
Add a value to the List, assign it a new [`Id`](Id#Id) and insert it at a given index.
Indexes lower than 0 will be inserted at index 0.
Indexes higher than the length of the list will inserted at the end of the list.
-}
addAt : Int -> value -> CounterList (Id a) value -> ( Id a, CounterList (Id a) value )
addAt index value ( CounterList nextId list ) =
    let id = Id nextId in
    ( id, pack ( nextId + 1 ) ( List.take index list ++ ( id, value ) :: List.drop index list ) )


{-|
Insert a Id-value pair into a list.
If there is a existing pair with the same Id if will be replaced.
If there isn't a existing pair the pair will be inserted at the start of the list.
-}
insert : Id a -> value -> CounterList (Id a) value -> CounterList (Id a) value
insert id value ( CounterList counter list ) =
    pack ( accountForId id counter ) ( Id.List.insert id value list )


{-|
Insert a Id-value pair into a given index into a Id.List.
This will remove any existing value with the same Id.
Indexes lower than 0 will be inserted at index 0.
Indexes higher than the length of the list will inserted at the end of the list.
-}
insertAt : Int -> Id a -> value -> CounterList (Id a) value -> CounterList (Id a) value
insertAt index id value ( CounterList counter list )=
    pack ( accountForId id counter ) ( Id.List.insertAt index id value list )


{-| Move a existing value to a new index. Does nothing if the item is not present. -}
move : Id a -> Int -> CounterList (Id a) value -> CounterList (Id a) value
move id index ( CounterList counter list ) =
    pack counter ( Id.List.move id index list )


{-| Determine if a list is empty. -}
isEmpty : CounterList (Id a) value -> Bool
isEmpty list =
    Id.List.isEmpty ( toList list )


{-| Determine the number of Id-value pairs in the list. -}
length : CounterList (Id a) value -> Int
length list =
    Id.List.length ( toList list )


{-| Determine if a Id is in a list. -}
member : Id a -> CounterList (Id a) value -> Bool
member id list =
    Id.List.member id ( toList list )


{-| Get the value associated with a Id. If the Id is not found, return Nothing. -}
get : Id a -> CounterList (Id a) value -> Maybe value
get id list =
    Id.List.get id ( toList list )


{-| Same as [get](Id.List#get) but with the arguments swapped, useful for pipelines. -}
getFrom : CounterList (Id a) value -> Id a -> Maybe value
getFrom list id =
    get id list


{-| Remove a Id-value pair from a list. If the Id is not found, no changes are made. -}
remove : Id a -> CounterList (Id a) value -> CounterList (Id a) value
remove id list =
    packInto list ( Id.List.remove id ( toList list ) )


{-| Update the value of a list for a specific Id with a given function. If the Id is not found, no changes are made. -}
update : Id a -> (value -> value) -> CounterList (Id a) value -> CounterList (Id a) value
update id func list =
    packInto list ( Id.List.update id func ( toList list ) )


{-| Apply a function to all values in a list. -}
map : (Id a -> b -> c) -> CounterList (Id a) b -> CounterList (Id a) c
map func list =
    packInto list ( Id.List.map func ( toList list ) )


{-| Keep only the Id-value pairs that pass the given test. -}
filter : (Id a -> value -> Bool) -> CounterList (Id a) value -> CounterList (Id a) value
filter func list =
    packInto list ( Id.List.filter func ( toList list ) )


{-| Apply a function to all values in the list and filter out the values that return Nothing. -}
filterMap : (Id a -> b -> Maybe c) -> CounterList (Id a) b -> CounterList (Id a) c
filterMap func list =
    packInto list ( Id.List.filterMap func ( toList list ) )


{-| Fold over the Id-value pairs in a list from right to left. -}
foldr : (Id a -> value -> c -> c) -> c -> CounterList (Id a) value -> c
foldr func start list =
    Id.List.foldr func start ( toList list )


{-| Fold over the Id-value pairs in a list from left to right. -}
foldl : (Id a -> value -> c -> c) -> c -> CounterList (Id a) value -> c
foldl func start list =
    Id.List.foldl func start ( toList list )


{-| Get all of the Ids in a list, sorted from lowest to highest. -}
ids : CounterList (Id a) value -> List (Id a)
ids list =
    Id.List.ids ( toList list )


{-| Get all of the values in a list, in the order of their Ids. -}
values : CounterList (Id a) value -> List value
values list =
    Id.List.values ( toList list )


{-|
Partition a list according to some test.
The first list contains all Id-value pairs which passed the test, and the second contains the pairs that did not.
The first list retains the Id counter while the second is turned into a [`Id.List`](Id.List)
-}
partition : (id -> value -> Bool) -> CounterList id value -> ( CounterList id value, Id.List.List id value )
partition predicate list =
    Id.List.partition predicate ( toList list )
    |> Tuple.mapFirst ( packInto list )


{-| Keep a Id-value pair when its Id does not appear in the second list. -}
diff : CounterList id b -> Id.List.List id c -> CounterList id b
diff list1 list2 =
    Id.List.diff ( toList list1 ) list2
    |> packInto list1


{-| Keep a Id-value pair when its Id appears in the second list. Preference is given to values in the first list. -}
intersect : CounterList id b -> Id.List.List id c -> CounterList id b
intersect list1 list2 =
    Id.List.intersect ( toList list1 ) list2
    |> packInto list1


{-| Combine two Lists. If there is a collision, preference is given to the first List. -}
append : Id.List.List (Id a) value -> CounterList (Id a) value -> CounterList (Id a) value
append list1 list2 =
    let
        counter = List.foldl ( Tuple.first >> Internal.accountForId ) ( getCounter_ list2 ) list1
    in
    toList list2
    |> Id.List.append list1
    |> pack counter


{-| Decompose a list of tuples into a tuple of lists. -}
unzip : CounterList id ( a, b ) -> ( CounterList id a, Id.List.List id b )
unzip list =
    Id.List.unzip ( toList list )
    |> Tuple.mapFirst ( packInto list )


{-| Encode `CounterList` to a JSON value -}
encode : ( value -> D.Value ) -> CounterList ( Id id ) value -> E.Value
encode encodeValue list =
    toList list
    |> List.map ( \( id, value ) -> ( "[" ++ Internal.idToString id ++ "]", encodeValue value ) )
    |> (::) ( "counter", Id.Counter.encode ( IdCounter ( getCounter_ list ) ) )
    |> E.object


keyParser : P.Parser (Id a)
keyParser =
    P.oneOf
    [ P.succeed identity
        |. P.symbol "["
        |= Internal.idParser
        |. P.symbol "]"
    , Internal.idParser
    ]


{-| JSON Decoder for `SetCounterList -}
decoder : D.Decoder value -> D.Decoder ( CounterList ( Id id ) value )
decoder valueDecoder =
    let
        addDecoder ( key, () ) baseDecoder =
            if key == "counter" then
                baseDecoder
            else
                case P.run keyParser key of
                    Ok id ->
                        D.map2 ( Id.List.insert id ) ( D.field key valueDecoder ) baseDecoder
                    
                    Err _ ->
                        D.fail ( "Invalid key: " ++ key )
        fromKeys = List.foldr addDecoder ( D.succeed [] )
    in
    D.keyValuePairs ( D.succeed () )
    |> D.andThen fromKeys
    |> D.map2 fromIdList ( D.field "counter" Id.Counter.decoder )

