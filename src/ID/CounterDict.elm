module ID.CounterDict exposing
    ( CounterDict
    , empty
    , singleton
    , fromList
    , toList
    , isEmpty
    , size
    , member
    , get
    , getFrom
    , add
    , insert
    , remove
    , update
    , map
    , map2
    , map3
    , map4
    , filter
    , filterMap
    , fold
    , ids
    , values
    , partition
    , diff
    , intersect
    , union
    , encode
    , decoder
    )

{-|
@docs CounterDict

Most of the functions in this module are a alias for the same function in [`ID.Dict`](ID.Dict), which can handle both 'ID.Dict' and 'ID.CounterDict'

@docs empty, singleton, fromList, toList
@docs isEmpty, size
@docs member, get, getFrom
@docs add, insert, remove, update
@docs map, filter, filterMap, fold
@docs ids, values
@docs partition, diff, intersect, union
@docs map2, map3, map4

# Json
@docs encode, decoder
-}

import Internal exposing (ID(..), Dict(..), Counter(..), unpackDict)
import Dict

import Json.Encode as E
import Json.Decode as D

import ID.Dict
import ID.Counter as Counter


{-| A dictionary with counter -}
type alias CounterDict id value = Dict Never id value


getCounter : CounterDict ( ID id ) value -> Counter.Counter ( ID id )
getCounter dict =
    case dict of
        WithCounter counter _ ->
            counter

        WithoutCounter _ _ ->
            Counter.accountForDictIDs dict ( IDCounter 0 )


{-| Create a empty `CounterDict` -}
empty : Counter ( ID a ) -> CounterDict (ID a) value
empty counter =
    WithCounter counter Dict.empty


{-| Create a 'CounterDict' with a single ID-value pair -}
singleton : Counter (ID a) -> value -> ( ID a, CounterDict (ID a) value )
singleton counter value =
    add value ( empty counter )


{-| Create a new `CounterDict` from a list of values and assign an [`ID`](ID#ID) to every value -}
fromList : Counter ( ID a ) -> List value -> CounterDict (ID a) value
fromList ( IDCounter counterValue ) list =
    WithCounter
    ( IDCounter ( counterValue + List.length list ) )
    ( Dict.fromList ( List.indexedMap ( \index value -> ( counterValue + index, value ) ) list ) )


{-| Convert a dictionary into an list of ID-value pairs, sorted by IDs. -}
toList : CounterDict (ID a) value -> List ( ID a, value )
toList =
    ID.Dict.toList


{-| Add a value to the Dictionary and assign it a new [`ID`](ID#ID). -}
add : value -> CounterDict (ID a) value -> ( ID a, CounterDict (ID a) value )
add value dict =
    let
        ( ID idValue as id, newCounter ) = Counter.newID ( getCounter dict )
    in
    ( id, WithCounter newCounter ( Dict.insert idValue value ( unpackDict dict ) ) )


{-| Insert a ID-value pair into a dictionary. Replaces value when there is a collision. -}
insert : ID a -> value -> CounterDict (ID a) value -> CounterDict (ID a) value
insert =
    ID.Dict.insert


{-| Determine if a dictionary is empty. -}
isEmpty : CounterDict (ID a) value -> Bool
isEmpty =
    ID.Dict.isEmpty


{-| Determine the number of ID-value pairs in the dictionary. -}
size : CounterDict (ID a) value -> Int
size =
    ID.Dict.size


{-| Determine if a ID is in a dictionary. -}
member : ID a -> CounterDict (ID a) value -> Bool
member =
    ID.Dict.member


{-| Get the value associated with a ID. If the ID is not found, return Nothing. -}
get : ID a -> CounterDict (ID a) value -> Maybe value
get =
    ID.Dict.get


{-| Same as [get](ID.Dict#get) but with the arguments swapped, useful for pipelines. -}
getFrom : CounterDict (ID a) value -> ID a -> Maybe value
getFrom =
    ID.Dict.getFrom


{-| Remove a ID-value pair from a dictionary. If the ID is not found, no changes are made. -}
remove : ID a -> CounterDict (ID a) value -> CounterDict (ID a) value
remove =
    ID.Dict.remove


{-| Update the value of a dictionary for a specific ID with a given function. If the ID is not found, no changes are made. -}
update : ID a -> (value -> value) -> CounterDict (ID a) value -> CounterDict (ID a) value
update =
    ID.Dict.update


{-| Apply a function to all values in a dictionary. -}
map : (ID a -> b -> c) -> CounterDict (ID a) b -> CounterDict (ID a) c
map =
    ID.Dict.map


setCounter : Counter id -> Dict a id value -> CounterDict id value
setCounter counter dict =
    WithCounter counter ( unpackDict dict )


{-| Apply a function to values that appear in both dictionaries and create a new dictionary with the results, values that don't appear in all dictionaries are skipped. -}
map2 : ( ID a -> b -> c -> d ) -> CounterDict (ID a) b -> Dict c2 (ID a) c -> CounterDict (ID a) d
map2 func dict1 dict2 =
    ID.Dict.map2 func dict1 dict2
    |> setCounter ( getCounter dict1 )


{-| Same as [`ID.Dict.map2`](ID.Dict#map2) but for 3 dictionaries -}
map3 : ( ID a -> b -> c -> d -> e ) -> CounterDict (ID a) b -> Dict c2 (ID a) c -> Dict c3 (ID a) d -> CounterDict (ID a) e
map3 func dict1 dict2 dict3 =
    ID.Dict.map3 func dict1 dict2 dict3
    |> setCounter ( getCounter dict1 )


{-| Same as [`ID.Dict.map2`](ID.Dict#map2) but for 4 dictionaries -}
map4 : ( ID a -> b -> c -> d -> e -> f ) -> CounterDict (ID a) b -> Dict c2 (ID a) c -> Dict c3 (ID a) d -> Dict c4 (ID a) e -> CounterDict (ID a) f
map4 func dict1 dict2 dict3 dict4 =
    ID.Dict.map4 func dict1 dict2 dict3 dict4
    |> setCounter ( getCounter dict1 )


{-| Keep only the ID-value pairs that pass the given test. -}
filter : (ID a -> value -> Bool) -> CounterDict (ID a) value -> CounterDict (ID a) value
filter =
    ID.Dict.filter


{-| Apply a function to all values in the dictionary and filter out the values that return Nothing. -}
filterMap : (ID a -> b -> Maybe c) -> CounterDict (ID a) b -> CounterDict (ID a) c
filterMap =
    ID.Dict.filterMap


{-| Fold over the ID-value pairs in a dictionary from lowest ID to highest ID. -}
fold : (ID a -> value -> c -> c) -> c -> CounterDict (ID a) value -> c
fold =
    ID.Dict.fold


{-| Get all of the IDs in a dictionary, sorted from lowest to highest. -}
ids : CounterDict (ID a) value -> List (ID a)
ids =
    ID.Dict.ids


{-| Get all of the values in a dictionary, in the order of their IDs. -}
values : CounterDict (ID a) value -> List value
values =
    ID.Dict.values


{-|
Partition a dictionary according to some test.
The first dictionary contains all ID-value pairs which passed the test, and the second contains the pairs that did not.
The first dictionary retains the ID counter while the second is turned into a [`ID.Dict`](ID.Dict)
-}
partition : (ID a -> value -> Bool) -> CounterDict (ID a) value -> ( CounterDict (ID a) value, ID.Dict.Dict (ID a) value )
partition test dict =
    Dict.partition (\idValue value -> test ( ID idValue ) value ) (unpackDict dict)
    |> Tuple.mapBoth ( WithCounter ( getCounter dict ) ) ( WithoutCounter () )


{-| Keep a ID-value pair when its ID does not appear in the second dictionary. -}
diff : CounterDict (ID a) b -> Internal.Dict counter(ID a) c -> CounterDict (ID a) b
diff =
    ID.Dict.diff


{-| Keep a ID-value pair when its ID appears in the second dictionary. Preference is given to values in the first dictionary. -}
intersect : CounterDict (ID a) b -> Internal.Dict counter(ID a) c -> CounterDict (ID a) b
intersect =
    ID.Dict.intersect


{-| Combine two dictionaries. If there is a collision, preference is given to the first dictionary. -}
union : Internal.Dict counter(ID a) value -> CounterDict (ID a) value -> CounterDict (ID a) value
union dict1 dict2 =
    ID.Dict.fold insert dict2 dict1


{-| Encode `CounterDict` to a JSON value -}
encode : ( value -> D.Value ) -> CounterDict ( ID id ) value -> E.Value
encode encodeValue dict =
    E.object
    [ ( "counter", Counter.encode ( getCounter dict ) )
    , ( "values", ID.Dict.encode encodeValue ( WithoutCounter () ( unpackDict dict ) ) )
    ]


{-| JSON Decoder for `SetCounterDict -}
decoder : D.Decoder value -> D.Decoder ( CounterDict ( ID id ) value )
decoder valueDecoder =
    D.map2 ( \counter dict -> WithCounter ( Counter.accountForDictIDs dict counter ) ( Internal.unpackDict dict ) )
    ( D.field "counter" Counter.decoder )
    ( D.field "values" ( ID.Dict.decoder valueDecoder ) )

