module Id.CounterDict exposing
    ( CounterDict
    , empty
    , singleton
    , fromList
    , getCounter
    , toList
    , toDict
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

Most of the functions in this module are a alias for the same function in [`Id.Dict`](Id.Dict), which can handle both 'Id.Dict' and 'Id.CounterDict'

@docs empty, singleton, fromList, getCounter, toList, toDict
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

import Internal exposing (Id(..), Dict(..), Counter(..), unpackDict)
import Dict

import Json.Encode as E
import Json.Decode as D

import Id.Dict
import Id.Counter as Counter


{-| A dictionary with counter -}
type alias CounterDict id value = Dict Never id value


getCounter_ : CounterDict ( Id id ) value -> Counter.Counter ( Id id )
getCounter_ dict =
    case dict of
        WithCounter counter _ ->
            counter

        WithoutCounter _ _ ->
            Counter.accountForDictIds dict ( IdCounter 0 )


{-| Create a empty `CounterDict` -}
empty : Counter ( Id a ) -> CounterDict (Id a) value
empty counter =
    WithCounter counter Dict.empty


{-| Create a 'CounterDict' with a single Id-value pair -}
singleton : Counter (Id a) -> value -> ( Id a, CounterDict (Id a) value )
singleton counter value =
    add value ( empty counter )


{-| Create a new `CounterDict` from a list of values and assign an [`Id`](Id#Id) to every value -}
fromList : Counter ( Id a ) -> List value -> CounterDict (Id a) value
fromList ( IdCounter counterValue ) list =
    WithCounter
    ( IdCounter ( counterValue + List.length list ) )
    ( Dict.fromList ( List.indexedMap ( \index value -> ( counterValue + index, value ) ) list ) )


{-| Get the counter from a counter dict -}
getCounter : a -> CounterDict ( Id a ) value -> Counter ( Id a )
getCounter _ =
    getCounter_


{-| Convert a dictionary into an list of Id-value pairs, sorted by Ids. -}
toList : CounterDict (Id a) value -> List ( Id a, value )
toList =
    Id.Dict.toList


{-| Convert a `CounterDict` into a regular [`Id.Dict`](Id.Dict) -}
toDict : CounterDict id value -> Id.Dict.Dict id value
toDict dict =
    WithoutCounter () ( unpackDict dict )


{-| Add a value to the Dictionary and assign it a new [`Id`](Id#Id). -}
add : value -> CounterDict (Id a) value -> ( Id a, CounterDict (Id a) value )
add value dict =
    let
        ( Id idValue as id, newCounter ) = Counter.newId ( getCounter_ dict )
    in
    ( id, WithCounter newCounter ( Dict.insert idValue value ( unpackDict dict ) ) )


{-| Insert a Id-value pair into a dictionary. Replaces value when there is a collision. -}
insert : Id a -> value -> CounterDict (Id a) value -> CounterDict (Id a) value
insert =
    Id.Dict.insert


{-| Determine if a dictionary is empty. -}
isEmpty : CounterDict (Id a) value -> Bool
isEmpty =
    Id.Dict.isEmpty


{-| Determine the number of Id-value pairs in the dictionary. -}
size : CounterDict (Id a) value -> Int
size =
    Id.Dict.size


{-| Determine if a Id is in a dictionary. -}
member : Id a -> CounterDict (Id a) value -> Bool
member =
    Id.Dict.member


{-| Get the value associated with a Id. If the Id is not found, return Nothing. -}
get : Id a -> CounterDict (Id a) value -> Maybe value
get =
    Id.Dict.get


{-| Same as [get](Id.Dict#get) but with the arguments swapped, useful for pipelines. -}
getFrom : CounterDict (Id a) value -> Id a -> Maybe value
getFrom =
    Id.Dict.getFrom


{-| Remove a Id-value pair from a dictionary. If the Id is not found, no changes are made. -}
remove : Id a -> CounterDict (Id a) value -> CounterDict (Id a) value
remove =
    Id.Dict.remove


{-| Update the value of a dictionary for a specific Id with a given function. If the Id is not found, no changes are made. -}
update : Id a -> (value -> value) -> CounterDict (Id a) value -> CounterDict (Id a) value
update =
    Id.Dict.update


{-| Apply a function to all values in a dictionary. -}
map : (Id a -> b -> c) -> CounterDict (Id a) b -> CounterDict (Id a) c
map =
    Id.Dict.map


setCounter : Counter id -> Dict a id value -> CounterDict id value
setCounter counter dict =
    WithCounter counter ( unpackDict dict )


{-| Apply a function to values that appear in both dictionaries and create a new dictionary with the results, values that don't appear in all dictionaries are skipped. -}
map2 : ( Id a -> b -> c -> d ) -> CounterDict (Id a) b -> Dict c2 (Id a) c -> CounterDict (Id a) d
map2 func dict1 dict2 =
    Id.Dict.map2 func dict1 dict2
    |> setCounter ( getCounter_ dict1 )


{-| Same as [`Id.Dict.map2`](Id.Dict#map2) but for 3 dictionaries -}
map3 : ( Id a -> b -> c -> d -> e ) -> CounterDict (Id a) b -> Dict c2 (Id a) c -> Dict c3 (Id a) d -> CounterDict (Id a) e
map3 func dict1 dict2 dict3 =
    Id.Dict.map3 func dict1 dict2 dict3
    |> setCounter ( getCounter_ dict1 )


{-| Same as [`Id.Dict.map2`](Id.Dict#map2) but for 4 dictionaries -}
map4 : ( Id a -> b -> c -> d -> e -> f ) -> CounterDict (Id a) b -> Dict c2 (Id a) c -> Dict c3 (Id a) d -> Dict c4 (Id a) e -> CounterDict (Id a) f
map4 func dict1 dict2 dict3 dict4 =
    Id.Dict.map4 func dict1 dict2 dict3 dict4
    |> setCounter ( getCounter_ dict1 )


{-| Keep only the Id-value pairs that pass the given test. -}
filter : (Id a -> value -> Bool) -> CounterDict (Id a) value -> CounterDict (Id a) value
filter =
    Id.Dict.filter


{-| Apply a function to all values in the dictionary and filter out the values that return Nothing. -}
filterMap : (Id a -> b -> Maybe c) -> CounterDict (Id a) b -> CounterDict (Id a) c
filterMap =
    Id.Dict.filterMap


{-| Fold over the Id-value pairs in a dictionary from lowest Id to highest Id. -}
fold : (Id a -> value -> c -> c) -> c -> CounterDict (Id a) value -> c
fold =
    Id.Dict.fold


{-| Get all of the Ids in a dictionary, sorted from lowest to highest. -}
ids : CounterDict (Id a) value -> List (Id a)
ids =
    Id.Dict.ids


{-| Get all of the values in a dictionary, in the order of their Ids. -}
values : CounterDict (Id a) value -> List value
values =
    Id.Dict.values


{-|
Partition a dictionary according to some test.
The first dictionary contains all Id-value pairs which passed the test, and the second contains the pairs that did not.
The first dictionary retains the Id counter while the second is turned into a [`Id.Dict`](Id.Dict)
-}
partition : (Id a -> value -> Bool) -> CounterDict (Id a) value -> ( CounterDict (Id a) value, Id.Dict.Dict (Id a) value )
partition test dict =
    Dict.partition (\idValue value -> test ( Id idValue ) value ) (unpackDict dict)
    |> Tuple.mapBoth ( WithCounter ( getCounter_ dict ) ) ( WithoutCounter () )


{-| Keep a Id-value pair when its Id does not appear in the second dictionary. -}
diff : CounterDict (Id a) b -> Internal.Dict counter(Id a) c -> CounterDict (Id a) b
diff =
    Id.Dict.diff


{-| Keep a Id-value pair when its Id appears in the second dictionary. Preference is given to values in the first dictionary. -}
intersect : CounterDict (Id a) b -> Internal.Dict counter(Id a) c -> CounterDict (Id a) b
intersect =
    Id.Dict.intersect


{-| Combine two dictionaries. If there is a collision, preference is given to the first dictionary. -}
union : Internal.Dict counter(Id a) value -> CounterDict (Id a) value -> CounterDict (Id a) value
union dict1 dict2 =
    Id.Dict.fold insert dict2 dict1


{-| Encode `CounterDict` to a JSON value -}
encode : ( value -> D.Value ) -> CounterDict ( Id id ) value -> E.Value
encode encodeValue dict =
    E.object
    [ ( "counter", Counter.encode ( getCounter_ dict ) )
    , ( "values", Id.Dict.encode encodeValue ( WithoutCounter () ( unpackDict dict ) ) )
    ]


{-| JSON Decoder for `SetCounterDict -}
decoder : D.Decoder value -> D.Decoder ( CounterDict ( Id id ) value )
decoder valueDecoder =
    D.map2 ( \counter dict -> WithCounter ( Counter.accountForDictIds dict counter ) ( Internal.unpackDict dict ) )
    ( D.field "counter" Counter.decoder )
    ( D.field "values" ( Id.Dict.decoder valueDecoder ) )

