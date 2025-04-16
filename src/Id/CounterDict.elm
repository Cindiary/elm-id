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
    , addInto
    , addList
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
@docs add, addInto, addList, insert, remove, update
@docs map, filter, filterMap, fold
@docs ids, values
@docs partition, diff, intersect, union
@docs map2, map3, map4

# Json
@docs encode, decoder
-}

import Internal exposing (Id(..), Dict(..), Counter(..), unpack)
import Dict

import Json.Encode as E
import Json.Decode as D

import Id.Dict
import Id.Counter


{-| A dictionary with a id counter -}
type alias CounterDict id value = Dict Never id value


pack : Int -> Dict.Dict Int value -> CounterDict id value
pack =
    WithCounter


{-| Create a empty `CounterDict` -}
empty : Id.Counter.Counter ( Id a ) -> CounterDict (Id a) value
empty ( IdCounter counter ) =
    pack counter Dict.empty


{-| Create a 'CounterDict' with a single Id-value pair -}
singleton : Id.Counter.Counter (Id a) -> value -> ( Id a, CounterDict (Id a) value )
singleton counter value =
    add value ( empty counter )


{-| Create a new `CounterDict` from a list of values and assign an [`Id`](Id#Id) to every value -}
fromList : Counter ( Id a ) -> List value -> CounterDict (Id a) value
fromList ( IdCounter counterValue ) list =
    pack
    ( counterValue + List.length list )
    ( Dict.fromList ( List.indexedMap ( \index value -> ( counterValue + index, value ) ) list ) )


{-| Get the counter from a counter dict -}
getCounter : a -> CounterDict ( Id a ) value -> Counter ( Id a )
getCounter _ dict =
    IdCounter ( Internal.getCounter dict )


{-| Convert a dictionary into an list of Id-value pairs, sorted by Ids. -}
toList : CounterDict (Id a) value -> List ( Id a, value )
toList =
    Id.Dict.toList


{-| Covert a Id.CounterDict into a regular (`Id.Dict`)[Id.Dict#Dict] -}
toDict : CounterDict (Id a) value -> Id.Dict.Dict (Id a) value
toDict dict =
    WithoutCounter () ( unpack dict )


{-| Add a value to the Dictionary and assign it a new [`Id`](Id#Id). -}
add : value -> CounterDict (Id a) value -> ( Id a, CounterDict (Id a) value )
add value dict =
    let
        idValue = Internal.getCounter dict
        id = Id idValue
    in
    ( id, pack ( idValue + 1 ) ( Dict.insert idValue value ( unpack dict ) ) )


{-| Same as as [add](Id.Dict#add) but only returns the Dictionary -}
addInto : value -> CounterDict (Id a) value -> CounterDict (Id a) value
addInto value dict =
    let
        ( idValue ) = Internal.getCounter dict
    in
    pack ( idValue + 1 ) ( Dict.insert idValue value ( unpack dict ) )


{-| Add a list of values to the Dictionary and assign them [`Id`](Id#Id)s -}
addList : List value -> CounterDict (Id a) value -> CounterDict (Id a) value
addList list dict =
    List.foldl addInto dict list


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


{-| Apply a function to values that appear in both dictionaries and create a new dictionary with the results, values that don't appear in all dictionaries are skipped. -}
map2 : ( Id a -> b -> c -> d ) -> CounterDict (Id a) b -> Dict c2 (Id a) c -> CounterDict (Id a) d
map2 =
    Id.Dict.map2


{-| Same as [`Id.Dict.map2`](Id.Dict#map2) but for 3 dictionaries -}
map3 : ( Id a -> b -> c -> d -> e ) -> CounterDict (Id a) b -> Dict c2 (Id a) c -> Dict c3 (Id a) d -> CounterDict (Id a) e
map3 =
    Id.Dict.map3


{-| Same as [`Id.Dict.map2`](Id.Dict#map2) but for 4 dictionaries -}
map4 : ( Id a -> b -> c -> d -> e -> f ) -> CounterDict (Id a) b -> Dict c2 (Id a) c -> Dict c3 (Id a) d -> Dict c4 (Id a) e -> CounterDict (Id a) f
map4 =
    Id.Dict.map4


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
partition : (Id a -> value -> Bool) -> CounterDict (Id a) value -> ( CounterDict (Id a) value, Dict () (Id a) value )
partition =
    Id.Dict.partition


{-| Keep a Id-value pair when its Id does not appear in the second dictionary. -}
diff : CounterDict (Id a) b -> Internal.Dict o2 (Id a) c -> CounterDict (Id a) b
diff dict1 dict2 =
    Id.Dict.diff dict1 dict2


{-| Keep a Id-value pair when its Id appears in the second dictionary. Preference is given to values in the first dictionary. -}
intersect : CounterDict (Id a) b -> Internal.Dict o2 (Id a) c -> CounterDict (Id a) b
intersect =
    Id.Dict.intersect


{-| Combine two dictionaries. If there is a collision, preference is given to the first dictionary. -}
union : Internal.Dict counter (Id a) value -> CounterDict (Id a) value -> CounterDict (Id a) value
union dict1 dict2 =
    Id.Dict.fold insert dict2 dict1


{-| Encode `CounterDict` to a JSON value -}
encode : ( value -> D.Value ) -> CounterDict ( Id id ) value -> E.Value
encode encodeValue dict =
    unpack dict
    |> Dict.toList
    |> List.map ( Tuple.mapBoth String.fromInt encodeValue )
    |> (::) ( "counter", E.int ( Internal.getCounter dict ) )
    |> E.object


{-| JSON Decoder for `SetCounterDict -}
decoder : D.Decoder value -> D.Decoder ( CounterDict ( Id id ) value )
decoder valueDecoder =
    let
        addDecoder ( key, () ) baseDecoder =
            case String.toInt key of
                Just idInt ->
                    D.map2 ( Dict.insert idInt ) ( D.field key valueDecoder ) baseDecoder
                
                Nothing ->
                    if key == "counter" then
                        baseDecoder
                    else
                        D.fail ( "Invalid key: " ++ key )
        fromKeys pairs = List.foldl addDecoder ( D.succeed Dict.empty ) pairs
        pack_ counter dict =
            let
                maxId = Dict.foldl ( \id _ acc -> max id acc ) 0 dict
                sanitizedCounter =
                    if maxId >= counter then
                        maxId + 10
                    else
                        counter
            in
            pack sanitizedCounter dict
    in
    D.keyValuePairs ( D.succeed () )
    |> D.andThen fromKeys
    |> D.map2 pack_ ( D.field "counter" D.int )

