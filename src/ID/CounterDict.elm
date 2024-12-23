module ID.CounterDict exposing
    ( CounterDict
    , empty
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
    , diff
    , intersect
    , union
    , encode
    , decoder
    )

{-|
@docs CounterDict

@docs empty, fromList, toList
@docs isEmpty, size
@docs member, get, getFrom
@docs add, insert, remove, update
@docs map, filter, filterMap, fold
@docs ids, values
@docs diff, intersect, union
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


{-| Create a new `CounterDict` from a list of values and assign an [`ID`](ID#ID) to every value -}
fromList : a -> List value -> CounterDict (ID a) value
fromList _ list =
    WithCounter ( IDCounter ( List.length list ) ) ( Dict.fromList ( List.indexedMap Tuple.pair list ) )


{-| Alias for [`ID.Dict.toList`](ID.Dict#toList) -}
toList : CounterDict (ID a) value -> List ( ID a, value )
toList =
    ID.Dict.toList


{-| Add a value to the Dictionary and assign it a new [`ID`](ID#ID) -}
add : value -> CounterDict (ID a) value -> ( ID a, CounterDict (ID a) value )
add value dict =
    let
        ( ID idValue as id, newCounter ) = Counter.newID ( getCounter dict )
    in
    ( id, WithCounter newCounter ( Dict.insert idValue value ( unpackDict dict ) ) )


{-| Alias for [`ID.Dict.insert`](ID.Dict#insert) -}
insert : ID a -> value -> CounterDict (ID a) value -> CounterDict (ID a) value
insert =
    ID.Dict.insert


{-| Alias for [`ID.Dict.isEmpty`](ID.Dict#isEmpty) -}
isEmpty : CounterDict (ID a) value -> Bool
isEmpty =
    ID.Dict.isEmpty


{-| Alias for [`ID.Dict.size`](ID.Dict#size) -}
size : CounterDict (ID a) value -> Int
size =
    ID.Dict.size


{-| Alias for [`ID.Dict.member`](ID.Dict#member) -}
member : ID a -> CounterDict (ID a) value -> Bool
member =
    ID.Dict.member


{-| Alias for [`ID.Dict.get`](ID.Dict#get) -}
get : ID a -> CounterDict (ID a) value -> Maybe value
get =
    ID.Dict.get


{-| Alias for [`ID.Dict.getFrom`](ID.Dict#getFrom) -}
getFrom : CounterDict (ID a) value -> ID a -> Maybe value
getFrom =
    ID.Dict.getFrom


{-| Alias for [`ID.Dict.remove`](ID.Dict#remove) -}
remove : ID a -> CounterDict (ID a) value -> CounterDict (ID a) value
remove =
    ID.Dict.remove


{-| Alias for [`ID.Dict.update`](ID.Dict#update) -}
update : ID a -> (value -> value) -> CounterDict (ID a) value -> CounterDict (ID a) value
update =
    ID.Dict.update


{-| Alias for [`ID.Dict.map`](ID.Dict#map) -}
map : (ID a -> b -> c) -> CounterDict (ID a) b -> CounterDict (ID a) c
map =
    ID.Dict.map


{-| Alias for [`ID.Dict.map`](ID.Dict#map2) -}
map2 : ( ID a -> b -> c -> d ) -> CounterDict (ID a) b -> Dict c2 (ID a) c -> CounterDict (ID a) d
map2 func dict1 dict2 =
    ID.Dict.map2 func dict1 dict2
    |> unpackDict
    |> WithCounter ( getCounter dict1 )


{-| Alias for [`ID.Dict.map`](ID.Dict#map3) -}
map3 : ( ID a -> b -> c -> d -> e ) -> CounterDict (ID a) b -> Dict c2 (ID a) c -> Dict c3 (ID a) d -> CounterDict (ID a) e
map3 func dict1 dict2 dict3 =
    ID.Dict.map3 func dict1 dict2 dict3
    |> unpackDict
    |> WithCounter ( getCounter dict1 )


{-| Alias for [`ID.Dict.map`](ID.Dict#map4) -}
map4 : ( ID a -> b -> c -> d -> e -> f ) -> CounterDict (ID a) b -> Dict c2 (ID a) c -> Dict c3 (ID a) d -> Dict c4 (ID a) e -> CounterDict (ID a) f
map4 func dict1 dict2 dict3 dict4 =
    ID.Dict.map4 func dict1 dict2 dict3 dict4
    |> unpackDict
    |> WithCounter ( getCounter dict1 )


{-| Alias for [`ID.Dict.filter`](ID.Dict#filter) -}
filter : (ID a -> value -> Bool) -> CounterDict (ID a) value -> CounterDict (ID a) value
filter =
    ID.Dict.filter


{-| Alias for [`ID.Dict.filterMap`](ID.Dict#filterMap) -}
filterMap : (ID a -> b -> Maybe c) -> CounterDict (ID a) b -> CounterDict (ID a) c
filterMap =
    ID.Dict.filterMap


{-| Alias for [`ID.Dict.fold`](ID.Dict#fold) -}
fold : (ID a -> value -> c -> c) -> c -> CounterDict (ID a) value -> c
fold =
    ID.Dict.fold


{-| Alias for [`ID.Dict.ids`](ID.Dict#ids) -}
ids : CounterDict (ID a) value -> List (ID a)
ids =
    ID.Dict.ids


{-| Alias for [`ID.Dict.values`](ID.Dict#values) -}
values : CounterDict (ID a) value -> List value
values =
    ID.Dict.values


{-| Alias for [`ID.Dict.diff`](ID.Dict#diff) -}
diff : CounterDict (ID a) b -> Internal.Dict counter(ID a) c -> CounterDict (ID a) b
diff =
    ID.Dict.diff


{-| Alias for [`ID.Dict.intersect`](ID.Dict#intersect) -}
intersect : CounterDict (ID a) b -> Internal.Dict counter(ID a) c -> CounterDict (ID a) b
intersect =
    ID.Dict.intersect


{-| Alias for [`ID.Dict.union`](ID.Dict#union) -}
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

