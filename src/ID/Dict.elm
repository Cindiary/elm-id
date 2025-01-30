module Id.Dict exposing
    ( Dict
    , Dict_
    , empty
    , singleton
    , fromList
    , toList
    , isEmpty
    , size
    , member
    , get
    , getFrom
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
@docs Dict, Dict_

@docs empty, singleton, fromList, toList
@docs isEmpty, size
@docs member, get, getFrom
@docs insert, remove, update
@docs map, filter, filterMap, fold
@docs ids, values
@docs partition
@docs diff, intersect, union
@docs map2, map3, map4

# Json
@docs encode, decoder
-}

import Internal exposing (Id(..), Dict(..), unpack, unpackDict, packDict, mapDict)
import Dict

import Json.Encode as E
import Json.Decode as D
import Internal


{-| A [`Dict`](https://package.elm-lang.org/packages/elm/core/latest/Dict#Dict) for [`Id`](Id#Id)-value pairs -}
type alias Dict id value = Dict_ () id value

{-| A type that can represent both [`Id.Dict`](Id.Dict#Dict) and [`Id.CounterDict`](Id.CounterDict#CounterDict) -}
type alias Dict_ counter id value = Internal.Dict counter id value


{-| Create a empty [`Dict`](Id.Dict#Dict) -}
empty : Dict (Id a) value
empty =
    pack Dict.empty


{-| Create a dictionary with a single Id-value pair -}
singleton : Id a -> value -> Dict (Id a) value
singleton id value =
    insert id value empty


{-| Convert an list of Id-value pairs into a dictionary. -}
fromList : List ( Id a, value ) -> Dict (Id a) value
fromList list =
    list
    |> List.foldl ( \( id, value ) dict -> Dict.insert ( unpack id ) value dict ) Dict.empty
    |> pack


pack : Dict.Dict Int value -> Dict id value
pack =
    WithoutCounter ()


{-| Determine if a dictionary is empty. -}
isEmpty : Dict_ counter (Id a) value -> Bool
isEmpty =
    Dict.isEmpty << unpackDict


{-| Determine the number of Id-value pairs in the dictionary. -}
size : Dict_ counter (Id a) value -> Int
size =
    Dict.size << unpackDict


{-| Determine if a Id is in a dictionary. -}
member : Id a -> Dict_ counter (Id a) value -> Bool
member id dict =
    Dict.member (unpack id) (unpackDict dict)


{-| Get the value associated with a Id. If the Id is not found, return Nothing. -}
get : Id a -> Dict_ counter (Id a) value -> Maybe value
get id dict =
    Dict.get (unpack id) (unpackDict dict)


{-| Same as [get](Id.Dict#get) but with the arguments swapped, useful for pipelines. -}
getFrom : Dict_ counter (Id a) value -> Id a -> Maybe value
getFrom dict id =
    get id dict


{-| Insert a Id-value pair into a dictionary. Replaces value when there is a collision. -}
insert : Id a -> value -> Dict_ counter (Id a) value -> Dict_ counter (Id a) value
insert id value dict =
    case dict of
        WithoutCounter noCounter inner ->
            WithoutCounter noCounter ( Dict.insert ( unpack id ) value inner )

        WithCounter counter inner ->
            WithCounter ( Internal.accountForId id counter ) ( Dict.insert ( unpack id ) value inner )



{-| Remove a Id-value pair from a dictionary. If the Id is not found, no changes are made. -}
remove : Id a -> Dict_ counter (Id a) value -> Dict_ counter (Id a) value
remove id dict =
    packDict dict (Dict.remove (unpack id) (unpackDict dict))


{-| Update the value of a dictionary for a specific Id with a given function. If the Id is not found, no changes are made. -}
update : Id a -> (value -> value) -> Dict_ counter (Id a) value -> Dict_ counter (Id a) value
update id func dict =
    case get id dict of
        Just value ->
            insert id (func value) dict

        Nothing ->
            dict


mapFunc : (Id a -> b -> c) -> (Int -> b -> c)
mapFunc func k v =
    func (Id k) v


{-| Apply a function to all values in a dictionary. -}
map : (Id a -> b -> c) -> Dict_ counter (Id a) b -> Dict_ counter (Id a) c
map func =
    mapDict (Dict.map (mapFunc func))


{-| Apply a function to values that appear in both dictionaries and create a new dictionary with the results, values that don't appear in all dictionaries are skipped. -}
map2 : (Id a -> b -> c -> d) -> Dict_ c1 (Id a) b -> Dict_ c2 (Id a) c -> Dict (Id a) d
map2 func dict1 dict2 =
    let
        dict2Internal = unpackDict dict2
        foldFunc idValue value1 resDict =
            case Dict.get idValue dict2Internal of
                Just value2 ->
                    Dict.insert idValue ( func ( Id idValue ) value1 value2 ) resDict
                
                Nothing ->
                    resDict
    in
    unpackDict dict1
    |> Dict.foldl foldFunc Dict.empty
    |> pack


{-| Same as [`Id.Dict.map2`](Id.Dict#map2) but for 3 dictionaries -}
map3 : (Id a -> b -> c -> d -> e) -> Dict_ c1 (Id a) b -> Dict_ c2 (Id a) c -> Dict_ c3 (Id a) d -> Dict (Id a) e
map3 func dict1 dict2 dict3 =
    let
        foldFunc id value1 resDict =
            case Maybe.map2 ( func id value1 ) ( get id dict2 ) ( get id dict3 ) of
                Just result ->
                    insert id result resDict
                
                Nothing ->
                    resDict
    in
    fold foldFunc empty dict1


{-| Same as [`Id.Dict.map2`](Id.Dict#map2) but for 4 dictionaries -}
map4 : (Id a -> b -> c -> d -> e -> f) -> Dict_ c1 (Id a) b -> Dict_ c2 (Id a) c -> Dict_ c3 (Id a) d -> Dict_ c4 (Id a) e -> Dict (Id a) f
map4 func dict1 dict2 dict3 dict4 =
    let
        foldFunc id value1 resDict =
            case Maybe.map3 ( func id value1 ) ( get id dict2 ) ( get id dict3 ) ( get id dict4 ) of
                Just result ->
                    insert id result resDict
                
                Nothing ->
                    resDict
    in
    fold foldFunc empty dict1


{-| Keep only the Id-value pairs that pass the given test. -}
filter : (Id a -> value -> Bool) -> Dict_ counter (Id a) value -> Dict_ counter (Id a) value
filter predicate =
    mapDict (Dict.filter (mapFunc predicate))


{-| Apply a function to all values in the dictionary and filter out the values that return Nothing. -}
filterMap : (Id a -> b -> Maybe c) -> Dict_ counter (Id a) b -> Dict_ counter (Id a) c
filterMap func dict =
    let
        foldFunc id val newDict =
            case func (Id id) val of
                Just newVal ->
                    Dict.insert id newVal newDict

                Nothing ->
                    newDict
    in
    Dict.foldl foldFunc Dict.empty (unpackDict dict)
    |> packDict dict


{-| Fold over the Id-value pairs in a dictionary from lowest Id to highest Id. -}
fold : (Id a -> value -> c -> c) -> c -> Dict_ counter (Id a) value -> c
fold func start dict =
    Dict.foldl (mapFunc func) start (unpackDict dict)


{-| Get all of the Ids in a dictionary, sorted from lowest to highest. -}
ids : Dict_ counter (Id a) value -> List (Id a)
ids dict =
    Dict.foldr (\idValue _ idList -> Id idValue :: idList) [] (unpackDict dict)


{-| Get all of the values in a dictionary, in the order of their Ids. -}
values : Dict_ counter (Id a) value -> List value
values dict =
    Dict.values (unpackDict dict)


{-| Convert a dictionary into an list of Id-value pairs, sorted by Ids. -}
toList : Dict_ counter (Id a) value -> List ( Id a, value )
toList dict =
    Dict.foldr (\idValue value list -> ( Id idValue, value ) :: list) [] (unpackDict dict)


{-| Partition a dictionary according to some test. The first dictionary contains all Id-value pairs which passed the test, and the second contains the pairs that did not. -}
partition : (Id a -> value -> Bool) -> Dict_ counter (Id a) value -> ( Dict (Id a) value, Dict (Id a) value )
partition test dict =
    Dict.partition (mapFunc test) (unpackDict dict)
    |> Tuple.mapBoth pack pack


{-| Keep a Id-value pair when its Id does not appear in the second dictionary. -}
diff : Dict_ counter1 (Id a) b -> Dict_ counter2 (Id a) c -> Dict_ counter1 (Id a) b
diff dict1 dict2 =
    Dict.diff (unpackDict dict1) (unpackDict dict2)
    |> packDict dict1


{-| Keep a Id-value pair when its Id appears in the second dictionary. Preference is given to values in the first dictionary. -}
intersect : Dict_ counter1 (Id a) b -> Dict_ counter2 (Id a) c -> Dict_ counter1 (Id a) b
intersect dict1 dict2 =
    filter (\k _ -> member k dict2) dict1


{-| Combine two dictionaries. If there is a collision, preference is given to the first dictionary. -}
union : Dict_ counter1 (Id a) value -> Dict_ counter2 (Id a) value -> Dict (Id a) value
union dict1 dict2 =
    pack (Dict.union (unpackDict dict1) (unpackDict dict2))


{-| Encode a [`Dict`](Id.Dict#Dict) to a JSON value -}
encode : ( value -> E.Value ) -> Dict id value -> E.Value
encode encodeValue dict =
    Dict.toList (unpackDict dict)
    |> List.map ( Tuple.mapBoth String.fromInt encodeValue )
    |> E.object


{-| JSON Decoder for [`Dict`](Id.Dict#Dict) -}
decoder : D.Decoder value -> D.Decoder ( Dict id value )
decoder valueDecoder =
    let
        fromPairs pairs =
            case pairs of
                ( key, value ) :: ls ->
                    case String.toInt key of
                        Just id ->
                            D.map
                            ( Dict.insert id value )
                            ( fromPairs ls )

                        Nothing ->
                            D.fail ( "Invalid id: \"" ++ key ++ "\"" )
                
                [] ->
                    D.succeed Dict.empty
    in
    D.keyValuePairs valueDecoder
    |> D.andThen fromPairs
    |> D.map pack
