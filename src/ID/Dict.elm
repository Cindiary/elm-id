module ID.Dict exposing
    ( Dict
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
@docs Dict

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

import Internal exposing (ID(..), Dict(..), unpack, unpackDict, packDict, mapDict)
import Dict

import Json.Encode as E
import Json.Decode as D
import Internal


{-| A [`Dict`](https://package.elm-lang.org/packages/elm/core/latest/Dict#Dict) for [`ID`](ID#ID)-value pairs -}
type alias Dict id value = Internal.Dict () id value


{-| Create a empty [`Dict`](ID.Dict#Dict) -}
empty : Dict (ID a) value
empty =
    pack Dict.empty


{-| Create a dictionary with a single ID-value pair -}
singleton : ID a -> value -> Dict (ID a) value
singleton id value =
    insert id value empty


{-| Convert an list of ID-value pairs into a dictionary. -}
fromList : List ( ID a, value ) -> Dict (ID a) value
fromList list =
    pack ( Dict.fromList ( List.map ( Tuple.mapFirst unpack ) list ) )


pack : Dict.Dict Int value -> Dict id value
pack =
    WithoutCounter ()


{-| Determine if a dictionary is empty. -}
isEmpty : Internal.Dict counter (ID a) value -> Bool
isEmpty =
    Dict.isEmpty << unpackDict


{-| Determine the number of ID-value pairs in the dictionary. -}
size : Internal.Dict counter (ID a) value -> Int
size =
    Dict.size << unpackDict


{-| Determine if a ID is in a dictionary. -}
member : ID a -> Internal.Dict counter (ID a) value -> Bool
member id dict =
    Dict.member (unpack id) (unpackDict dict)


{-| Get the value associated with a ID. If the ID is not found, return Nothing. -}
get : ID a -> Internal.Dict counter (ID a) value -> Maybe value
get id dict =
    Dict.get (unpack id) (unpackDict dict)


{-| Same as [get](ID.Dict#get) but with the arguments swapped, useful for pipelines. -}
getFrom : Internal.Dict counter (ID a) value -> ID a -> Maybe value
getFrom dict id =
    get id dict


{-| Insert a ID-value pair into a dictionary. Replaces value when there is a collision. -}
insert : ID a -> value -> Internal.Dict counter (ID a) value -> Internal.Dict counter (ID a) value
insert id value dict =
    case dict of
        WithoutCounter noCounter inner ->
            WithoutCounter noCounter ( Dict.insert ( unpack id ) value inner )

        WithCounter counter inner ->
            WithCounter ( Internal.accountForID id counter ) ( Dict.insert ( unpack id ) value inner )



{-| Remove a ID-value pair from a dictionary. If the ID is not found, no changes are made. -}
remove : ID a -> Internal.Dict counter (ID a) value -> Internal.Dict counter (ID a) value
remove id dict =
    packDict dict (Dict.remove (unpack id) (unpackDict dict))


{-| Update the value of a dictionary for a specific ID with a given function. If the ID is not found, no changes are made. -}
update : ID a -> (value -> value) -> Internal.Dict counter (ID a) value -> Internal.Dict counter (ID a) value
update id func dict =
    case get id dict of
        Just value ->
            insert id (func value) dict

        Nothing ->
            dict


mapFunc : (ID a -> b -> c) -> (Int -> b -> c)
mapFunc func k v =
    func (ID k) v


{-| Apply a function to all values in a dictionary. -}
map : (ID a -> b -> c) -> Internal.Dict counter (ID a) b -> Internal.Dict counter (ID a) c
map func =
    mapDict (Dict.map (mapFunc func))


{-| Apply a function to values that appear in both dictionaries and create a new dictionary with the results, values that don't appear in all dictionaries are skipped. -}
map2 : (ID a -> b -> c -> d) -> Internal.Dict c1 (ID a) b -> Internal.Dict c2 (ID a) c -> Dict (ID a) d
map2 func dict1 dict2 =
    let
        dict2Internal = unpackDict dict2
        foldFunc idValue value1 resDict =
            case Dict.get idValue dict2Internal of
                Just value2 ->
                    Dict.insert idValue ( func ( ID idValue ) value1 value2 ) resDict
                
                Nothing ->
                    resDict
    in
    unpackDict dict1
    |> Dict.foldl foldFunc Dict.empty
    |> pack


{-| Same as [`ID.Dict.map2`](ID.Dict#map2) but for 3 dictionaries -}
map3 : (ID a -> b -> c -> d -> e) -> Internal.Dict c1 (ID a) b -> Internal.Dict c2 (ID a) c -> Internal.Dict c3 (ID a) d -> Dict (ID a) e
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


{-| Same as [`ID.Dict.map2`](ID.Dict#map2) but for 4 dictionaries -}
map4 : (ID a -> b -> c -> d -> e -> f) -> Internal.Dict c1 (ID a) b -> Internal.Dict c2 (ID a) c -> Internal.Dict c3 (ID a) d -> Internal.Dict c4 (ID a) e -> Dict (ID a) f
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


{-| Keep only the ID-value pairs that pass the given test. -}
filter : (ID a -> value -> Bool) -> Internal.Dict counter (ID a) value -> Internal.Dict counter (ID a) value
filter predicate =
    mapDict (Dict.filter (mapFunc predicate))


{-| Apply a function to all values in the dictionary and filter out the values that return Nothing. -}
filterMap : (ID a -> b -> Maybe c) -> Internal.Dict counter (ID a) b -> Internal.Dict counter (ID a) c
filterMap func dict =
    let
        foldFunc id val newDict =
            case func (ID id) val of
                Just newVal ->
                    Dict.insert id newVal newDict

                Nothing ->
                    newDict
    in
    Dict.foldl foldFunc Dict.empty (unpackDict dict)
    |> packDict dict


{-| Fold over the ID-value pairs in a dictionary from lowest ID to highest ID. -}
fold : (ID a -> value -> c -> c) -> c -> Internal.Dict counter (ID a) value -> c
fold func start dict =
    Dict.foldl (mapFunc func) start (unpackDict dict)


{-| Get all of the IDs in a dictionary, sorted from lowest to highest. -}
ids : Internal.Dict counter (ID a) value -> List (ID a)
ids dict =
    Dict.foldr (\idValue _ idList -> ID idValue :: idList) [] (unpackDict dict)


{-| Get all of the values in a dictionary, in the order of their IDs. -}
values : Internal.Dict counter (ID a) value -> List value
values dict =
    Dict.values (unpackDict dict)


{-| Convert a dictionary into an list of ID-value pairs, sorted by IDs. -}
toList : Internal.Dict counter (ID a) value -> List ( ID a, value )
toList dict =
    Dict.foldr (\idValue value list -> ( ID idValue, value ) :: list) [] (unpackDict dict)


{-| Partition a dictionary according to some test. The first dictionary contains all ID-value pairs which passed the test, and the second contains the pairs that did not. -}
partition : (ID a -> value -> Bool) -> Internal.Dict counter (ID a) value -> ( Dict (ID a) value, Dict (ID a) value )
partition test dict =
    Dict.partition (mapFunc test) (unpackDict dict)
    |> Tuple.mapBoth pack pack


{-| Keep a ID-value pair when its ID does not appear in the second dictionary. -}
diff : Internal.Dict counter1 (ID a) b -> Internal.Dict counter2 (ID a) c -> Internal.Dict counter1 (ID a) b
diff dict1 dict2 =
    Dict.diff (unpackDict dict1) (unpackDict dict2)
    |> packDict dict1


{-| Keep a ID-value pair when its ID appears in the second dictionary. Preference is given to values in the first dictionary. -}
intersect : Internal.Dict counter1 (ID a) b -> Internal.Dict counter2 (ID a) c -> Internal.Dict counter1 (ID a) b
intersect dict1 dict2 =
    filter (\k _ -> member k dict2) dict1


{-| Combine two dictionaries. If there is a collision, preference is given to the first dictionary. -}
union : Internal.Dict counter1 (ID a) value -> Internal.Dict counter2 (ID a) value -> Dict (ID a) value
union dict1 dict2 =
    pack (Dict.union (unpackDict dict1) (unpackDict dict2))


{-| Encode a [`Dict`](ID.Dict#Dict) to a JSON value -}
encode : ( value -> E.Value ) -> Dict id value -> E.Value
encode encodeValue dict =
    Dict.toList (unpackDict dict)
    |> List.map ( Tuple.mapBoth String.fromInt encodeValue )
    |> E.object


{-| JSON Decoder for [`Dict`](ID.Dict#Dict) -}
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
