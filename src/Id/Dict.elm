module Id.Dict exposing
    ( Dict
    , Dict_
    , empty
    , singleton
    , fromDict
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
    , accountForIds
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

@docs empty, singleton, fromDict, fromList, toList
@docs isEmpty, size
@docs member, get, getFrom
@docs insert, remove, update
@docs accountForIds
@docs map, filter, filterMap, fold
@docs ids, values
@docs partition
@docs diff, intersect, union
@docs map2, map3, map4

# Json
@docs encode, decoder
-}

import Internal exposing (Id(..), Dict(..), unpackId, unpack)
import Dict
import Id.Counter

import Json.Encode as E
import Json.Decode as D
import Internal


{-| A [`Dict`](https://package.elm-lang.org/packages/elm/core/latest/Dict#Dict) for [`Id`](Id#Id)-value pairs -}
type alias Dict id value = Dict_ () id value

{-| A type that can represent [`Id.Dict`](Id.Dict#Dict) and [`Id.CounterDict`](Id.CounterDict#CounterDict) -}
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
    |> List.foldl ( \( id, value ) dict -> Dict.insert ( unpackId id ) value dict ) Dict.empty
    |> pack


{-| Convert any `Id` dictionary into a regular [`Id.Dict`](Id.Dict) -}
fromDict : Dict_ counter id value -> Dict id value
fromDict dict =
    pack ( unpack dict )


pack : Dict.Dict Int value -> Dict id value
pack =
    WithoutCounter ()


{-| Determine if a dictionary is empty. -}
isEmpty : Dict_ counter (Id a) value -> Bool
isEmpty =
    Dict.isEmpty << unpack


{-| Determine the number of Id-value pairs in the dictionary. -}
size : Dict_ counter (Id a) value -> Int
size =
    Dict.size << unpack


{-| Determine if a Id is in a dictionary. -}
member : Id a -> Dict_ counter (Id a) value -> Bool
member id dict =
    Dict.member (unpackId id) (unpack dict)


{-| Get the value associated with a Id. If the Id is not found, return Nothing. -}
get : Id a -> Dict_ counter (Id a) value -> Maybe value
get id dict =
    Dict.get (unpackId id) (unpack dict)


{-| Same as [get](Id.Dict#get) but with the arguments swapped, useful for pipelines. -}
getFrom : Dict_ counter (Id a) value -> Id a -> Maybe value
getFrom dict id =
    get id dict


{-| Insert a Id-value pair into a dictionary. Replaces value when there is a collision. -}
insert : Id a -> value -> Dict_ counter (Id a) value -> Dict_ counter (Id a) value
insert id value dict =
    let
        idValue = unpackId id
    in
    case dict of
        WithoutCounter noCounter inner ->
            WithoutCounter noCounter ( Dict.insert idValue value inner )
        
        WithCounter counter inner ->
            WithCounter ( Internal.accountForId id counter ) ( Dict.insert idValue value inner )



{-| Remove a Id-value pair from a dictionary. If the Id is not found, no changes are made. -}
remove : Id a -> Dict_ counter (Id a) value -> Dict_ counter (Id a) value
remove id dict =
    Internal.pack dict ( Dict.remove ( unpackId id ) ( unpack dict ) )


{-| Same as [`Id.Counter.accountForIds`](Id.Counter#accountForIds) but can be used with a [`Dict`](Id.Dict#Dict) -}
accountForIds : Dict_ counter (Id a) value -> Id.Counter.Counter (Id a) -> Id.Counter.Counter (Id a)
accountForIds dict counter =
    if isEmpty dict then
        counter
    else
        let
            fold_ id _ res = max id res
            maxId = Dict.foldl fold_ -1 ( unpack dict )
        in
        Id.Counter.accountForId ( Id maxId ) counter


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
    Internal.map (Dict.map (mapFunc func))


{-| Apply a function to values that appear in both dictionaries and create a new dictionary with the results, values that don't appear in all dictionaries are skipped. -}
map2 : (Id a -> b -> c -> d) -> Dict_ c1 (Id a) b -> Dict_ c2 (Id a) c -> Dict_ c1 (Id a) d
map2 func dict1 dict2 =
    let
        dict2Internal = unpack dict2
        foldFunc idValue value1 resDict =
            case Dict.get idValue dict2Internal of
                Just value2 ->
                    Dict.insert idValue ( func ( Id idValue ) value1 value2 ) resDict
                
                Nothing ->
                    resDict
    in
    unpack dict1
    |> Dict.foldl foldFunc Dict.empty
    |> Internal.pack dict1


{-| Same as [`Id.Dict.map2`](Id.Dict#map2) but for 3 dictionaries -}
map3 : (Id a -> b -> c -> d -> e) -> Dict_ c1 (Id a) b -> Dict_ c2 (Id a) c -> Dict_ c3 (Id a) d -> Dict_ c1 (Id a) e
map3 func dict1 dict2 dict3 =
    let
        foldFunc id value1 resDict =
            case Maybe.map2 ( func id value1 ) ( get id dict2 ) ( get id dict3 ) of
                Just result ->
                    Dict.insert ( unpackId id ) result resDict
                
                Nothing ->
                    resDict
    in
    fold foldFunc Dict.empty dict1
    |> Internal.pack dict1


{-| Same as [`Id.Dict.map2`](Id.Dict#map2) but for 4 dictionaries -}
map4 : (Id a -> b -> c -> d -> e -> f) -> Dict_ c1 (Id a) b -> Dict_ c2 (Id a) c -> Dict_ c3 (Id a) d -> Dict_ c4 (Id a) e -> Dict_ c1 (Id a) f
map4 func dict1 dict2 dict3 dict4 =
    let
        foldFunc id value1 resDict =
            case Maybe.map3 ( func id value1 ) ( get id dict2 ) ( get id dict3 ) ( get id dict4 ) of
                Just result ->
                    Dict.insert ( unpackId id ) result resDict
                
                Nothing ->
                    resDict
    in
    fold foldFunc Dict.empty dict1
    |> Internal.pack dict1


{-| Keep only the Id-value pairs that pass the given test. -}
filter : (Id a -> value -> Bool) -> Dict_ counter (Id a) value -> Dict_ counter (Id a) value
filter predicate dict =
    Internal.pack dict (Dict.filter (mapFunc predicate) (unpack dict))


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
    unpack dict
    |> Dict.foldl foldFunc Dict.empty
    |> Internal.pack dict


{-| Fold over the Id-value pairs in a dictionary from lowest Id to highest Id. -}
fold : (Id a -> value -> c -> c) -> c -> Dict_ counter (Id a) value -> c
fold func start dict =
    Dict.foldl (mapFunc func) start (unpack dict)


{-| Get all of the Ids in a dictionary, sorted from lowest to highest. -}
ids : Dict_ counter (Id a) value -> List (Id a)
ids dict =
    Dict.foldr (\idValue _ idList -> Id idValue :: idList) [] (unpack dict)


{-| Get all of the values in a dictionary, in the order of their Ids. -}
values : Dict_ counter (Id a) value -> List value
values dict =
    Dict.values (unpack dict)


{-| Convert a dictionary into an list of Id-value pairs, sorted by Ids. -}
toList : Dict_ counter (Id a) value -> List ( Id a, value )
toList dict =
    Dict.foldr (\idValue value list -> ( Id idValue, value ) :: list) [] (unpack dict)


{-| Partition a dictionary according to some test. The first dictionary contains all Id-value pairs which passed the test, and the second contains the pairs that did not. -}
partition : (Id a -> value -> Bool) -> Dict_ counter (Id a) value -> ( Dict_ counter (Id a) value, Dict (Id a) value )
partition test dict =
    unpack dict
    |> Dict.partition ( mapFunc test )
    |> Tuple.mapBoth ( Internal.pack dict ) pack


{-| Keep a Id-value pair when its Id does not appear in the second dictionary. -}
diff : Dict_ counter1 (Id a) b -> Dict_ counter2 (Id a) c -> Dict_ counter1 (Id a) b
diff dict1 dict2 =
    Dict.diff (unpack dict1) (unpack dict2)
    |> Internal.pack dict1


{-| Keep a Id-value pair when its Id appears in the second dictionary. Preference is given to values in the first dictionary. -}
intersect : Dict_ counter1 (Id a) b -> Dict_ counter2 (Id a) c -> Dict_ counter1 (Id a) b
intersect dict1 dict2 =
    filter (\k _ -> member k dict2) dict1


{-| Combine two dictionaries. If there is a collision, preference is given to the first dictionary. -}
union : Dict_ counter1 (Id a) value -> Dict_ counter2 (Id a) value -> Dict (Id a) value
union dict1 dict2 =
    pack (Dict.union (unpack dict1) (unpack dict2))


{-| Encode a [`Dict`](Id.Dict#Dict) to a JSON value -}
encode : ( value -> E.Value ) -> Dict id value -> E.Value
encode encodeValue dict =
    E.dict String.fromInt encodeValue ( unpack dict )


{-| JSON Decoder for [`Dict`](Id.Dict#Dict) -}
decoder : D.Decoder value -> D.Decoder ( Dict id value )
decoder valueDecoder =
    let
        addPair ( key, value ) ( dict, invalidKeys ) =
            case String.toInt key of
                Just id ->
                    ( Dict.insert id value dict, invalidKeys )
                
                Nothing ->
                    ( dict, key :: invalidKeys )
        fromPairs pairs =
            case List.foldl addPair ( Dict.empty, [] ) pairs of
                ( dict, [] ) ->
                    D.succeed ( pack dict )
                
                ( _, keys ) ->
                    D.fail ( "Invalid keys: " ++ String.join ", " keys )
    in
    D.keyValuePairs valueDecoder
    |> D.andThen fromPairs
