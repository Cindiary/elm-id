module ID.Dict exposing
    ( Dict
    , empty
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

@docs empty, fromList, toList
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


{-| A [`Dict`](https://package.elm-lang.org/packages/elm/core/latest/Dict#Dict) for [`ID`](ID#ID), value pairs -}
type alias Dict id value = Internal.Dict () id value


{-| Create a empty [`Dict`](ID.Dict#Dict) -}
empty : Dict (ID a) value
empty =
    pack Dict.empty


pack : Dict.Dict Int value -> Dict id value
pack =
    WithoutCounter ()


{-| Same as [`Dict.isEmpty`](https://package.elm-lang.org/packages/elm/core/latest/Dict#isEmpty) -}
isEmpty : Internal.Dict counter (ID a) value -> Bool
isEmpty =
    Dict.isEmpty << unpackDict


{-| Same as [`Dict.size`](https://package.elm-lang.org/packages/elm/core/latest/Dict#size) -}
size : Internal.Dict counter (ID a) value -> Int
size =
    Dict.size << unpackDict


{-| Same as [`Dict.fromList`](https://package.elm-lang.org/packages/elm/core/latest/Dict#fromList) -}
fromList : List ( ID a, value ) -> Dict (ID a) value
fromList list =
    pack ( Dict.fromList ( List.map ( Tuple.mapFirst unpack ) list ) )


{-| Same as [`Dict.member`](https://package.elm-lang.org/packages/elm/core/latest/Dict#member) -}
member : ID a -> Internal.Dict counter (ID a) value -> Bool
member id dict =
    Dict.member (unpack id) (unpackDict dict)


{-| Same as [`Dict.get`](https://package.elm-lang.org/packages/elm/core/latest/Dict#get) -}
get : ID a -> Internal.Dict counter (ID a) value -> Maybe value
get id dict =
    Dict.get (unpack id) (unpackDict dict)


{-| Same as [get](ID.Dict#get) but with the arguments swapped, useful for pipelines -}
getFrom : Internal.Dict counter (ID a) value -> ID a -> Maybe value
getFrom dict id =
    get id dict


{-| Same as [`Dict.insert`](https://package.elm-lang.org/packages/elm/core/latest/Dict#insert) -}
insert : ID a -> value -> Internal.Dict counter (ID a) value -> Internal.Dict counter (ID a) value
insert id value dict =
    case dict of
        WithoutCounter noCounter inner ->
            WithoutCounter noCounter ( Dict.insert ( unpack id ) value inner )

        WithCounter counter inner ->
            WithCounter ( Internal.accountForID id counter ) ( Dict.insert ( unpack id ) value inner )



{-| Same as [`Dict.remove`](https://package.elm-lang.org/packages/elm/core/latest/Dict#remove) -}
remove : ID a -> Internal.Dict counter (ID a) value -> Internal.Dict counter (ID a) value
remove id dict =
    packDict dict (Dict.remove (unpack id) (unpackDict dict))


{-| Same as [`Dict.update`](https://package.elm-lang.org/packages/elm/core/latest/Dict#update) but does not operate on Maybe values -}
update : ID a -> (value -> value) -> Internal.Dict counter (ID a) value -> Internal.Dict counter (ID a) value
update id func dict =
    case get id dict of
        Just value ->
            insert id (func value) dict

        Nothing ->
            dict


{-| Same as [`Dict.mapFunc`](https://package.elm-lang.org/packages/elm/core/latest/Dict#mapFunc) -}
mapFunc : (ID a -> b -> c) -> (Int -> b -> c)
mapFunc func k v =
    func (ID k) v


{-| Same as [`Dict.map`](https://package.elm-lang.org/packages/elm/core/latest/Dict#map) -}
map : (ID a -> b -> c) -> Internal.Dict counter (ID a) b -> Internal.Dict counter (ID a) c
map func =
    mapDict (Dict.map (mapFunc func))


{-| Apply a function to values that appear in both dictionaries and create a new dictionary with the results, values that only appear in one dictionary are skipped -}
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


{-| Same as [`Dict.filter`](https://package.elm-lang.org/packages/elm/core/latest/Dict#filter) -}
filter : (ID a -> value -> Bool) -> Internal.Dict counter (ID a) value -> Internal.Dict counter (ID a) value
filter predicate =
    mapDict (Dict.filter (mapFunc predicate))


{-| Same as [`List.filterMap`](https://package.elm-lang.org/packages/elm/core/latest/List#filterMap) but for Dicts -}
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


{-| Same as [`Dict.foldl`](https://package.elm-lang.org/packages/elm/core/latest/Dict#foldl) -}
fold : (ID a -> value -> c -> c) -> c -> Internal.Dict counter (ID a) value -> c
fold func start dict =
    Dict.foldl (mapFunc func) start (unpackDict dict)


{-| Same as [`Dict.keys`](https://package.elm-lang.org/packages/elm/core/latest/Dict#keys) -}
ids : Internal.Dict counter (ID a) value -> List (ID a)
ids dict =
    Dict.foldr (\key _ idList -> ID key :: idList) [] (unpackDict dict)


{-| Same as [`Dict.values`](https://package.elm-lang.org/packages/elm/core/latest/Dict#values) -}
values : Internal.Dict counter (ID a) value -> List value
values dict =
    Dict.values (unpackDict dict)


{-| Same as [`Dict.toList`](https://package.elm-lang.org/packages/elm/core/latest/Dict#toList) -}
toList : Internal.Dict counter (ID a) value -> List ( ID a, value )
toList dict =
    Dict.foldr (\key value list -> ( ID key, value ) :: list) [] (unpackDict dict)


{-| Same as [`Dict.partition`](https://package.elm-lang.org/packages/elm/core/latest/Dict#partition) -}
partition : (ID a -> value -> Bool) -> Internal.Dict counter (ID a) value -> ( Dict (ID a) value, Dict (ID a) value )
partition test dict =
    Dict.partition (mapFunc test) (unpackDict dict)
    |> Tuple.mapBoth pack pack


{-| Same as [`Dict.diff`](https://package.elm-lang.org/packages/elm/core/latest/Dict#diff) -}
diff : Internal.Dict counter1 (ID a) b -> Internal.Dict counter2 (ID a) c -> Internal.Dict counter1 (ID a) b
diff dict1 dict2 =
    Dict.diff (unpackDict dict1) (unpackDict dict2)
    |> packDict dict1

{-| Same as [`Dict.intersect`](https://package.elm-lang.org/packages/elm/core/latest/Dict#intersect), but can accept dictionaries with different value types -}
intersect : Internal.Dict counter1 (ID a) b -> Internal.Dict counter2 (ID a) c -> Internal.Dict counter1 (ID a) b
intersect dict1 dict2 =
    filter (\k _ -> member k dict2) dict1


{-| Same as [`Dict.union`](https://package.elm-lang.org/packages/elm/core/latest/Dict#union) -}
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
