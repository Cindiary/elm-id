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
@docs member, get, getFrom, insert, remove, update
@docs map, filterMap, fold
@docs ids, values
@docs partition
@docs diff, intersect, union

#Json
@docs encode, decoder
-}

import Internal exposing (ID(..), Dict(..), unpack)
import Dict

import Json.Encode as E
import Json.Decode as D


{-| A [`Dict`](https://package.elm-lang.org/packages/elm/core/latest/Dict#Dict) for [`ID`](ID#ID)s -}
type alias Dict id value = Internal.Dict id value


{-| Same as [`Dict.empty`](https://package.elm-lang.org/packages/elm/core/latest/Dict#empty) -}
empty : Dict (ID a) value
empty =
    IDDict Dict.empty


unpackDict : Dict (ID a) value -> Dict.Dict Int value
unpackDict (IDDict dict) =
    dict


{-| Same as [`Dict.isEmpty`](https://package.elm-lang.org/packages/elm/core/latest/Dict#isEmpty) -}
isEmpty : Dict (ID a) value -> Bool
isEmpty =
    Dict.isEmpty << unpackDict


{-| Same as [`Dict.size`](https://package.elm-lang.org/packages/elm/core/latest/Dict#size) -}
size : Dict (ID a) value -> Int
size =
    Dict.size << unpackDict


{-| Same as [`Dict.fromList`](https://package.elm-lang.org/packages/elm/core/latest/Dict#fromList) -}
fromList : List ( ID a, value ) -> Dict (ID a) value
fromList list =
    IDDict ( Dict.fromList ( List.map ( Tuple.mapFirst unpack ) list ) )


{-| Same as [`Dict.member`](https://package.elm-lang.org/packages/elm/core/latest/Dict#member) -}
member : ID a -> Dict (ID a) value -> Bool
member id (IDDict dict) =
    Dict.member (unpack id) dict


{-| Same as [`Dict.get`](https://package.elm-lang.org/packages/elm/core/latest/Dict#get) -}
get : ID a -> Dict (ID a) value -> Maybe value
get id (IDDict dict) =
    Dict.get (unpack id) dict


{-| Same as [`get`](ID.Dict#get) but with the arguments swapped, useful for pipelines -}
getFrom : Dict (ID a) value -> ID a -> Maybe value
getFrom dict id =
    get id dict


{-| Same as [`Dict.insert`](https://package.elm-lang.org/packages/elm/core/latest/Dict#insert) -}
insert : ID a -> value -> Dict (ID a) value -> Dict (ID a) value
insert id value (IDDict dict) =
    IDDict (Dict.insert (unpack id) value dict)


{-| Same as [`Dict.remove`](https://package.elm-lang.org/packages/elm/core/latest/Dict#remove) -}
remove : ID a -> Dict (ID a) value -> Dict (ID a) value
remove id (IDDict dict) =
    IDDict (Dict.remove (unpack id) dict)


{-| Same as [`Dict.update`](https://package.elm-lang.org/packages/elm/core/latest/Dict#update) but does not operate on Maybe values -}
update : ID a -> (value -> value) -> Dict (ID a) value -> Dict (ID a) value
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
map : (ID a -> b -> c) -> Dict (ID a) b -> Dict (ID a) c
map func (IDDict dict) =
    IDDict (Dict.map (mapFunc func) dict)


{-| Same as [`Dict.filter`](https://package.elm-lang.org/packages/elm/core/latest/Dict#filter) -}
filter : (ID a -> value -> Bool) -> Dict (ID a) value -> Dict (ID a) value
filter predicate (IDDict dict) =
    IDDict (Dict.filter (mapFunc predicate) dict)


{-| Same as [`List.filterMap`](https://package.elm-lang.org/packages/elm/core/latest/List#filterMap) but for Dicts -}
filterMap : (ID a -> b -> Maybe c) -> Dict (ID a) b -> Dict (ID a) c
filterMap func dict =
    let
        foldFunc id val newDict =
            case func id val of
                Just newVal ->
                    insert id newVal newDict

                Nothing ->
                    newDict
    in
    fold foldFunc empty dict


{-| Same as [`Dict.foldl`](https://package.elm-lang.org/packages/elm/core/latest/Dict#foldl) -}
fold : (ID a -> value -> c -> c) -> c -> Dict (ID a) value -> c
fold func start (IDDict dict) =
    Dict.foldl (mapFunc func) start dict


{-| Same as [`Dict.keys`](https://package.elm-lang.org/packages/elm/core/latest/Dict#keys) -}
ids : Dict (ID a) value -> List (ID a)
ids (IDDict dict) =
    Dict.foldr (\key _ idList -> ID key :: idList) [] dict


{-| Same as [`Dict.values`](https://package.elm-lang.org/packages/elm/core/latest/Dict#values) -}
values : Dict (ID a) value -> List value
values (IDDict dict) =
    Dict.values dict


{-| Same as [`Dict.toList`](https://package.elm-lang.org/packages/elm/core/latest/Dict#toList) -}
toList : Dict (ID a) value -> List ( ID a, value )
toList (IDDict dict) =
    Dict.foldr (\key value list -> ( ID key, value ) :: list) [] dict


{-| Same as [`Dict.partition`](https://package.elm-lang.org/packages/elm/core/latest/Dict#partition) -}
partition : (ID a -> value -> Bool) -> Dict (ID a) value -> ( Dict (ID a) value, Dict (ID a) value )
partition test (IDDict dict) =
    Dict.partition (mapFunc test) dict
    |> Tuple.mapBoth ( IDDict ) ( IDDict )


{-| Same as [`Dict.diff`](https://package.elm-lang.org/packages/elm/core/latest/Dict#diff) -}
diff : Dict (ID a) b -> Dict (ID a) c -> Dict (ID a) b
diff (IDDict dict1) (IDDict dict2) =
    IDDict (Dict.diff dict1 dict2)

{-| Same as [`Dict.intersect`](https://package.elm-lang.org/packages/elm/core/latest/Dict#intersect), but can accept dictionaries with different value types -}
intersect : Dict (ID a) b -> Dict (ID a) c -> Dict (ID a) b
intersect dict1 dict2 =
    filter (\k _ -> member k dict2) dict1


{-| Same as [`Dict.union`](https://package.elm-lang.org/packages/elm/core/latest/Dict#union) -}
union : Dict (ID a) value -> Dict (ID a) value -> Dict (ID a) value
union (IDDict dict1) (IDDict dict2) =
    IDDict (Dict.union dict1 dict2)


{-| Encode a `Dict` to a JSON value -}
encode : ( value -> E.Value ) -> Dict id value -> E.Value
encode encodeValue (IDDict dict) =
    Dict.toList dict
    |> List.map ( Tuple.mapBoth String.fromInt encodeValue )
    |> E.object


{-| JSON Decoder for `Dict` -}
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
    |> D.map IDDict

