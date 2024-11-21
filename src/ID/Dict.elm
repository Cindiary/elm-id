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

# Json
@docs encode, decoder
-}

import Internal exposing (ID(..), Dict(..), unpack)
import Dict

import Json.Encode as E
import Json.Decode as D
import Internal exposing (Counter(..))


{-| A [`Dict`](https://package.elm-lang.org/packages/elm/core/latest/Dict#Dict) for [`ID`](ID#ID), value pairs -}
type alias Dict id value = Internal.Dict () id value


{-| Create a empty [`Dict`](ID.Dict#Dict) -}
empty : Dict (ID a) value
empty =
    IDDict () Dict.empty


unpackDict : Internal.Dict counter (ID a) value -> Dict.Dict Int value
unpackDict (IDDict _ dict) =
    dict


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
    IDDict () ( Dict.fromList ( List.map ( Tuple.mapFirst unpack ) list ) )


{-| Same as [`Dict.member`](https://package.elm-lang.org/packages/elm/core/latest/Dict#member) -}
member : ID a -> Internal.Dict counter (ID a) value -> Bool
member id (IDDict _ dict) =
    Dict.member (unpack id) dict


{-| Same as [`Dict.get`](https://package.elm-lang.org/packages/elm/core/latest/Dict#get) -}
get : ID a -> Internal.Dict counter (ID a) value -> Maybe value
get id (IDDict _ dict) =
    Dict.get (unpack id) dict


{-| Same as [get](ID.Dict#get) but with the arguments swapped, useful for pipelines -}
getFrom : Internal.Dict counter (ID a) value -> ID a -> Maybe value
getFrom dict id =
    get id dict


{-| Same as [`Dict.insert`](https://package.elm-lang.org/packages/elm/core/latest/Dict#insert) -}
insert : ID a -> value -> Internal.Dict counter (ID a) value -> Internal.Dict counter (ID a) value
insert id value (IDDict counter dict) =
    IDDict counter (Dict.insert (unpack id) value dict)



{-| Same as [`Dict.remove`](https://package.elm-lang.org/packages/elm/core/latest/Dict#remove) -}
remove : ID a -> Internal.Dict counter (ID a) value -> Internal.Dict counter (ID a) value
remove id (IDDict counter dict) =
    IDDict counter (Dict.remove (unpack id) dict)


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
map func (IDDict counter dict) =
    IDDict counter (Dict.map (mapFunc func) dict)


{-| Same as [`Dict.filter`](https://package.elm-lang.org/packages/elm/core/latest/Dict#filter) -}
filter : (ID a -> value -> Bool) -> Internal.Dict counter (ID a) value -> Internal.Dict counter (ID a) value
filter predicate (IDDict counter dict) =
    IDDict counter (Dict.filter (mapFunc predicate) dict)


{-| Same as [`List.filterMap`](https://package.elm-lang.org/packages/elm/core/latest/List#filterMap) but for Dicts -}
filterMap : (ID a -> b -> Maybe c) -> Internal.Dict counter (ID a) b -> Internal.Dict counter (ID a) c
filterMap func ( IDDict counter _ as dict ) =
    let
        foldFunc id val newDict =
            case func id val of
                Just newVal ->
                    insert id newVal newDict

                Nothing ->
                    newDict
    in
    fold foldFunc ( IDDict counter Dict.empty ) dict


{-| Same as [`Dict.foldl`](https://package.elm-lang.org/packages/elm/core/latest/Dict#foldl) -}
fold : (ID a -> value -> c -> c) -> c -> Internal.Dict counter (ID a) value -> c
fold func start (IDDict _ dict) =
    Dict.foldl (mapFunc func) start dict


{-| Same as [`Dict.keys`](https://package.elm-lang.org/packages/elm/core/latest/Dict#keys) -}
ids : Internal.Dict counter (ID a) value -> List (ID a)
ids (IDDict _ dict) =
    Dict.foldr (\key _ idList -> ID key :: idList) [] dict


{-| Same as [`Dict.values`](https://package.elm-lang.org/packages/elm/core/latest/Dict#values) -}
values : Internal.Dict counter (ID a) value -> List value
values (IDDict _ dict) =
    Dict.values dict


{-| Same as [`Dict.toList`](https://package.elm-lang.org/packages/elm/core/latest/Dict#toList) -}
toList : Internal.Dict counter (ID a) value -> List ( ID a, value )
toList (IDDict _ dict) =
    Dict.foldr (\key value list -> ( ID key, value ) :: list) [] dict


{-| Same as [`Dict.partition`](https://package.elm-lang.org/packages/elm/core/latest/Dict#partition) -}
partition : (ID a -> value -> Bool) -> Internal.Dict counter (ID a) value -> ( Dict (ID a) value, Dict (ID a) value )
partition test (IDDict _ dict) =
    Dict.partition (mapFunc test) dict
    |> Tuple.mapBoth ( IDDict () ) ( IDDict () )


{-| Same as [`Dict.diff`](https://package.elm-lang.org/packages/elm/core/latest/Dict#diff) -}
diff : Internal.Dict counter1 (ID a) b -> Internal.Dict counter2 (ID a) c -> Internal.Dict counter1 (ID a) b
diff (IDDict counter dict1) (IDDict _ dict2) =
    IDDict counter (Dict.diff dict1 dict2)

{-| Same as [`Dict.intersect`](https://package.elm-lang.org/packages/elm/core/latest/Dict#intersect), but can accept dictionaries with different value types -}
intersect : Internal.Dict counter1 (ID a) b -> Internal.Dict counter2 (ID a) c -> Internal.Dict counter1 (ID a) b
intersect dict1 dict2 =
    filter (\k _ -> member k dict2) dict1


{-| Same as [`Dict.union`](https://package.elm-lang.org/packages/elm/core/latest/Dict#union) -}
union : Internal.Dict counter1 (ID a) value -> Internal.Dict counter2 (ID a) value -> Dict (ID a) value
union (IDDict _ dict1) (IDDict _ dict2) =
    IDDict () (Dict.union dict1 dict2)


{-| Encode a [`Dict`](ID.Dict#Dict) to a JSON value -}
encode : ( value -> E.Value ) -> Dict id value -> E.Value
encode encodeValue dict =
    Internal.encodeDictFields encodeValue dict
    |> E.object


{-| JSON Decoder for [`Dict`](ID.Dict#Dict) -}
decoder : D.Decoder value -> D.Decoder ( Dict id value )
decoder =
    Internal.dictDecoder ( D.succeed () )

