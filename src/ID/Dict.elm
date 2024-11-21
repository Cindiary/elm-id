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

import Internal exposing (ID(..), Dict(..), unpack)
import Dict

import Json.Encode as E
import Json.Decode as D


type alias Dict id value = Internal.Dict id value


empty : Dict (ID a) value
empty =
    IDDict Dict.empty


unpackDict : Dict (ID a) value -> Dict.Dict Int value
unpackDict (IDDict dict) =
    dict


isEmpty : Dict (ID a) value -> Bool
isEmpty =
    Dict.isEmpty << unpackDict


size : Dict (ID a) value -> Int
size =
    Dict.size << unpackDict


fromList : List ( ID a, value ) -> Dict (ID a) value
fromList list =
    IDDict ( Dict.fromList ( List.map ( Tuple.mapFirst unpack ) list ) )


member : ID a -> Dict (ID a) value -> Bool
member id (IDDict dict) =
    Dict.member (unpack id) dict


get : ID a -> Dict (ID a) value -> Maybe value
get id (IDDict dict) =
    Dict.get (unpack id) dict


getFrom : Dict (ID a) value -> ID a -> Maybe value
getFrom dict id =
    get id dict


insert : ID a -> value -> Dict (ID a) value -> Dict (ID a) value
insert id value (IDDict dict) =
    IDDict (Dict.insert (unpack id) value dict)


remove : ID a -> Dict (ID a) value -> Dict (ID a) value
remove id (IDDict dict) =
    IDDict (Dict.remove (unpack id) dict)


update : ID a -> (value -> value) -> Dict (ID a) value -> Dict (ID a) value
update id func dict =
    case get id dict of
        Just value ->
            insert id (func value) dict

        Nothing ->
            dict


mapFunc : (ID a -> b -> c) -> (Int -> b -> c)
mapFunc func k v =
    func (ID k) v


map : (ID a -> b -> c) -> Dict (ID a) b -> Dict (ID a) c
map func (IDDict dict) =
    IDDict (Dict.map (mapFunc func) dict)


filter : (ID a -> value -> Bool) -> Dict (ID a) value -> Dict (ID a) value
filter predicate (IDDict dict) =
    IDDict (Dict.filter (mapFunc predicate) dict)


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


fold : (ID a -> value -> c -> c) -> c -> Dict (ID a) value -> c
fold func start (IDDict dict) =
    Dict.foldl (mapFunc func) start dict


ids : Dict (ID a) value -> List (ID a)
ids (IDDict dict) =
    Dict.foldr (\key _ idList -> ID key :: idList) [] dict


values : Dict (ID a) value -> List value
values (IDDict dict) =
    Dict.values dict


toList : Dict (ID a) value -> List ( ID a, value )
toList (IDDict dict) =
    Dict.foldr (\key value list -> ( ID key, value ) :: list) [] dict


{-| Partition a dictionary according to some test. The first dictionary
contains all key-value pairs which passed the test, and the second contains
the pairs that did not.
-}
partition : (ID a -> value -> Bool) -> Dict (ID a) value -> ( Dict (ID a) value, Dict (ID a) value )
partition test (IDDict dict) =
    Dict.partition (mapFunc test) dict
    |> Tuple.mapBoth ( IDDict ) ( IDDict )


{-| Keep a key-value pair when its key does not appear in the second dictionary.
-}
diff : Dict (ID a) b -> Dict (ID a) c -> Dict (ID a) b
diff (IDDict dict1) (IDDict dict2) =
    IDDict (Dict.diff dict1 dict2)


{-| Keep a key-value pair when its key appears in the second dictionary.
-}
intersect : Dict (ID a) b -> Dict (ID a) c -> Dict (ID a) b
intersect dict1 dict2 =
    filter (\k _ -> member k dict2) dict1


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.
-}
union : Dict (ID a) value -> Dict (ID a) value -> Dict (ID a) value
union (IDDict dict1) (IDDict dict2) =
    IDDict (Dict.union dict1 dict2)


encode : ( value -> E.Value ) -> Dict id value -> E.Value
encode encodeValue (IDDict dict) =
    Dict.toList dict
    |> List.map ( Tuple.mapBoth String.fromInt encodeValue )
    |> E.object


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

