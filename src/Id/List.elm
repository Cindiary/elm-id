module Id.List exposing
    ( List
    , empty
    , singleton
    , isEmpty
    , length
    , member
    , get
    , getFrom
    , insert
    , insertAt
    , move
    , remove
    , update
    , accountForIds
    , map
    , map2
    , map3
    , map4
    , removeDuplicateIds
    , filter
    , filterMap
    , foldl
    , foldr
    , ids
    , values
    , partition
    , diff
    , intersect
    , append
    , unzip
    , encode
    , decoder
    )

{-|
@docs List

@docs empty, singleton
@docs isEmpty, length
@docs member, get, getFrom
@docs insert, insertAt, move, remove, update
@docs accountForIds
@docs map, removeDuplicateIds, filter, filterMap, foldl, foldr
@docs ids, values
@docs partition
@docs diff, intersect, append, unzip
@docs map2, map3, map4

# Json
@docs encode, decoder
-}

import Internal exposing(Id)
import Core as List

import Json.Encode as E
import Json.Decode as D
import Parser as P exposing((|=), (|.))
import Id.Counter


{-| A ordered list of Id-value pairs, is constructed as a List of Id-value Tuples. -}
type alias List id value = List.List ( id, value )


{-| Create a empty [`List`](Id.List#List) -}
empty : List id value
empty =
    []


{-| Create a Id.List with a single Id-value pair -}
singleton : id -> value -> List id value
singleton id value =
    [ ( id, value ) ]


{-| Determine if a Id.List is empty. -}
isEmpty : List id value -> Bool
isEmpty =
    List.isEmpty


{-| Determine the number of Id-value pairs in the List. -}
length : List id value -> Int
length =
    List.length


{-| Determine if a Id is in a Id.List. -}
member : id -> List id value -> Bool
member id list =
    List.any ( \( id_, _ ) -> id_ == id ) list


{-| Get the value associated with a Id. If the Id is not found, return Nothing. -}
get : id -> List id value -> Maybe value
get id list =
    case list of
        ( id_, value ) :: ls ->
            if id_ == id then
                Just value
            else
                get id ls
        
        [] ->
            Nothing


{-| Same as [get](Id.List#get) but with the arguments swapped, useful for pipelines. -}
getFrom : List id value -> id -> Maybe value
getFrom list id =
    get id list


{-|
Insert a Id-value pair into a Id.List.
If there is a existing pair with the same Id if will be replaced.
If there isn't a existing pair the pair will be inserted at the start of the list.
-}
insert : id -> value -> List id value -> List id value
insert id value list =
    if member id list then
        List.map ( \( ( id_, _ ) as pair ) -> if id_ == id then ( id, value ) else pair ) list
    else
        ( id, value ) :: list


{-|
Insert a Id-value pair into a given index into a Id.List.
This will remove any existing value with the same Id.
Indexes lower than 0 will be inserted at index 0.
Indexes higher than the length of the list will inserted at the end of the list.
-}
insertAt : Int -> id -> value -> List id value -> List id value
insertAt index id value list =
    let filtered = remove id list in
    List.take index filtered ++ ( id, value ) :: List.drop index filtered


{-| Move a existing value to a new index. Does nothing if the item is not present. -}
move : id -> Int -> List id value -> List id value
move id index list =
    case get id list of
        Just value ->
            insertAt index id value list
        
        Nothing ->
            list


{-| Remove a Id-value pair from a Id.List. If the Id is not found, no changes are made. -}
remove : id -> List id value -> List id value
remove id list =
    List.filter ( \( id_, _ ) -> id_ /= id ) list


{-| Update the value of a Id.List for a specific Id with a given function. If the Id is not found, no changes are made. -}
update : id -> (value -> value) -> List id value -> List id value
update id func list =
    List.map ( \( ( id_, value ) as pair ) -> if id_ == id then ( id, func value ) else pair ) list


{-| Same as [`Id.Counter.accountForIds`](Id.Counter#accountForIds) but can be used with a [`Id.List`](Id.List#List) -}
accountForIds : List (Id a) value -> Id.Counter.Counter (Id a) -> Id.Counter.Counter (Id a)
accountForIds =
    Id.Counter.accountForItemIds Tuple.first


mapFunc : ( id -> a -> b ) -> ( ( id, a ) -> b )
mapFunc func ( id, value ) =
    func id value


{-| Apply a function to all values in a Id.List. -}
map : (id -> b -> c) -> List id b -> List id c
map func list =
    List.map ( \( id, value ) -> ( id, func id value ) ) list


{-| Apply a function to values that appear in both Lists and create a new List with the results, values that don't appear in all Lists are skipped. -}
map2 : (id -> a -> b -> c) -> List id a -> List id b -> List id c
map2 func list1 list2 =
    let
        foldFunc id value1 resList =
            case get id list2 of
                Just value2 ->
                    ( id, func id value1 value2 ) :: resList
                
                Nothing ->
                    resList
    in
    foldr foldFunc [] list1


{-| Same as [`Id.List.map2`](Id.List#map2) but for 3 Lists -}
map3 : (id -> a -> b -> c -> d) -> List id a -> List id b -> List id c -> List id d
map3 func list1 list2 list3 =
    let
        foldFunc id value1 resList =
            case Maybe.map2 ( func id value1 ) ( get id list2 ) ( get id list3 ) of
                Just result ->
                    ( id, result ) :: resList
                
                Nothing ->
                    resList
    in
    foldr foldFunc [] list1


{-| Same as [`Id.List.map2`](Id.List#map2) but for 4 Lists -}
map4 : (id -> a -> b -> c -> d -> e) -> List id a -> List id b -> List id c -> List id d -> List id e
map4 func list1 list2 list3 list4 =
    let
        foldFunc id value1 resList =
            case Maybe.map3 ( func id value1 ) ( get id list2 ) ( get id list3 ) ( get id list4 ) of
                Just result ->
                    ( id, result ) :: resList
                
                Nothing ->
                    resList
    in
    foldr foldFunc [] list1


{-| Removes pairs that have a duplicate Id. Retains the last pair. -}
removeDuplicateIds : List.List ( id, value ) -> List id value
removeDuplicateIds list =
    let
        fold ( ( id, _ ) as pair ) target = pair :: remove id target
    in
    List.foldr fold [] list


{-| Keep only the Id-value pairs that pass the given test. -}
filter : (id -> value -> Bool) -> List id value -> List id value
filter predicate list =
    List.filter (mapFunc predicate) list


{-| Apply a function to all values in the List and filter out the values that return Nothing. -}
filterMap : (id -> b -> Maybe c) -> List id b -> List id c
filterMap func list =
    let
        foldFunc id val newList =
            case func id val of
                Just newVal ->
                    ( id, newVal ) :: newList

                Nothing ->
                    newList
    in
    foldr foldFunc [] list


{-| Reduce the list from the left. -}
foldr : (id -> value -> c -> c) -> c -> List id value -> c
foldr func start list =
    List.foldl (mapFunc func) start list


{-| Reduce the list from the left. -}
foldl : (id -> value -> c -> c) -> c -> List id value -> c
foldl func start list =
    List.foldl (mapFunc func) start list


{-| Get all of the Ids in the List. -}
ids : List id value -> List.List id
ids list =
    List.map Tuple.first list


{-| Get all of the values in the List. -}
values : List id value -> List.List value
values list =
    List.map Tuple.second list


{-| Partition a Id.List according to some test. The first List contains all Id-value pairs which passed the test, and the second contains the pairs that did not. -}
partition : (id -> value -> Bool) -> List id value -> ( List id value, List id value )
partition test list =
    List.partition ( mapFunc test ) list


{-| Keep a Id-value pair when its Id does not appear in the second List. -}
diff : List id b -> List id c -> List id b
diff list1 list2 =
    filter ( \id _ -> not ( member id list2 ) ) list1


{-| Keep a Id-value pair when its Id appears in the second List. Preference is given to values in the first List. -}
intersect : List id b -> List id c -> List id b
intersect list1 list2 =
    filter (\id _ -> member id list2) list1


{-| Combine two Lists. If there is a collision, preference is given to the first List. -}
append : List id value -> List id value -> List id value
append list1 list2 =
    list1 ++ List.filter ( \( id, _ ) -> not ( member id list1 ) ) list2


{-| Decompose a list of tuples into a tuple of lists. -}
unzip : List id ( a, b ) -> ( List id a, List id b )
unzip list =
    ( List.map ( Tuple.mapSecond Tuple.first ) list
    , List.map ( Tuple.mapSecond Tuple.second ) list
    )


{-| Encode a [`List`](Id.List#List) to a JSON value -}
encode : ( value -> E.Value ) -> List ( Id id ) value -> E.Value
encode encodeValue list =
    list
    |> List.map ( \( id, value ) -> ( "[" ++ Internal.idToString id ++ "]", encodeValue value ) )
    |> E.object


keyParser : P.Parser ( Id id )
keyParser =
    P.oneOf
    [ P.succeed identity
        |. P.symbol "["
        |= Internal.idParser
        |. P.symbol "]"
    , Internal.idParser
    ]


{-| JSON Decoder for [`List`](Id.List#List) -}
decoder : D.Decoder value -> D.Decoder ( List ( Id id ) value )
decoder valueDecoder =
    let
        addPair ( key, value ) ( list, errors ) =
            case P.run keyParser key of
                Ok id ->
                    ( ( id, value ) :: list, errors )
                
                Err _ ->
                    ( list, D.fail ( "Invalid key: " ++ key ) :: errors )
        fromPairs pairs =
            case List.foldr addPair ( [], [] ) pairs of
                ( list, [] ) ->
                    D.succeed list
                
                ( _, [ err ] ) ->
                    err
                
                ( _, errors ) ->
                    D.oneOf errors
    in
    D.keyValuePairs valueDecoder
    |> D.andThen fromPairs

