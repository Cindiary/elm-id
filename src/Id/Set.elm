module Id.Set exposing
    ( Set
    , Set_
    , empty
    , singleton
    , fromList
    , fromDict
    , toList
    , isEmpty
    , size
    , member
    , insert
    , remove
    , accountForIds
    , filter
    , fold
    , partition
    , diff
    , intersect
    , union
    , encode
    , decoder
    )

{-|
@docs Set, Set_

@docs empty, singleton, fromList, fromDict, toList

@docs isEmpty, size, member

@docs insert, remove

@docs accountForIds

@docs filter, fold

@docs partition, diff, intersect, union

# Json
@docs encode, decoder
-}

import Internal exposing (Id, unpackId, unpack)
import Id.Dict
import Dict

import Json.Encode as E
import Json.Decode as D
import Id.Counter

{-| A set type for Ids, is equal to a [`Id.Dict`](Id.Dict#Dict) with empty tuples as values -}
type alias Set id = Id.Dict.Dict id ()

{-| A type that can represent [`Id.Set`](Id.Set#Set) and [`Id.CounterDict id ()`](Id.CounterDict#CounterDict) -}
type alias Set_ counter id = Id.Dict.Dict_ counter id ()


pack : Dict.Dict Int () -> Set id
pack =
    Internal.WithoutCounter ()


{-| Create a empty [`Set`](Id.Set#Set) -}
empty : Set (Id a)
empty =
    Id.Dict.empty


{-| Create a set with a single Id -}
singleton : Id a -> Set (Id a)
singleton id =
    insert id empty


{-| Create a set from a list of Ids -}
fromList : List (Id a) -> Set (Id a)
fromList list =
    list
    |> List.foldl ( \id dict -> Dict.insert ( unpackId id ) () dict ) Dict.empty
    |> pack


{-| Create a set with the Ids from a [`Dict`](Id.Dict#Dict) -}
fromDict : Id.Dict.Dict_ counter (Id a) value -> Set_ counter (Id a)
fromDict dict =
    unpack dict
    |> Dict.map ( \_ _ -> () )
    |> Internal.pack dict


{-| Get all of the Ids in a set, sorted from lowest to highest. -}
toList : Set_ counter (Id a) -> List ( Id a )
toList =
    Id.Dict.ids


{-| Determine if a set is empty. -}
isEmpty : Set_ counter (Id a) -> Bool
isEmpty =
    Id.Dict.isEmpty


{-| Determine the number of Ids in the set. -}
size : Set_ counter (Id a) -> Int
size =
    Id.Dict.size


{-| Determine if a Id is in a set. -}
member : Id a -> Set_ counter (Id a) -> Bool
member =
    Id.Dict.member


{-| Insert a Id into a set. -}
insert : Id a -> Set_ counter (Id a) -> Set_ counter (Id a)
insert id set =
    Id.Dict.insert id () set


{-| Remove a Id from a set. If the Id is not found, no changes are made. -}
remove : Id a -> Set_ counter (Id a) -> Set_ counter (Id a)
remove =
    Id.Dict.remove


{-| Same as [`Id.Counter.accountForIds`](Id.Counter#accountForIds) but can be used with a [`Set`](Id.Set#Set) -}
accountForIds : Set_ counter (Id a) -> Id.Counter.Counter (Id a) -> Id.Counter.Counter (Id a)
accountForIds =
    Id.Dict.accountForIds


{-| Keep only the Ids that pass the given test. -}
filter : ( Id a -> Bool ) -> Set_ counter (Id a) -> Set_ counter (Id a)
filter predicate =
    Id.Dict.filter ( \id () -> predicate id )


{-| Fold over the Ids in a set from lowest Id to highest Id. -}
fold : (Id a -> c -> c) -> c -> Set_ counter (Id a) -> c
fold func start set =
    Dict.foldl ( \id _ acc -> func ( Internal.Id id ) acc ) start ( unpack set )


{-| Partition a set according to some test. The first set contains all Ids which passed the test, and the second contains the Ids that did not. -}
partition : (Id a -> Bool) -> Set_ counter (Id a) -> ( Set_ counter (Id a), Set (Id a) )
partition func set =
    Id.Dict.partition ( \id () -> func id ) set


{-| Get the difference between the first set and the second. Keeps Ids that do not appear in the second set. -}
diff : Set_ counter1 (Id a) -> Id.Dict.Dict_ counter2 (Id a) b -> Set_ counter1 (Id a)
diff =
    Id.Dict.diff


{-| Get the intersection of two sets. Keeps Ids that appear in both sets. -}
intersect : Set_ counter1 (Id a) -> Id.Dict.Dict_ counter2 (Id a) b -> Set_ counter1 (Id a)
intersect =
    Id.Dict.intersect


{-| Get the union of two sets. Keep all Ids. -}
union : Id.Dict.Dict_ counter1 (Id a) b -> Set_ counter2 (Id a) -> Set_ counter2 (Id a)
union dict1 set2 =
    Id.Dict.fold ( \id _ set -> insert id set ) set2 dict1


{-| Encode [`Set`](Id.Set#Set) to a JSON value -}
encode : Set id -> E.Value
encode set =
    Dict.keys ( unpack set )
    |> E.list E.int


{-| JSON Decoder for [`Set`](Id.Set#Set) -}
decoder : D.Decoder ( Set id )
decoder =
    D.list D.int
    |> D.map ( \ids_ -> pack ( Dict.fromList ( List.map ( \id -> ( id, () ) ) ids_ ) ) )