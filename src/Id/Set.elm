module Id.Set exposing
    ( Set
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
@docs Set

@docs empty, singleton, fromList, fromDict, toList

@docs isEmpty, size, member

@docs insert, remove

@docs filter, fold

@docs partition, diff, intersect, union

# Json
@docs encode, decoder
-}

import Internal exposing (Id, unpack, unpackDict)
import Id.Dict
import Dict

import Json.Encode as E
import Json.Decode as D

{-| A set type for Ids, is equal to a [`Id.Dict`](Id.Dict#Dict) with empty tuples as values -}
type alias Set id =
    Id.Dict.Dict id ()


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
    |> List.foldl ( \id dict -> Dict.insert ( unpack id ) () dict ) Dict.empty
    |> pack


{-| Create a set with the Ids from a [`Dict`](Id.Dict#Dict) -}
fromDict : Id.Dict.Dict_ counter (Id a) value -> Set (Id a)
fromDict dict =
    unpackDict dict
    |> Dict.map ( \_ _ -> () )
    |> pack


{-| Get all of the Ids in a set, sorted from lowest to highest. -}
toList : Set (Id a) -> List ( Id a )
toList =
    Id.Dict.ids


{-| Determine if a set is empty. -}
isEmpty : Set (Id a) -> Bool
isEmpty =
    Id.Dict.isEmpty


{-| Determine the number of Ids in the set. -}
size : Set (Id a) -> Int
size =
    Id.Dict.size


{-| Determine if a Id is in a set. -}
member : Id a -> Set (Id a) -> Bool
member =
    Id.Dict.member


{-| Insert a Id into a set. -}
insert : Id a -> Set (Id a) -> Set (Id a)
insert id set =
    Id.Dict.insert id () set


{-| Remove a Id from a set. If the Id is not found, no changes are made. -}
remove : Id a -> Set (Id a) -> Set (Id a)
remove =
    Id.Dict.remove


{-| Keep only the Ids that pass the given test. -}
filter : ( Id a -> Bool ) -> Set (Id a) -> Set (Id a)
filter predicate =
    Id.Dict.filter ( \id () -> predicate id )


{-| Fold over the Ids in a set from lowest Id to highest Id. -}
fold : (Id a -> c -> c) -> c -> Set (Id a) -> c
fold func start set =
    Dict.foldl ( \id _ acc -> func ( Internal.Id id ) acc ) start ( unpackDict set )


{-| Partition a set according to some test. The first set contains all Ids which passed the test, and the second contains the Ids that did not. -}
partition : (Id a -> Bool) -> Set (Id a) -> ( Set (Id a), Set (Id a) )
partition func set =
    Id.Dict.partition ( \id () -> func id ) set


{-| Get the difference between the first set and the second. Keeps Ids that do not appear in the second set. -}
diff : Set (Id a) -> Set (Id a) -> Set (Id a)
diff =
    Id.Dict.diff


{-| Get the intersection of two sets. Keeps Ids that appear in both sets. -}
intersect : Set (Id a) -> Set (Id a) -> Set (Id a)
intersect =
    Id.Dict.intersect


{-| Get the union of two sets. Keep all Ids. -}
union : Set (Id a) -> Set (Id a) -> Set (Id a)
union =
    Id.Dict.union


{-| Encode [`Set`](Id.Set#Set) to a JSON value -}
encode : Set id -> E.Value
encode set =
    Dict.keys ( unpackDict set )
    |> E.list E.int


{-| JSON Decoder for [`Set`](Id.Set#Set) -}
decoder : D.Decoder ( Set id )
decoder =
    D.list D.int
    |> D.map ( \ids_ -> pack ( Dict.fromList ( List.map ( \id -> ( id, () ) ) ids_ ) ) )