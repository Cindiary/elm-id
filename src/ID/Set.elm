module ID.Set exposing
    ( Set
    , empty
    , singleton
    , fromList
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

@docs empty, singleton, fromList, toList

@docs isEmpty, size, member

@docs insert, remove

@docs filter, fold

@docs partition, diff, intersect, union

# Json
@docs encode, decoder
-}

import Internal exposing (ID, unpackDict)
import ID.Dict
import Dict

import Json.Encode as E
import Json.Decode as D

{-| A set type for IDs, is equal to a [`ID.Dict`](ID.Dict#Dict) with empty tuples as values -}
type alias Set id =
    ID.Dict.Dict id ()


{-| Create a empty [`Dict`](ID.Dict#Dict) -}
empty : Set (ID a)
empty =
    ID.Dict.empty


{-| Create a set with a single ID -}
singleton : ID a -> Set (ID a)
singleton id =
    insert id empty


{-| Create a set from a list of IDs -}
fromList : List ( ID a ) -> Set (ID a)
fromList list =
    ID.Dict.fromList (List.map (\id -> ( id, () )) list)


{-| Get all of the IDs in a set, sorted from lowest to highest. -}
toList : Set (ID a) -> List ( ID a )
toList =
    ID.Dict.ids


{-| Determine if a set is empty. -}
isEmpty : Set (ID a) -> Bool
isEmpty =
    ID.Dict.isEmpty


{-| Determine the number of IDs in the set. -}
size : Set (ID a) -> Int
size =
    ID.Dict.size


{-| Determine if a ID is in a set. -}
member : ID a -> Set (ID a) -> Bool
member =
    ID.Dict.member


{-| Insert a ID into a set. -}
insert : ID a -> Set (ID a) -> Set (ID a)
insert id set =
    ID.Dict.insert id () set


{-| Remove a ID from a set. If the ID is not found, no changes are made. -}
remove : ID a -> Set (ID a) -> Set (ID a)
remove =
    ID.Dict.remove


{-| Keep only the IDs that pass the given test. -}
filter : ( ID a -> Bool ) -> Set (ID a) -> Set (ID a)
filter predicate =
    ID.Dict.filter ( \id () -> predicate id )


{-| Fold over the IDs in a set from lowest ID to highest ID. -}
fold : (ID a -> c -> c) -> c -> Set (ID a) -> c
fold func start set =
    Dict.foldl ( \id _ acc -> func ( Internal.ID id ) acc ) start ( unpackDict set )


{-| Partition a set according to some test. The first set contains all IDs which passed the test, and the second contains the IDs that did not. -}
partition : (ID a -> Bool) -> Set (ID a) -> ( Set (ID a), Set (ID a) )
partition func set =
    ID.Dict.partition ( \id () -> func id ) set


{-| Get the difference between the first set and the second. Keeps IDs that do not appear in the second set. -}
diff : Set (ID a) -> Set (ID a) -> Set (ID a)
diff =
    ID.Dict.diff


{-| Get the intersection of two sets. Keeps IDs that appear in both sets. -}
intersect : Set (ID a) -> Set (ID a) -> Set (ID a)
intersect =
    ID.Dict.intersect


{-| Get the union of two sets. Keep all IDs. -}
union : Set (ID a) -> Set (ID a) -> Set (ID a)
union =
    ID.Dict.union


{-| Encode [`Set`](ID.Set#Set) to a JSON value -}
encode : Set id -> E.Value
encode set =
    Dict.keys ( unpackDict set )
    |> E.list E.int


{-| JSON Decoder for [`Set`](ID.Set#Set) -}
decoder : D.Decoder ( Set id )
decoder =
    D.list D.int
    |> D.map ( \ids_ -> Internal.WithoutCounter () ( Dict.fromList ( List.map ( \id -> ( id, () ) ) ids_ ) ) )