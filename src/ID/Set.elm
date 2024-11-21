module ID.Set exposing
    ( Set
    , empty
    , fromList
    , toList
    , isEmpty
    , size
    , member
    , insert
    , remove
    , fold
    , diff
    , intersect
    , union
    , encode
    , decoder
    )

{-|
@docs Set

@docs empty, fromList, toList

@docs isEmpty, size, member

@docs insert, remove

@docs fold

@docs diff, intersect, union

# Json
@docs encode, decoder
-}

import Internal exposing (ID(..))
import ID.Dict
import Dict

import Json.Encode as E
import Json.Decode as D
import Internal exposing (Dict(..))

{-| A set type for IDs, is equal to a [`ID.Dict`](ID.Dict#Dict) with empty tuples as values -}
type alias Set id =
    ID.Dict.Dict id ()


{-| Alias for [`ID.Dict.empty`](ID.Dict#empty) -}
empty : Set (ID a)
empty =
    ID.Dict.empty


{-| Create a set from a list of IDs -}
fromList : List ( ID a ) -> Set (ID a)
fromList list =
    ID.Dict.fromList (List.map (\id -> ( id, () )) list)


{-| Alias for [`ID.Dict.ids`](ID.Dict#ids) -}
toList : Set (ID a) -> List ( ID a )
toList =
    ID.Dict.ids


{-| Alias for [`ID.Dict.isEmpty`](ID.Dict#isEmpty) -}
isEmpty : Set (ID a) -> Bool
isEmpty =
    ID.Dict.isEmpty


{-| Alias for [`ID.Dict.size`](ID.Dict#size) -}
size : Set (ID a) -> Int
size =
    ID.Dict.size


{-| Alias for [`ID.Dict.member`](ID.Dict#member) -}
member : ID a -> Set (ID a) -> Bool
member =
    ID.Dict.member


{-| Alias for [`ID.Dict.insert`](ID.Dict#insert) but without a value -}
insert : ID a -> Set (ID a) -> Set (ID a)
insert id set =
    ID.Dict.insert id () set


{-| Alias for [`ID.Dict.remove`](ID.Dict#remove) -}
remove : ID a -> Set (ID a) -> Set (ID a)
remove =
    ID.Dict.remove


{-| Alias for [`ID.Dict.fold`](ID.Dict#fold) but without a value argument -}
fold : (ID a -> c -> c) -> c -> Set (ID a) -> c
fold func start ( IDDict _ dict ) =
    Dict.foldl ( \id _ acc -> func ( ID id ) acc ) start dict


{-| Alias for [`ID.Dict.diff`](ID.Dict#diff) -}
diff : Set (ID a) -> Set (ID a) -> Set (ID a)
diff =
    ID.Dict.diff


{-| Alias for [`ID.Dict.intersect`](ID.Dict#intersect) -}
intersect : Set (ID a) -> Set (ID a) -> Set (ID a)
intersect =
    ID.Dict.intersect


{-| Alias for [`ID.Dict.union`](ID.Dict#union) -}
union : Set (ID a) -> Set (ID a) -> Set (ID a)
union =
    ID.Dict.union


{-| Encode [`Set`](ID.Set#Set) to a JSON value -}
encode : Set id -> E.Value
encode ( IDDict _ set ) =
    Dict.keys set
    |> E.list E.int


{-| JSON Decoder for [`Set`](ID.Set#Set) -}
decoder : D.Decoder ( Set id )
decoder =
    D.list D.int
    |> D.map ( \ids_ -> IDDict () ( Dict.fromList ( List.map ( \id -> ( id, () ) ) ids_ ) ) )