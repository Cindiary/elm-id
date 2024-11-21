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

import Internal exposing (ID(..))
import ID.Dict
import Dict

import Json.Encode as E
import Json.Decode as D
import Internal exposing (Dict(..))



type alias Set id =
    ID.Dict.Dict id ()

empty : Set (ID a)
empty =
    ID.Dict.empty


fromList : List ( ID a ) -> Set (ID a)
fromList list =
    ID.Dict.fromList (List.map (\id -> ( id, () )) list)


toList : Set (ID a) -> List ( ID a )
toList =
    ID.Dict.ids


isEmpty : Set (ID a) -> Bool
isEmpty =
    ID.Dict.isEmpty


size : Set (ID a) -> Int
size =
    ID.Dict.size


member : ID a -> Set (ID a) -> Bool
member =
    ID.Dict.member


insert : ID a -> Set (ID a) -> Set (ID a)
insert id set =
    ID.Dict.insert id () set


remove : ID a -> Set (ID a) -> Set (ID a)
remove =
    ID.Dict.remove


fold : (ID a -> c -> c) -> c -> Set (ID a) -> c
fold func start ( IDDict dict ) =
    Dict.foldl ( \id _ acc -> func ( ID id ) acc ) start dict


diff : Set (ID a) -> Set (ID a) -> Set (ID a)
diff =
    ID.Dict.diff


intersect : Set (ID a) -> Set (ID a) -> Set (ID a)
intersect =
    ID.Dict.intersect


union : Set (ID a) -> Set (ID a) -> Set (ID a)
union =
    ID.Dict.union


encode : Set id -> E.Value
encode ( IDDict set ) =
    Dict.keys set
    |> E.list E.int


decoder : D.Decoder ( Set id )
decoder =
    D.list D.int
    |> D.map ( \ids_ -> IDDict ( Dict.fromList ( List.map ( \id -> ( id, () ) ) ids_ ) ) )