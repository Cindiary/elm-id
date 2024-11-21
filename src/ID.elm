module ID exposing
    ( ID
    , Counter
    , Dict
    , Set
    , getIDString
    , debugID
    , encode
    , decoder
    )

import Internal exposing(ID(..))

import ID.Dict
import ID.Set
import ID.Counter

import Json.Encode as E
import Json.Decode as D


type alias ID a = Internal.ID a
type alias Counter id = ID.Counter.Counter id
type alias Dict id value = ID.Dict.Dict id value
type alias Set id = ID.Set.Set id


getIDString : ID a -> String
getIDString ( ID id ) =
    String.fromInt id


debugID : (String -> Never) -> Int -> ID a
debugID _ num =
    ID num


encode : ID a -> E.Value
encode (ID id) =
    E.int id


decoder : D.Decoder ( ID a )
decoder =
    D.map ID D.int
