module Internal exposing (..)

import Dict

import Json.Decode as D

type ID a
    = ID Int

type Counter id
    = IDCounter Int

type Dict counter id value
    = IDDict counter ( Dict.Dict Int value )


unpack : ID a -> Int
unpack (ID id) =
    id


newCounter : a -> Counter (ID a)
newCounter _ =
    IDCounter 0


newID : Counter (ID a) -> ( ID a, Counter (ID a) )
newID (IDCounter nextID) =
    ( ID nextID, IDCounter (nextID + 1) )

