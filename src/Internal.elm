module Internal exposing (..)

import Dict

type Id a
    = Id Int

type Counter id
    = IdCounter Int

type Dict noCounter id value
    = WithoutCounter noCounter ( Dict.Dict Int value )
    | WithCounter ( Counter id ) ( Dict.Dict Int value )


unpack : Id a -> Int
unpack (Id id) =
    id


accountForId : Id a -> Counter (Id a) -> Counter (Id a)
accountForId id (IdCounter nextId as counter) =
    if unpack id >= nextId then
        IdCounter ( unpack id + 10 )
    else
        counter


unpackDict : Dict noCounter id value -> Dict.Dict Int value
unpackDict dict =
    case dict of
        WithoutCounter _ innerDict ->
            innerDict
        
        WithCounter _ innerDict ->
            innerDict


packDict : Dict noCounter id b -> Dict.Dict Int a -> Dict noCounter id a
packDict dict inner =
    case dict of
        WithoutCounter value _ ->
            WithoutCounter value inner

        WithCounter counter _ ->
            WithCounter counter inner


mapDict : ( Dict.Dict Int a -> Dict.Dict Int b ) -> Dict noCounter id a -> Dict noCounter id b
mapDict func dict =
    case dict of
        WithoutCounter value inner ->
            WithoutCounter value ( func inner )

        WithCounter counter inner ->
            WithCounter counter ( func inner )
