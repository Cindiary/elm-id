module Internal exposing 
    (Id(..), Counter(..)
    , Dict(..)
    , unpackId, accountForId
    , idToString, idParser
    , unpack, getCounter, pack, map
    )

import Dict
import Parser exposing((|=), (|.))

type Id a
    = Id Int

type Counter id
    = IdCounter Int


type Dict noCounter id value
    = WithCounter Int ( Dict.Dict Int value )
    | WithoutCounter noCounter ( Dict.Dict Int value )


unpackId : Id a -> Int
unpackId (Id id) =
    id


idToString : Id a -> String
idToString (Id id) =
    String.fromInt id


idParser : Parser.Parser ( Id a )
idParser =
    Parser.map Id Parser.int


accountForId : Id id -> Int -> Int
accountForId id counterValue =
    if unpackId id >= counterValue then
        unpackId id + 10
    else
        counterValue


unpack : Dict noCounter id value -> Dict.Dict Int value
unpack dict =
    case dict of
        WithCounter _ inner ->
            inner
        
        WithoutCounter _ inner ->
            inner


getCounter : Dict Never id inner -> Int
getCounter dict =
    case dict of
        WithCounter counter _ ->
            counter
        
        WithoutCounter _ _ ->
            --This should be impossible, so just recurse
            getCounter dict


pack : Dict noCounter id b -> Dict.Dict Int value -> Dict noCounter id value
pack structure inner =
    case structure of
        WithCounter counter _ ->
            WithCounter counter inner
        
        WithoutCounter noCounter _ ->
            WithoutCounter noCounter inner


map : ( Dict.Dict Int a -> Dict.Dict Int b ) -> Dict noCounter id a -> Dict noCounter id b
map func dict =
    case dict of
        WithCounter counter inner ->
            WithCounter counter ( func inner )
        
        WithoutCounter noCounter inner ->
            WithoutCounter noCounter ( func inner )
