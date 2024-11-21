module ID.CounterDict exposing
    ( CounterDict
    , empty
    , fromList
    , insert
    , encode
    , decoder
    )

{-|
@docs CounterDict

@docs empty, fromList
@docs insert

# Json
@docs encode, decoder
-}

import Internal exposing (ID(..), Dict(..))
import Dict

import Json.Encode as E
import Json.Decode as D
import Internal exposing (Counter(..))



{-| A dictionary with counter -}
type alias CounterDict id value = Dict ( Internal.Counter id ) id value


{-| Create a empty `CounterDict` -}
empty : a -> CounterDict (ID a) value
empty internal =
    IDDict ( Internal.newCounter internal ) Dict.empty


{-| Create a new `CounterDict` from a list of values and assign an [`ID`](ID#ID) to every value -}
fromList : a -> List value -> CounterDict (ID a) value
fromList _ list =
    IDDict ( IDCounter ( List.length list ) ) ( Dict.fromList ( List.indexedMap Tuple.pair list ) )


{-| Insert a value with a new [`ID`](ID#ID) into the Dictionary -}
insert : value -> CounterDict (ID a) value -> ( ID a, CounterDict (ID a) value )
insert value ( IDDict counter dict ) =
  let
    ( ID idValue as id, newCounter ) = Internal.newID counter
  in
  ( id, IDDict newCounter ( Dict.insert idValue value dict ) )


{-| Encode `CounterDict` to a JSON value -}
encode : ( value -> D.Value ) -> CounterDict id value -> E.Value
encode encodeValue ( IDDict ( IDCounter counter ) _ as dict ) =
    Internal.encodeDictFields encodeValue dict
    |> (::) ( "counter", E.int counter )
    |> E.object


{-| JSON Decoder for `SetCounterDict -}
decoder : D.Decoder value -> D.Decoder ( CounterDict id value )
decoder =
    Internal.dictDecoder ( D.field "counter" ( D.map IDCounter D.int ) )

