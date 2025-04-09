module Id exposing
    ( Id
    , Counter
    , Dict
    , Set
    , CounterDict
    , Dict_
    , List
    , CounterList
    , toString
    , debugId
    , parser
    , encode
    , decoder
    )

{-|
Module in which the base Id type is defined

# Types
@docs Id, Counter, Dict, Set, CounterDict, Dict_, List, CounterList

# Functions
@docs toString, debugId

# Parser
@docs parser

# Json
@docs encode, decoder
-}

import Internal exposing(Id(..))

import Id.Counter
import Id.Dict
import Id.Set
import Id.CounterDict
import Id.List
import Id.CounterList

import Json.Encode as E
import Json.Decode as D
import Parser


{-| The basis for making a Id type, has one generic type argument which should be an internal type to the module the Id type is defined in.

    module UserId exposing(Id)

    import Id

    type alias Id = Id.Id IdInternal

    type IdInternal = IdInternal
-}
type alias Id a = Internal.Id a


{-| Alias for [`Id.Counter.Counter`](Id.Counter#Counter) -}
type alias Counter id = Id.Counter.Counter id


{-| Alias for [`Id.Dict.Dict`](Id.Dict#Dict) -}
type alias Dict id value = Id.Dict.Dict id value


{-| Alias for [`Id.Set.Set`](Id.Set#Set) -}
type alias Set id = Id.Set.Set id

{-| Alias for [`Id.CounterDict.CounterDict`](Id.CounterDict#CounterDict) -}
type alias CounterDict id value = Id.CounterDict.CounterDict id value

{-| Alias for [`Id.Dict.Dict_`](Id.Dict#Dict_) -}
type alias Dict_ counter id value = Id.Dict.Dict_ counter id value

{-| Alias for ['Id.List.List'](Id.List#List) -}
type alias List id value = Id.List.List id value

{-| Alias for ['Id.CounterList.CounterList'](Id.CounterList#CounterList) -}
type alias CounterList id value = Id.CounterList.CounterList id value


{-| Converts an Id into a string representing the number inside the Id -}
toString : Id a -> String
toString =
    Internal.idToString


{-| Create an Id with an arbitrary value for debugging purposes. Requires 'Debug.todo' as a first argument -}
debugId : (String -> Never) -> Int -> Id a
debugId _ num =
    Id num


{-| Parser for 'Id' -}
parser : Parser.Parser ( Id a )
parser =
    Internal.idParser


{-| Encode `Id` to a JSON value -}
encode : Id a -> E.Value
encode (Id id) =
    E.int id


{-| JSON Decoder for `Id` -}
decoder : D.Decoder ( Id a )
decoder =
    D.map Id D.int
