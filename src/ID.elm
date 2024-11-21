module ID exposing
    ( ID
    , Counter
    , Dict
    , Set
    , CounterDict
    , toString
    , debugID
    , encode
    , decoder
    )

{-|
Module in which the base ID type is defined

# Types
@docs ID, Counter, Dict, Set, CounterDict

# Functions
@docs toString, debugID

# Json
@docs encode, decoder
-}

import Internal exposing(ID(..))

import ID.Counter
import ID.Dict
import ID.Set
import ID.CounterDict

import Json.Encode as E
import Json.Decode as D


{-| The basis for making a ID type, has one generic type argument which should be an internal type to the module the ID type is defined in.

    module UserID exposing(ID)

    import ID

    type alias ID = ID.ID IDInternal

    type IDInternal = IDInternal
-}
type alias ID a = Internal.ID a


{-| Alias for [`ID.Counter.Counter`](ID.Counter#Counter) -}
type alias Counter id = ID.Counter.Counter id


{-| Alias for [`ID.Dict.Dict`](ID.Dict#Dict) -}
type alias Dict id value = ID.Dict.Dict id value


{-| Alias for [`ID.Set.Set`](ID.Set#Set) -}
type alias Set id = ID.Set.Set id

{-| Alias for [`ID.CounterDict.CounterDict`](ID.CounterDict#CounterDict) -}
type alias CounterDict id value = ID.CounterDict.CounterDict id value


{-| Converts an ID into a string representing the number inside the ID -}
toString : ID a -> String
toString ( ID id ) =
    String.fromInt id


{-| Create an ID with an arbitrary value for debugging purposes. Requires 'Debug.todo' as a first argument -}
debugID : (String -> Never) -> Int -> ID a
debugID _ num =
    ID num


{-| Encode `ID` to a JSON value -}
encode : ID a -> E.Value
encode (ID id) =
    E.int id


{-| JSON Decoder for `ID` -}
decoder : D.Decoder ( ID a )
decoder =
    D.map ID D.int
