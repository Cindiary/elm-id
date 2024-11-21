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


encodeDictFields : ( value -> D.Value ) -> Dict counter id value -> List ( String, D.Value )
encodeDictFields encodeValue ( IDDict _ dict ) =
    Dict.toList dict
    |> List.map ( Tuple.mapBoth String.fromInt encodeValue )


dictDecoder : D.Decoder counter -> D.Decoder value -> D.Decoder ( Dict counter id value )
dictDecoder counterDecoder valueDecoder =
    let
        fromPairs pairs =
            case pairs of
                ( key, value ) :: ls ->
                    case String.toInt key of
                        Just id ->
                            D.map
                            ( Dict.insert id value )
                            ( fromPairs ls )

                        Nothing ->
                            D.fail ( "Invalid id: \"" ++ key ++ "\"" )
                
                [] ->
                    D.succeed Dict.empty
    in
    D.keyValuePairs valueDecoder
    |> D.andThen fromPairs
    |> D.map2 IDDict counterDecoder

