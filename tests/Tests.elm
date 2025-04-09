module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list)
import Json.Decode as D
import Json.Encode as E
import Parser
import Test exposing (..)

import Internal
import Id
import Id.Dict
import Id.CounterDict
import Id.Counter
import Id.List
import Id.CounterList
import Id.Set


type alias Id =
    Id.Id ()


type alias ExpectationTest value =
    ( String, value -> Expectation )


id : Int -> Id.Id a
id value =
    Id.debugId Debug.todo value


idFuzzer : Fuzzer Id
idFuzzer =
    Fuzz.intAtLeast 1
    |> Fuzz.map id


idTest : Test
idTest =
    describe "Id"
    [ idValueTest
        |> toTests 1
        |> concat
    , idValueTest
        |> toFuzzTests ( Fuzz.intAtLeast 1 )
        |> describe "Fuzz"
    ]


idValueTest : List (ExpectationTest Int)
idValueTest =
    [ ( "toString"
      , \value ->
            Id.toString (id value)
            |> Expect.equal (String.fromInt value)
      )
    , ( "parser"
      , \value ->
            Parser.run Id.parser (String.fromInt value)
            |> Expect.equal (Ok (id value))
      )
    , ( "Json encoding and decoding", \value -> testJson Id.encode Id.decoder ( id value ) )
    ]


counterTest : Test
counterTest =
    let
        counter = Id.Counter.new ()
        ( id1, counter1 ) = Id.Counter.newId counter
        ( id2, counter2 ) = Id.Counter.newId counter1
    in
    describe "Id.Counter"
    [ test "Id is at least 1" <| \_ -> Expect.atLeast 1 ( Internal.unpackId id1 )
    , test "Successive Ids are different" <| \_ -> Expect.notEqual id1 id2
    , test "Counter changes 1" <| \_ -> Expect.notEqual counter counter1
    , test "Counter changes 2" <| \_ -> Expect.notEqual counter1 counter2
    ]


dictTest : Test
dictTest =
    let
        dictFuzzer valueFuzzer =
            Fuzz.pair idFuzzer valueFuzzer
            |> Fuzz.list
            |> Fuzz.map Id.Dict.fromList
    in
    describe "Id.Dict"
    [ test "Dict.empty size is 0" <| \_ ->
        Id.Dict.empty
        |> Id.Dict.size
        |> Expect.equal 0
    , test "isEmpty with empty" <| \_ ->
        Id.Dict.empty
        |> Id.Dict.isEmpty
        |> Expect.equal True
    , test "isEmpty with item" <| \_ ->
        Id.Dict.singleton ( id 1 ) ()
        |> Id.Dict.isEmpty
        |> Expect.equal False
    , test "member with match" <| \_ ->
        Id.Dict.singleton ( id 1 ) ()
        |> Id.Dict.member ( id 1 )
        |> Expect.equal True
    , test "member without match" <| \_ ->
        Id.Dict.singleton ( id 1 ) ()
        |> Id.Dict.member ( id 2 )
        |> Expect.equal False
    , test "get with match" <| \_ ->
        Id.Dict.singleton ( id 1 ) "Value"
        |> Id.Dict.get ( id 1 )
        |> Expect.equal ( Just "Value" )
    , test "get without match" <| \_ ->
        Id.Dict.singleton ( id 1 ) "Value"
        |> Id.Dict.get ( id 2 )
        |> Expect.equal Nothing
    , test "insert is same as single" <| \_ ->
        Id.Dict.empty
        |> Id.Dict.insert ( id 1 ) ()
        |> Expect.equal ( Id.Dict.singleton ( id 1 ) () )
    , test "Inserting value with same id multiple times" <| \_ ->
        Id.Dict.empty
        |> Id.Dict.insert ( id 1 ) 1
        |> Id.Dict.insert ( id 1 ) 2
        |> Id.Dict.get ( id 1 )
        |> Expect.equal ( Just 2 )
    , describe "Json" <|
        [ test "Encoding and decoding" <| \_ ->
            let
                dict =
                    Id.Dict.fromList
                    [ ( id 1, "value 1" )
                    , ( id 200, "Wow" )
                    , ( id -1, "Not possible!" )
                    ]
            in
            testJson ( Id.Dict.encode E.string ) ( Id.Dict.decoder D.string ) dict
        , fuzz ( dictFuzzer Fuzz.string ) "Fuzz encoding and decoding" <| \dict ->
            testJson ( Id.Dict.encode E.string ) ( Id.Dict.decoder D.string ) dict
        ]
    ]


setTest : Test
setTest =
    let
        setFuzzer =
            Fuzz.list idFuzzer
            |> Fuzz.map Id.Set.fromList
    in
    describe "Id.Set"
    [ test "Dict.empty size is 0" <| \_ ->
        Id.Set.empty
        |> Id.Set.size
        |> Expect.equal 0
    , test "isEmpty with empty" <| \_ ->
        Id.Set.empty
        |> Id.Set.isEmpty
        |> Expect.equal True
    , test "isEmpty with item" <| \_ ->
        Id.Set.singleton ( id 1 )
        |> Id.Set.isEmpty
        |> Expect.equal False
    , test "member with match" <| \_ ->
        Id.Set.singleton ( id 1 )
        |> Id.Set.member ( id 1 )
        |> Expect.equal True
    , test "member without match" <| \_ ->
        Id.Set.singleton ( id 1 )
        |> Id.Set.member ( id 2 )
        |> Expect.equal False
    , test "insert is same as single" <| \_ ->
        Id.Set.empty
        |> Id.Set.insert ( id 1 )
        |> Expect.equal ( Id.Set.singleton ( id 1 ) )
    , test "Inserting same id multiple times" <| \_ ->
        Id.Set.empty
        |> Id.Set.insert ( id 1 )
        |> Id.Set.insert ( id 1 )
        |> Expect.equal ( Id.Set.singleton ( id 1 ) )
    , describe "Json" <|
        [ test "Encoding and decoding" <| \_ ->
            let
                set =
                    Id.Set.fromList
                    [ id 1
                    , id 200
                    , id -1
                    ]
            in
            testJson Id.Set.encode Id.Set.decoder set
        , fuzz setFuzzer "Fuzz encoding and decoding" <| \set ->
            testJson Id.Set.encode Id.Set.decoder set
        ]
    ]


counterDictTest : Test
counterDictTest =
    let
        counter = Id.Counter.new ()
        empty = Id.CounterDict.empty counter
        fromMaybeIdList = List.foldl addPair empty
        addPair ( maybeId, value ) dict =
            case maybeId of
                Just id_ ->
                    Id.CounterDict.insert id_ value dict
                
                Nothing ->
                    Id.CounterDict.addInto value dict
        dictFuzzer valueFuzzer =
            Fuzz.pair ( Fuzz.maybe idFuzzer ) valueFuzzer
            |> Fuzz.list
            |> Fuzz.map fromMaybeIdList
    in
    describe "Id.CounterDict"
    [ test "Insert increases internal counter when adding higher Id" <| \_ ->
        empty
        |> Id.CounterDict.insert ( id 200 ) ()
        |> Id.CounterDict.add ()
        |> Tuple.first
        |> Internal.unpackId
        |> Expect.atLeast 201
    , test "Initial counter value is below 200 (for the sake of previous test)" <| \_ ->
        empty
        |> Id.CounterDict.add ()
        |> Tuple.first
        |> Internal.unpackId
        |> Expect.atMost 199
    , describe "Json" <|
        [ test "Encoding and decoding" <| \_ ->
            let
                dict =
                    fromMaybeIdList
                    [ ( Nothing, "value 1" )
                    , ( Just ( id 200 ), "Wow" )
                    , ( Just ( id -1 ), "Not possible!" )
                    , ( Nothing, "Another one" )
                    ]
            in
            testJson ( Id.CounterDict.encode E.string ) ( Id.CounterDict.decoder D.string ) dict
        , fuzz ( dictFuzzer Fuzz.string ) "Fuzz encoding and decoding" <| \dict ->
            testJson ( Id.CounterDict.encode E.string ) ( Id.CounterDict.decoder D.string ) dict
        ]
    ]


idListTest : Test
idListTest =
    let
        pairs =
            [ ( id 5, 7 )
            , ( id 3, 69 )
            , ( id 69, 420 )
            ]
        listFuzzer value =
            Fuzz.map2 Tuple.pair idFuzzer value
            |> Fuzz.list
            |> Fuzz.map Id.List.removeDuplicateIds
    in
    describe "Id.List"
    [ test "Insert" <| \_ ->
        Id.List.empty
        |> Id.List.insert ( id 1 ) ()
        |> Expect.equal ( Id.List.singleton ( id 1 ) () )
    , test "Inserts at start" <| \_ ->
        Id.List.empty
        |> Id.List.insert ( id 1 ) ()
        |> Id.List.insert ( id 2 ) ()
        |> Id.List.ids
        |> Expect.equal [ id 2, id 1 ]
    , test "Multiple inserts with same id" <| \_ ->
        Id.List.empty
        |> Id.List.insert ( id 1 ) 5
        |> Id.List.insert ( id 2 ) 13
        |> Id.List.insert ( id 3 ) 420
        |> Id.List.insert ( id 2 ) 69
        |> Expect.equal [ ( id 3, 420 ), ( id 2, 69 ), ( id 1, 5 ) ]
    , test "Insert at 0" <| \_ ->
        Id.List.empty
        |> Id.List.insert ( id 1 ) ()
        |> Id.List.insertAt 0 ( id 2 ) ()
        |> Id.List.ids
        |> Expect.equal [ id 2, id 1 ]
    , test "Insert at 1" <| \_ ->
        Id.List.empty
        |> Id.List.insert ( id 1 ) ()
        |> Id.List.insertAt 1 ( id 2 ) ()
        |> Id.List.ids
        |> Expect.equal [ id 1, id 2 ]
    , describe "removeDuplicateIds"
        [ test "duplicatesGetRemoved" <| \_ ->
            [ ( id 1, () ), ( id 1, () ) ]
            |> Id.List.removeDuplicateIds
            |> Expect.equal [ ( id 1, () ) ]
        , test "order" <| \_ ->
            pairs ++ List.reverse pairs
            |> Id.List.removeDuplicateIds
            |> Expect.equal pairs
        ]
    , describe "Json"
        [ test "Encoding and decoding" <| \_ ->
            testJson ( Id.List.encode E.int ) ( Id.List.decoder D.int ) pairs
        , fuzz ( listFuzzer Fuzz.string ) "Fuzz encoding and decoding" <| \list ->
            testJson ( Id.List.encode E.string ) ( Id.List.decoder D.string ) list
        ]
    ]


counterListTest : Test
counterListTest =
    let
        counter = Id.Counter.new ()
        empty = Id.CounterList.empty counter
        fromMaybeIdList = List.foldl addPair empty
        addPair ( maybeId, value ) dict =
            case maybeId of
                Just id_ ->
                    Id.CounterList.insert id_ value dict
                
                Nothing ->
                    Id.CounterList.addInto value dict
        dictFuzzer valueFuzzer =
            Fuzz.pair ( Fuzz.maybe idFuzzer ) valueFuzzer
            |> Fuzz.list
            |> Fuzz.map fromMaybeIdList
    in
    describe "Id.CounterList"
    [ test "Add" <| \_ ->
        empty
        |> Id.CounterList.add ()
        |> Expect.equal ( Id.CounterList.singleton counter () )
    , test "Adds at start" <| \_ ->
        empty
        |> Id.CounterList.addInto 1
        |> Id.CounterList.addInto 2
        |> Id.CounterList.values
        |> Expect.equal [ 2, 1 ]
    , test "Multiple inserts with same id" <| \_ ->
        empty
        |> Id.CounterList.insert ( id 1 ) 5
        |> Id.CounterList.insert ( id 2 ) 13
        |> Id.CounterList.insert ( id 3 ) 420
        |> Id.CounterList.insert ( id 2 ) 69
        |> Id.CounterList.toList
        |> Expect.equal [ ( id 3, 420 ), ( id 2, 69 ), ( id 1, 5 ) ]
    , test "Add at 0" <| \_ ->
        empty
        |> Id.CounterList.addInto 1
        |> Id.CounterList.addAt 0 2
        |> Tuple.second
        |> Id.CounterList.values
        |> Expect.equal [ 2, 1 ]
    , test "Add at 1" <| \_ ->
        empty
        |> Id.CounterList.addInto 1
        |> Id.CounterList.addInto 1
        |> Id.CounterList.addAt 1 2
        |> Tuple.second
        |> Id.CounterList.values
        |> Expect.equal [ 1, 2, 1 ]
    , describe "fromIdList"
        [ test "duplicatesGetRemoved" <| \_ ->
            [ ( id 1, () ), ( id 1, () ) ]
            |> Id.CounterList.fromIdList counter
            |> Id.CounterList.toList
            |> Expect.equal [ ( id 1, () ) ]
        , test "order" <| \_ ->
            let
                pairs =
                    [ ( id 5, 3 )
                    , ( id 1, 423 )
                    , ( id 7, 3 )
                    , ( id 1, 34 )
                    ]
            in
            pairs ++ List.reverse pairs
            |> Id.CounterList.fromIdList counter
            |> Id.CounterList.toList
            |> Expect.equal ( Id.List.removeDuplicateIds pairs )
        ]
    , test "Insert increases internal counter when adding higher Id" <| \_ ->
        empty
        |> Id.CounterList.insert ( id 200 ) ()
        |> Id.CounterList.add ()
        |> Tuple.first
        |> Internal.unpackId
        |> Expect.atLeast 201
    , test "Initial counter value is below 200 (for the sake of previous test)" <| \_ ->
        empty
        |> Id.CounterList.add ()
        |> Tuple.first
        |> Internal.unpackId
        |> Expect.atMost 199
    , describe "Json" <|
        [ test "Encoding and decoding" <| \_ ->
            let
                dict =
                    fromMaybeIdList
                    [ ( Nothing, "value 1" )
                    , ( Just ( id 200 ), "Wow" )
                    , ( Just ( id 534 ), "Not possible!" )
                    , ( Nothing, "Another one" )
                    ]
            in
            testJson ( Id.CounterList.encode E.string ) ( Id.CounterList.decoder D.string ) dict
        , fuzz ( dictFuzzer Fuzz.string ) "Fuzz encoding and decoding" <| \dict ->
            testJson ( Id.CounterList.encode E.string ) ( Id.CounterList.decoder D.string ) dict
        ]
    ]


testJson : ( value -> E.Value ) -> D.Decoder value -> value -> Expectation
testJson encode decoder value =
    encode value
    |> D.decodeValue decoder
    |> Expect.equal ( Ok value )


jsonTest : (value -> E.Value) -> D.Decoder value -> value -> E.Value -> List Test
jsonTest encode decoder value encodedValue =
    jsonExpectations encode decoder
    |> toTests ( value, encodedValue )


jsonExpectations : (value -> E.Value) -> D.Decoder value -> List (ExpectationTest ( value, E.Value ))
jsonExpectations encode decoder =
    [ ( "encode", \( value, encodedValue ) -> testEncoder encode value encodedValue )
    , ( "decode", \( value, encodedValue ) -> testDecoder decoder value encodedValue )
    ]


testEncoder : (value -> E.Value) -> value -> E.Value -> Expectation
testEncoder encode value encodedValue =
    encode value
        |> jsonEqual encodedValue


testDecoder : D.Decoder value -> value -> E.Value -> Expectation
testDecoder decoder value encodedValue =
    D.decodeValue decoder encodedValue
        |> Expect.equal (Ok value)


toTest : value -> ExpectationTest value -> Test
toTest value ( name, func ) =
    test name (\_ -> func value)


toTests : value -> List (ExpectationTest value) -> List Test
toTests value =
    List.map (toTest value)


toFuzz : Fuzzer value -> ( String, value -> Expectation ) -> Test
toFuzz fuzzer ( name, func ) =
    fuzz fuzzer name func


toFuzzTests : Fuzzer value -> List ( String, value -> Expectation ) -> List Test
toFuzzTests fuzzer =
    List.map (toFuzz fuzzer)

jsonEqual : E.Value -> E.Value -> Expectation
jsonEqual val1 val2 =
    -- if val1 == val2 then
    --     Expect.equal val1 val2
    -- else
        Expect.equal ( E.encode 0 val1 ) ( E.encode 0 val2 )
