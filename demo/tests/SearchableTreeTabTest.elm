module SearchableTreeTabTest exposing (testSuite)

import Expect
import Test exposing (..)
import Tree as T
import SearchableTreeTab as STT
import Regex as RX
import Json.Decode as D


node : T.Node STT.NodeData
node =
    T.Node
        { children = []
        , data = (STT.NodeData "1234" "helloo goodbye")
        }


testSuite =
    describe "SearchableTreeTab tests"
        [ describe "highlightMatches tests"
              [ test "With no search-regex, node text is rendered ordinary" <|
                  \() ->
                      [ STT.Vanilla "helloo goodbye" ]
                          |> Expect.equal (STT.highlightMatches Nothing node)
              , test "Not matching the regex, node text is rendered ordinary" <|
                  \() ->
                      [ STT.Vanilla "helloo goodbye" ]
                          |> Expect.equal (STT.highlightMatches (Just RX.never) node)
              , test "One match at the beginning gets STT.Highlit prefix" <|
                  \() ->
                      [STT.Highlit "hel", STT.Vanilla "loo goodbye" ]
                          |> Expect.equal (STT.highlightMatches (RX.fromString "hel") node)
              , test "One match at the end gets STT.Highlit suffix" <|
                  \() ->
                      [ STT.Vanilla "helloo good", STT.Highlit "bye" ]
                          |> Expect.equal (STT.highlightMatches (RX.fromString "bye") node)
              , test "One match in the middle gets STT.Highlit middle" <|
                  \() ->
                      [ STT.Vanilla "hel", STT.Highlit "loo", STT.Vanilla " goodbye" ]
                          |> Expect.equal (STT.highlightMatches (RX.fromString "lo+") node)
              , test "Multiple matches are all strengthened" <|
                  \() ->
                      [ STT.Vanilla "hell", STT.Highlit "oo", STT.Vanilla " g", STT.Highlit "oo", STT.Vanilla "dbye" ]
                          |> Expect.equal (STT.highlightMatches (RX.fromString "o+") node)
              , test "Multiple matches can be consecutive" <|
                  \() ->
                      [ STT.Vanilla "he", STT.Highlit "ll", STT.Highlit "oo", STT.Vanilla " g", STT.Highlit "oo", STT.Vanilla "dbye" ]
                          |> Expect.equal (STT.highlightMatches (RX.fromString "(o+|l+)") node)
              ]
        , describe "Parsing JSON into NodeData tree/forest tests"
            [ test "JSON of a single node is parsed correctly" <|
                \() ->
                    let
                        json =
                            """{ "uid" : "1234", "content" : "helloo goodbye", "children" : [] }"""

                        parseJsonNode : String -> Result.Result D.Error (T.Node STT.NodeData)
                        parseJsonNode string =
                            D.decodeString STT.jsonNodeDecoder string
                                |> Result.map STT.jsonNodeToTree
                    in
                        (Result.Ok node)
                            |> Expect.equal (parseJsonNode json)
            , test "JSON of a list of nodes is parsed correctly" <|
                \() ->
                    let
                        json =
                            """
                            [ { "uid" : "1", "content" : "forty-two", "children" : [] }
                            , { "uid" : "2"
                              , "content" : "forty-three"
                              , "children" : [
                                    { "uid" : "2.1", "content" : "forty-four", "children" : [] }
                                ]
                              }
                            ]
                            """

                        forest =
                            [ T.Node
                                { data = STT.NodeData "1" "forty-two"
                                , children = []
                                }
                            , T.Node
                                { data = STT.NodeData "2" "forty-three"
                                , children =
                                    [ T.Node
                                        { data = STT.NodeData "2.1" "forty-four"
                                        , children = []
                                        }
                                    ]
                                }
                            ]
                    in
                        forest
                            |> Expect.equal (STT.parseJsonNodes json)
            ]
        ]
