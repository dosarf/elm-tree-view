module RecursiveJsonTreeTabTest exposing (testSuite)

import Expect
import Test exposing (..)
import Json.Decode as D
import Tree as T
import RecursiveJsonTreeTab as R

singleCommentJson : String
singleCommentJson =
    """{  "author": "Jane Doe",
          "message": "I don't like this",
          "uid": ["janedoe-thread-123", 0],
          "responses": []
        }
    """

singleComment : R.Comment
singleComment =
    R.Comment
        "Jane Doe"
        "I don't like this"
        ("janedoe-thread-123", 0)
        ( R.Responses [] )

singleCommentTree : T.Node R.CommentData
singleCommentTree =
    T.Node
        { data =
            R.CommentData
                "Jane Doe"
                "I don't like this"
                ("janedoe-thread-123", 0)
        , children = []
        }

complexCommentHistoryJson : String
complexCommentHistoryJson =
    """{  "author": "Jane Doe",
          "message": "I don't like this",
          "uid": ["janedoe-thread-123", 0],
          "responses": [
            { "author": "John Deer",
              "message": "What is it exactly you don't like?",
              "uid": ["janedoe-thread-123", 1],
              "responses" : [ {
                  "author": "Jane Doe",
                  "message": "None of it",
                  "uid": ["janedoe-thread-123", 2],
                  "responses": []
                }
              ]
            },
            { "author": "Jake Steer",
              "message": "Me neither",
              "uid": ["janedoe-thread-123", 3],
              "responses": []
            }
          ]
        }
    """

complexCommentHistory : R.Comment
complexCommentHistory =
    R.Comment
        "Jane Doe"
        "I don't like this"
        ("janedoe-thread-123", 0)
        ( R.Responses
            [ R.Comment
                "John Deer"
                "What is it exactly you don't like?"
                ("janedoe-thread-123", 1)
                ( R.Responses
                    [ R.Comment
                        "Jane Doe"
                        "None of it"
                        ("janedoe-thread-123", 2)
                        ( R.Responses [] )
                    ]
                )
            , R.Comment
                "Jake Steer"
                "Me neither"
                ("janedoe-thread-123", 3)
                ( R.Responses [] )
            ]
        )

complexCommentTree : T.Node R.CommentData
complexCommentTree =
    T.Node
        { data =
            R.CommentData
                "Jane Doe"
                "I don't like this"
                ("janedoe-thread-123", 0)
        , children =
            [ T.Node
                  { data =
                      R.CommentData
                          "John Deer"
                          "What is it exactly you don't like?"
                          ("janedoe-thread-123", 1)
                  , children =
                      [ T.Node
                          { data =
                                R.CommentData
                                "Jane Doe"
                                "None of it"
                                ("janedoe-thread-123", 2)
                          , children = []
                          }
                      ]
                  }
            , T.Node
                  { data =
                      R.CommentData
                      "Jake Steer"
                      "Me neither"
                      ("janedoe-thread-123", 3)
                  , children = []
                  }
            ]
        }


testSuite =
    describe "Tests/practice code for conversions within RecursiveJsonTreeTab"
      [ describe "JSON -> R.Comment decoding practice code"
          [ test "Simple JSON content without recursion is correctly decoded" <|
              \() ->
                  Result.Ok singleComment
                      |> Expect.equal (D.decodeString R.commentDecoder singleCommentJson)
          , test "Recursive JSON content is correctly decoded" <|
              \() ->
                  Result.Ok complexCommentHistory
                      |> Expect.equal (D.decodeString R.commentDecoder complexCommentHistoryJson)
          ]
      , describe "R.Comment -> T.Node CommentData conversion tests"
          [ test "Simple comment node is converted into a tree correctly" <|
              \() ->
                  singleCommentTree
                      |> Expect.equal (R.commentToTree singleComment)
          , test "Recursive comment node is converted into a tree correctly" <|
              \() ->
                  complexCommentTree
                      |> Expect.equal (R.commentToTree complexCommentHistory)
          ]
      ]
