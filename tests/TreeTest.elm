module TreeTest exposing (testSuite)

import Expect
import Test exposing (..)
import Tree as T

singleNodeTree : T.Node String
singleNodeTree =
    T.Node { children = [], data = "single" }


parentChildTree : T.Node String
parentChildTree =
    T.Node
      { children = [ singleNodeTree ]
      , data = "parent"
      }


twin1Tree : T.Node String
twin1Tree =
    T.Node
      { children = []
      , data = "twin1"
      }


twin2Tree : T.Node String
twin2Tree =
    T.Node
      { children = []
      , data = "twin2"
      }


parentTwoChildrenTree : T.Node String
parentTwoChildrenTree =
    T.Node
      { children =
          [ twin1Tree
          , twin2Tree
          ]
      , data = "parent2"
      }


moreComplexTree : T.Node String
moreComplexTree =
    T.Node
      { children =
          [ parentChildTree
          , parentTwoChildrenTree
          ]
      , data = "complex"
      }


dataStringListOf : T.Node String -> List String
dataStringListOf tree =
    tree
        |> T.listTreeNodes
        |> List.map T.dataOf


dataStringListOfForest : List (T.Node String) -> List String
dataStringListOfForest forest =
    forest
        |> List.map dataStringListOf
        |> List.concat


testSuite =
    describe "tree test cases"
        [ describe "defaultFoldOptions cases"
            [ test "defaultFoldOptions produce the initialFoldState as result" <|
                \() ->
                    "initial"
                        |> Expect.equal (T.foldTree T.defaultFoldOptions "initial" moreComplexTree)
            ]
        , describe "height test cases"
            [ test "tree of single node has height of 1" <|
                \() ->
                    1
                        |> Expect.equal (T.treeHeight singleNodeTree)
            , test "tree with a parent and child node has height of 2" <|
                \() ->
                    2
                        |> Expect.equal (T.treeHeight parentChildTree)
            , test "tree with a parent and two children has height of 2" <|
                \() ->
                    2
                        |> Expect.equal (T.treeHeight parentTwoChildrenTree)
            , test "the height of a tree is the max height of its children plus one" <|
                \() ->
                    3
                        |> Expect.equal (T.treeHeight moreComplexTree)
            ]
        , describe "forest height test cases"
            [ test "height of a forest with no tree is zero" <|
                \() ->
                    0
                        |> Expect.equal (T.forestHeight [])
            , test "height of a forest with single tree is the height of the tree" <|
                \() ->
                    (T.treeHeight moreComplexTree)
                        |> Expect.equal (T.forestHeight [ moreComplexTree ])
            , test "height of a forest with more trees is the height of the tallest one" <|
                \() ->
                    (T.treeHeight moreComplexTree)
                        |> Expect.equal (T.forestHeight [ moreComplexTree, parentTwoChildrenTree ])
            ]
        , describe "joinTree tests"
            [ test "join of a tree of single node produces the text of that node" <|
                \() ->
                    "single"
                        |> Expect.equal (T.joinTree T.dataOf ", " singleNodeTree)
            , test "join of a tree with parent and child node produces the enumerated texts of those two nodes, in descending order" <|
                \() ->
                    "parent, single"
                        |> Expect.equal (T.joinTree T.dataOf ", " parentChildTree)
            , test "join of a tree with parent and two children produces the enumerated texts of those three nodes, in descending and left->right order" <|
                \() ->
                    "parent2, twin1, twin2"
                        |> Expect.equal (T.joinTree T.dataOf ", " parentTwoChildrenTree)
            , test "join of a more complex tree produces the enumerated texts of those three nodes, in descending and left->right order" <|
                \() ->
                    "complex, parent, single, parent2, twin1, twin2"
                        |> Expect.equal (T.joinTree T.dataOf ", " moreComplexTree)
            ]
        , describe "joinForest tests"
            [ test "join of an empty forest produces empty string" <|
                \() ->
                    ""
                        |> Expect.equal (T.joinForest T.dataOf ", " [])
            , test "join of a forest with a single tree produces the join of that tree" <|
                \() ->
                    "parent, single"
                        |> Expect.equal (T.joinForest T.dataOf ", " [ parentChildTree ])
            , test "join of a forest with more trees produces the join of those threes, in left->right order" <|
                \() ->
                    "single, parent, single"
                        |> Expect.equal (T.joinForest T.dataOf ", " [ singleNodeTree, parentChildTree ])
            ]
        , describe "listTreeNodes tests"
            [ test "listing a tree of a single node produces that single node" <|
                \() ->
                    [ singleNodeTree ]
                        |> Expect.equal (T.listTreeNodes singleNodeTree)
            , test "listing a more complex tree produces the list of its nodes, in descending and left->right order" <|
                \() ->
                    [ moreComplexTree, parentChildTree, singleNodeTree, parentTwoChildrenTree, twin1Tree, twin2Tree ]
                        |> Expect.equal (T.listTreeNodes moreComplexTree)
            ]
        , describe "listForestNodes tests"
            [ test "listing a forest empty of trees produces an empty list" <|
                \() ->
                    []
                        |> Expect.equal (T.listForestNodes [])
            , test "listing a forest produces the concatenated listing of the trees within, in left->right order" <|
                \() ->
                    [ parentTwoChildrenTree, twin1Tree, twin2Tree, parentChildTree, singleNodeTree ]
                        |> Expect.equal (T.listForestNodes [ parentTwoChildrenTree, parentChildTree ])
            ]
        , describe "listAnnotatedTreeNodes tests"
            [ test "annotated listing a tree of a single node produces (0, 0, single node)" <|
                \() ->
                    [ T.AnnotatedNode 0 0 singleNodeTree ]
                        |> Expect.equal (T.listAnnotatedTreeNodes singleNodeTree)
            , test "annotated listing a more complex tree produces an annotated list of nodes, in descending and left-right order" <|
                \() ->
                    [ T.AnnotatedNode 0 0 moreComplexTree
                    , T.AnnotatedNode 1 1 parentChildTree
                    , T.AnnotatedNode 2 2 singleNodeTree
                    , T.AnnotatedNode 3 1 parentTwoChildrenTree
                    , T.AnnotatedNode 4 2 twin1Tree
                    , T.AnnotatedNode 5 2 twin2Tree
                    ]
                        |> Expect.equal (T.listAnnotatedTreeNodes moreComplexTree)
            ]
        , describe "listAnnotatedForestNodes tests" <|
            [ test "annotated listing a forest empty of trees produces an empty list" <|
                \() ->
                    []
                        |> Expect.equal (T.listAnnotatedForestNodes [])
            , test "annotated listing a forest produces the concatenated listing of the trees withing, in left->right order" <|
                \() ->
                    [ T.AnnotatedNode 0 0 parentTwoChildrenTree
                    , T.AnnotatedNode 1 1 twin1Tree
                    , T.AnnotatedNode 2 1 twin2Tree
                    , T.AnnotatedNode 3 0 parentChildTree
                    , T.AnnotatedNode 4 1 singleNodeTree
                    ]
                        |> Expect.equal (T.listAnnotatedForestNodes [ parentTwoChildrenTree, parentChildTree ])
            ]
        , describe "updateTreeData tests"
            [ test "single list node gets updated, if instructed" <|
                \() ->
                    [ "SINGLE" ]
                        |> Expect.equal (T.updateTreeData (\_ -> True) (String.toUpper) singleNodeTree |> dataStringListOf)
            , test "single list node does not get updated, if not instructed" <|
                \() ->
                    [ "single" ]
                        |> Expect.equal (T.updateTreeData (\_ -> False) (String.toUpper) singleNodeTree |> dataStringListOf)
            , test "one node can be singled out for updating" <|
                \() ->
                    [ "complex"
                    , "parent"
                    , "SINGLE"
                    , "parent2"
                    , "twin1"
                    , "twin2"
                    ]
                        |> Expect.equal (T.updateTreeData (\s -> String.startsWith "s" s) (String.toUpper) moreComplexTree |> dataStringListOf)
            ]
        , describe "updateForestData tests"
            [ test "nodes can be controlled for updating even for a forest" <|
                \() ->
                    [ "SINGLE"
                    , "complex"
                    , "parent"
                    , "SINGLE"
                    , "parent2"
                    , "twin1"
                    , "twin2"
                    ]
                        |> Expect.equal (T.updateForestData (\s -> String.startsWith "s" s) (String.toUpper) [singleNodeTree, moreComplexTree] |> dataStringListOfForest)

            ]
        ]
