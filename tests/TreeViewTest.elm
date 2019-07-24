module TreeViewTest exposing (testSuite)

import Expect
import Test exposing (..)
import Tree as T
import TreeView as TV

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

configuration : TV.Configuration String String
configuration =
    let
        nodeText : T.Node String -> String
        nodeText node =
            T.dataOf node
    in
        TV.Configuration (nodeText >> TV.NodeUid) nodeText TV.defaultCssClasses

initTreeViewModel : List (T.Node String) -> TV.Model String String
initTreeViewModel nodes =
    TV.initializeModel configuration nodes

parentChildTreeAnnotatedNodes : List (T.AnnotatedNode String)
parentChildTreeAnnotatedNodes =
    T.listAnnotatedTreeNodes parentChildTree

moreComplexTreeAnnotatedNodes : List (T.AnnotatedNode String)
moreComplexTreeAnnotatedNodes =
    T.listAnnotatedTreeNodes moreComplexTree


testSuite =
    describe "tree-view test cases"
        [ describe "updateExpandedStateOf tests"
            [ test "with all nodes expanded, all nodes are visible" <|
                \() ->
                    let
                        model = initTreeViewModel [ parentChildTree ]
                    in
                        parentChildTreeAnnotatedNodes
                            |> Expect.equal (TV.getVisibleAnnotatedNodes model)
            , test "with a parent node collapsed, child nodes are invisible" <|
                \() ->
                    let
                        model = initTreeViewModel [ parentChildTree ]
                            |> TV.updateExpandedStateOf (TV.NodeUid "parent") False
                    in
                        [ T.AnnotatedNode 0 0 parentChildTree ]
                            |> Expect.equal (TV.getVisibleAnnotatedNodes model)
            , test "next sibling (and its children) of a collapsed node are visible, if expanded" <|
                \() ->
                    let
                        model = initTreeViewModel [ moreComplexTree ]
                            |> TV.updateExpandedStateOf (TV.NodeUid "parent") False
                    in
                        [ T.AnnotatedNode 0 0 moreComplexTree
                        , T.AnnotatedNode 1 1 parentChildTree
                        , T.AnnotatedNode 3 1 parentTwoChildrenTree
                        , T.AnnotatedNode 4 2 twin1Tree
                        , T.AnnotatedNode 5 2 twin2Tree
                        ]
                            |> Expect.equal (TV.getVisibleAnnotatedNodes model)
              , test "next sibling (and its children) of a collapsed node are invisible, if collapsed" <|
                  \() ->
                      let
                          model = initTreeViewModel [ moreComplexTree ]
                              |> TV.updateExpandedStateOf (TV.NodeUid "parent") False
                              |> TV.updateExpandedStateOf (TV.NodeUid "parent2") False
                      in
                          [ T.AnnotatedNode 0 0 moreComplexTree
                          , T.AnnotatedNode 1 1 parentChildTree
                          , T.AnnotatedNode 3 1 parentTwoChildrenTree
                          ]
                              |> Expect.equal (TV.getVisibleAnnotatedNodes model)
              , test "with root node collapsed, nothing else is visible" <|
                  \() ->
                      let
                          model = initTreeViewModel [ moreComplexTree ]
                              |> TV.updateExpandedStateOf (TV.NodeUid "complex") False
                      in
                          [ T.AnnotatedNode 0 0 moreComplexTree
                          ]
                              |> Expect.equal (TV.getVisibleAnnotatedNodes model)
            ]
        , describe "expandAll tests"
            [ test "expandAll expands all nodes" <|
                \() ->
                    let
                        model = initTreeViewModel [ moreComplexTree ]
                            |> TV.updateExpandedStateOf (TV.NodeUid "parent") False
                            |> TV.updateExpandedStateOf (TV.NodeUid "parent2") False
                            |> TV.expandAll
                    in
                        moreComplexTreeAnnotatedNodes
                            |> Expect.equal (TV.getVisibleAnnotatedNodes model)
            ]
        , describe "collapseAll tests"
            [ test "collapseAll collapses all (non-leaf) nodes" <|
                \() ->
                    let
                        model = initTreeViewModel [ moreComplexTree ]
                    in
                        [ T.AnnotatedNode 0 0 moreComplexTree ]
                            |> Expect.equal (TV.collapseAll model |> TV.getVisibleAnnotatedNodes)
            ]
        ]
