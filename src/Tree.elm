module Tree exposing
  ( Node(..)
  , dataOf, childrenOf
  , FoldOptions, defaultFoldOptions, foldTree, foldForest
  , treeHeight, forestHeight
  , joinTree, joinForest
  , listTreeNodes, listForestNodes
  , AnnotatedNode, listAnnotatedTreeNodes, listAnnotatedForestNodes)

{-| A module containing a tree (model) facility + handy functions. This tree
is _not_ a search tree (e.g. binary tree), it's for representing hierarchical
information to users (e.g. folder-file structure, document structure, etc).

@docs Node

## Utility functions
@docs treeHeight, forestHeight, joinTree, joinForest, AnnotatedNode, listAnnotatedTreeNodes, listAnnotatedForestNodes

## Getting stuff out of nodes

@docs childrenOf, dataOf

## More utility functions

@docs listTreeNodes, listForestNodes

## Folding tree structures

@docs FoldOptions, defaultFoldOptions, foldTree, foldForest
-}

{-| A tree node carrying some data. Can have children nodes.

Create a node like:

    type alias MyData = { .. }

    Node { data = MyData .., children = [ .. ] }
-}
type Node d =
    Node
      { data : d
      , children : List (Node d)
      }

{-| Retrieves the data stored within a node.
-}
dataOf : Node d -> d
dataOf n =
    case n of
        Node node ->
            node.data

{-| Retrieves the list of children of a node.
-}
childrenOf : Node d -> List (Node d)
childrenOf n =
    case n of
        Node node ->
            node.children

{-| Fold options to use with [`foldTree`](#foldTree) or [`foldForest`](#foldForest).

Think of the first argument to a `List.foldl` invocation (`(a -> b -> b)`), which is a function
to fold the value of a list item with some previously obtained value.

In the case of folding an entire tree, more aggregator fold functions are needed.

Implement these aggregator functions as if they had the following declarations:

### preFoldingThunk

    preFoldingThunk : foldState -> Node d -> foldState
    preFoldingThunk foldStateFromParent node =
        ...

Function `FoldOptions.preFoldingThunk` is used just before visiting the children
of a node, and its first argument will be either
* the fold state handed down from the parent node
* or the initial fold fold state given to a root [`foldTree`](#foldTree) invocation.

### postFoldingThunk

    postFoldingThunk : foldState -> Node d -> foldState -> foldState
    postFoldingThunk foldStateFromParent node previousFoldState =
        ...

Function `FoldOptions.postFoldingThunk` is used after visiting all children of a node, just before
leaving the node.

Its first argument will be the same value given to `FoldOptions.preFoldingThunk` earlier
(as a convenience).

The second argument will be the node we are about to leave.

The third argument will be either
* the fold state calculated by `FoldOptions.preFoldingThunk` if there are no child nodes of this node,
* or the fold state obtained by processing the last child node.

### childrenFoldingThunk

    childrenFoldingThunk : foldState -> Node d -> foldState -> foldState
    childrenFoldingThunk previousFoldState node nodeFoldState =
        ...

Function `FoldOptions.childrenFoldingThunk` is used during visiting the children nodes of a node, or when
folding a list of nodes by invoking [`foldForest`](#foldForest).

Its first argument will be
* the fold state calculated by `FoldOptions.preFoldingThunk` if this is the first child node of a parent node,
* or the initial fold state given to the root `foldForest` invocation if the first node in the list of nodes,
* or the fold state obtained by processing the previous sibling node (= the result of `FoldOptions.postFoldingThunk`
  produced by the `foldTree` invocation on the previous sibling node).

The second argument will be the node that just has been visited.

The third argument will be the fold value obtained from visiting that node.

### Fold states example

For a simple tree with root node `a`,

    a
    |
    +- b
    +- c

the following fold states will be calculated for an invocation of `foldTree myFoldOptions initialFoldState a`:

* `fS1  = preFoldingThunk initialFoldState a`
* `fS2  = preFoldingThunk fS1 b`
* `fS3  = postFoldingThunk fS1 b fS2`
* `fS4  = childrenFoldingThunk fS1 b fS3`
* `fS5  = preFoldingThunk fS4 c`
* `fS6  = postFoldingThunk fS4 c fS5`
* `fS7  = childrenFoldingThunk fS4 c fS6`
* `fS8  = postFoldingThunk initialFoldState a fS7`

and return eventually with fold state value `fS8`.
-}
type alias FoldOptions d foldState =
  { preFoldingThunk : foldState -> Node d  -> foldState
  , postFoldingThunk : foldState -> Node d -> foldState -> foldState
  , childrenFoldingThunk : foldState -> Node d -> foldState -> foldState
  }

{-| Default fold options. On the off chance the default implementation of some
fold state functions suits you, you may re-use them specifying only what you
need:

    myFoldOptions =
      { defaultFoldOptions
      | preFoldingThunk = \foldStateFromParent node -> ..
      }

The default fold state function implementations are:

* `preFoldingThunk = \foldStateFromParent node -> foldStateFromParent`
* `postFoldingThunk = \foldStateFromParent node previousFoldState -> previousFoldState`
* `childrenFoldingThunk = \previousFoldState node nodeFoldState -> nodeFoldState`

essentially, the initial fold state value will be handed over, from calculation to calculation,
and returned as the final fold state, without change.
-}
defaultFoldOptions : FoldOptions d foldState
defaultFoldOptions =
    { preFoldingThunk = \foldStateFromParent node -> foldStateFromParent
    , postFoldingThunk = \foldStateFromParent node previousFoldState -> previousFoldState
    , childrenFoldingThunk = \previousFoldState node nodeFoldState -> nodeFoldState
    }

{-| Folds a tree, similar to `List.foldl`. Children of a node are visited from
left to right. Specifics of folding / aggregating are controlled by fold options,
see [`FoldOptions`](#FoldOptions) and [`defaultFoldOptions`](#defaultFoldOptions).
-}
foldTree : FoldOptions d foldState -> foldState -> Node d -> foldState
foldTree foldOptions initialFoldState node =
    let
        preFoldingValue = foldOptions.preFoldingThunk initialFoldState node
        childrenFoldingValue = foldForest foldOptions preFoldingValue <| childrenOf node
    in
        foldOptions.postFoldingThunk initialFoldState node childrenFoldingValue

{-| Folds a forest (list of trees), visiting trees from left to right of the list.
-}
foldForest : FoldOptions d foldState -> foldState -> List (Node d) -> foldState
foldForest foldOptions initialFoldState nodes =
    let
        childrenFoldingThunk : Node d -> foldState -> foldState
        childrenFoldingThunk node previousFoldState =
            foldTree foldOptions previousFoldState node |> foldOptions.childrenFoldingThunk previousFoldState node
    in
        List.foldl childrenFoldingThunk initialFoldState nodes

treeHeightFoldOptions : FoldOptions d Int
treeHeightFoldOptions =
    { preFoldingThunk = \foldStateFromParent node -> 0
    , postFoldingThunk = \foldStateFromParent node childrenHeight -> childrenHeight + 1
    , childrenFoldingThunk = \heightSoFar node childHeight -> max heightSoFar childHeight
    }

{-| Calculates the height of a tree, which is the number of steps of the longest path to a
leaf node. Since a tree consists of at least one node, the height of a tree is always at least 1.
-}
treeHeight : Node d -> Int
treeHeight node =
    foldTree treeHeightFoldOptions 0 node


{-| Calculates the height of a forest (list of trees), which is the height of the
tallest tree in the forest. The calculated height value is 0 if the forest is
empty of trees, otherwise always at least 1.
-}
forestHeight : List (Node d) -> Int
forestHeight nodes =
    foldForest treeHeightFoldOptions 0 nodes

joinFoldOptions : (Node d -> String) -> String -> FoldOptions d String
joinFoldOptions nodeToString separator =
    { defaultFoldOptions
    | preFoldingThunk = \foldStateFromParent node ->
          (if String.isEmpty foldStateFromParent then "" else foldStateFromParent ++ separator) ++ (nodeToString node)
    }

{-| Joins the text representation of all nodes of a tree into a single string,
separated by given separator, in descending and left->right traversal order.
Will never be an empty string.
-}
joinTree : (Node d -> String) -> String -> Node d -> String
joinTree nodeToString separator node =
    foldTree (joinFoldOptions nodeToString separator) "" node

{-| Joins the text representation of all nodes of a forest into a single string,
separated by given separator, in left->right order. Can be an empty string in case
the forest is empty of trees.
-}
joinForest : (Node d -> String) -> String -> List (Node d) -> String
joinForest nodeToString separator nodes =
    foldForest (joinFoldOptions nodeToString separator) "" nodes


{-| A node annotated with
* zero-based index of the node in the order of traversal
* zero-based level of the node (level is less then the height of the tree
  the node is in)
* the node itself.
-}
type alias AnnotatedNode d =
    { index : Int
    , level : Int
    , node : Node d
    }

type alias AnnotatedListingFoldState d =
    { index : Int
    , level : Int
    , annotatedNodes : List (AnnotatedNode d)
    }

initialAnnotatedListingFoldState : AnnotatedListingFoldState d
initialAnnotatedListingFoldState =
    AnnotatedListingFoldState -1 -1 []

listAnnotatedFoldOptions : FoldOptions d (AnnotatedListingFoldState d)
listAnnotatedFoldOptions =
    { defaultFoldOptions
    | preFoldingThunk = \foldStateFromParent node ->
          let
              index = foldStateFromParent.index + 1
              level = foldStateFromParent.level + 1
              annotatedNode = AnnotatedNode index level node
          in
              { foldStateFromParent
              | index = index
              , level = level
              , annotatedNodes = foldStateFromParent.annotatedNodes ++ [ annotatedNode ]
              }
    , childrenFoldingThunk = \previousFoldState node nodeFoldState ->
        { nodeFoldState
        | level = nodeFoldState.level - 1
        }
    }

{-| Lists the nodes of a tree, annotated, in descending and left->right traversal order.
Will never be an empty list. See [`AnnotatedNode`](#AnnotatedNode) for what a node
is annotated with.
-}
listAnnotatedTreeNodes : Node d -> List (AnnotatedNode d)
listAnnotatedTreeNodes node =
    foldTree listAnnotatedFoldOptions initialAnnotatedListingFoldState node |> .annotatedNodes


{-| Lists the nodes of a forest, annotated, in left-right order. Can be an empty list in case
the forest is empty of trees. See [`AnnotatedNode`](#AnnotatedNode) for what a node
is annotated with.
-}
listAnnotatedForestNodes : List (Node d) -> List (AnnotatedNode d)
listAnnotatedForestNodes nodes =
    foldForest listAnnotatedFoldOptions initialAnnotatedListingFoldState nodes |> .annotatedNodes

listFoldOptions : FoldOptions d (List (Node d))
listFoldOptions =
    { defaultFoldOptions
    | preFoldingThunk = \foldStateFromParent node -> foldStateFromParent ++ [ node ]
    }

{-| Lists the nodes of a tree, in descending and left->right traversal order.
Will never be an empty list.
-}
listTreeNodes : Node d -> List (Node d)
listTreeNodes node =
    foldTree listFoldOptions [] node

{-| Lists the nodes of a forest, in left-right order. Can be an empty list in case
the forest is empty of trees.
-}
listForestNodes : List (Node d) -> List (Node d)
listForestNodes nodes =
    foldForest listFoldOptions [] nodes
