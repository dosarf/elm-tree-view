# Elm Tree View
Tree view library (`elm-tree-view`) for Elm 0.19.

You give the tree of nodes with your data embedded
in them, along with with some configuration, and this will take care of
some usual UI interaction, such as
* navigate between visible nodes with up/down arrow keys,
* expand / collapse non-leaf nodes with right/left arrow keys,
* expand / collapse non-leaf nodes with mouse clicks.

## Demo
Check out
* the working [demo](https://dosarf.github.io/tree-view-demo/index.html),
* or the [demo sources](https://github.com/dosarf/elm-tree-view/tree/master/demo).

## Overview
To take it into use
* define a data type for the values you'll put into the tree nodes,
  * define a function to calculate the text representation of a node,
* decide on a _comparable_ type for the uids of the nodes of the tree,
  * define another function to calculate the uid of a node,
* build a tree of nodes, putting your data into them,
* initialize the TreeView model, and otherwise interact with your tree view
  in the usual [TEA](https://guide.elm-lang.org/architecture/) manner

## Test
* Install `elm-test` (with `npm`)
* Run `elm-test`.

## Changelog

### 2.x

#### 2.0.1 (2019-08-30)
Added changelog to README.md, no code changes.

#### 2.0.0 (2019-08-29)
Nodes can have custom HTML rendering with messages and interactions on their
own.

To upgrade from using 1.x to 2.0.0, modify 1.x TreeView model declarations
like

```
    type alias MyModel =
    {
        ...
    ,   treeViewModel : TreeView.Model MyData MyUid -- elm-tree-view 1.x
    }
```

just add a `Never` to say no custom messages are coming from such a simple tree
view, like

```
    type alias MyModel =
    {
        ...
    ,   treeViewModel : TreeView.Model MyData MyUid Never -- elm-tree-view 2.0.0
    }
```

it should build and behave the same (nodes rendered as simple texts).

To go for custom node rendering, use the new types and functions with the
suffix `2`, like [`TreeView.Configuration2`](TreeView#Configuration2),
[`TreeView.initializeModel2`](TreeView#initializeModel2), etc ...

### 1.x

#### 1.0.0 (2019-07-26)
Initial version - nodes are rendered as simple text only.
