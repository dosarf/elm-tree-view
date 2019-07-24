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
* or the [demo sources](https://github.com/dosarf/elm-tree-view/demo).

## Overview
To take it into use
* define a data type for the values you'll put into the tree nodes,
  * define a function to calculate the text representation of a node,
* decide on a _comparable_ type for the uids of the nodes of the tree,
  * define another function to calculate the uid of a node,
* build a tree of nodes, putting your data into them,
* initialize the TreeView model, and otherwise interact with your tree view
  in the usual [TEA](https://guide.elm-lang.org/architecture/) manner

For details, see [TreeView](TreeView)

## Test
* Install `elm-test` (with `npm`)
* Run `elm-test`.
