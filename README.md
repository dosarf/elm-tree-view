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
The simplest way to take this facility into use is to show tree views that
have a simple text representation for each node data.

For that,
* define a data type for the values you'll put into the tree nodes,
  * define a function to calculate the text representation of a node, e.g.

```
    type alias MyData = { ... }

    nodeLabel : Tree.Node MyData -> String
    nodeLabel node = ...

```

* decide on a _comparable_ type for the uids of the nodes of the tree,
  * define another function to calculate the uid of a node,

```
    nodeUid : Tree.Node MyData -> TreeView.NodeUid String
    nodeUid = ...
```


* build a tree (forest) of nodes, putting your data into them,

```
    rootNodes : List (Tree.Node MyData)
    rootNodes =
        [ Tree.Node { data = ..., children = [...] }
        , ...
        ]
```

* construct a configuration for your tree view

```
    -- configuration for this tree view needs two type parameters
    -- (a) data type and (b) node UID type
    configuration : TreeView.Configuration MyData String
    configuration =
        TV.Configuration
            nodeUid  -- to construct node UIDs
            nodeLabel  -- to render node (data) to text
            TreeView.defaultCssClasses  -- CSS classes to use
```

* initialize the TreeView model

```
    type alias Model =
        { ...
        , treeViewModel = TreeView.Model MyData String Never ()
        , ...
        }

    initialModel : Model
    initialModel =
        { ...
        , treeViewModel =
            TreeView.initializeModel configuration rootNodes
        , ...
        }
```

* and otherwise interact with your tree view
  in the usual [TEA](https://guide.elm-lang.org/architecture/) manner, making
  calls to [`TreeView.update`](TreeView#update), [`TreeView.view`](TreeView#view)

```
    type Msg
        = TreeViewMsg (TreeView.Msg String) -- for node UIDs of type String
        | ...

```

* if (or when) you want the tree view to navigate up/down between visible nodes
  and expand/collapse nodes on arrow key presses, use
  [`TreeView.subscriptions`](TreeView#subscriptions)

```
    subscriptions : Model -> Sub Msg
    subscriptions model =
        Sub.map TreeViewMsg (TreeView.subscriptions model.treeModel)

```

### Custom node rendering
You may want fancier rendering of nodes, possibly with user interaction.

The essential difference to the simple case outlined above is that instead of
a node data -> text rendering function, you need a function that turns a
cookie (of yours) and a node data into a piece of Html emitting your own
little node-specific messages.

```
-- the type of application data that may be needed for custom
-- node rendering
type alias MyCookie = { ... }

-- messages coming out of node specific Html fragments
type MyDataMsg = ...

-- fancy custom node rendering using cookie + node data
viewNodeData : MyCookie -> Tree.Node MyData -> Html.Html MyDataMsg
viewNodeData = ...
```

From here on, switch to using types and functions with the suffix `2`, such as
[`TreeView.Configuration2`](TreeView#Configuration2),
[`TreeView.initializeModel2`](TreeView#initializeModel2),
[`TreeView.Msg2`](TreeView#Msg2),
[`TreeView.update2`](TreeView#update2),
[`TreeView.view2`](TreeView#view2) and
[`TreeView.subscriptions2`](TreeView#subscriptions2).


## Test
* Install `elm-test` (with `npm`)
* Run `elm-test`.

## Changelog

### 3.x

#### 3.0.0 (TODO)

##### Cookie for custom node rendering
Custom node rendering function can now use cookie data - data that is not
within the node. That's an extra type parameter to the TreeView.Model.

If you are not using custom rendering at all, or you do but you don't need
any cookie data for node rendering, just update your 2.x TreeView application

```
    type alias MyModel =
    {
        ...
    ,   treeViewModel : TreeView.Model MyData MyUid Never -- elm-tree-view 2.x
    }
```

thusly:

```
    type alias MyModel =
    {
        ...
    ,   treeViewModel : TreeView.Model MyData MyUid ()
    }
```

If you use custom node rendering with cookie, then you will be using some
type of yours instead of the unit type `()`.

##### Support for TreeView search
You can now construct tree views that can be searched. Or more accurately,
you now have a way (see [`TreeView.expandOnly`](TreeView#expandOnly)) to
expand only those nodes that harbor a child node matching a certain criteria,
effectively hiding (collapsing) significant part of the tree. See also
the demo.


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
