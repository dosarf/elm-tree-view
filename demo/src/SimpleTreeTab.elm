module SimpleTreeTab exposing (Model, Msg, initialModel, update, view, subscriptions)

import Html.Styled exposing (Html, div, map, fromUnstyled)
import Css exposing (px, width)
import Html.Styled.Attributes exposing (css)
import Tree as T
import TreeView as TV
import Mwc.Button
import Mwc.TextField

type alias NodeData =
    { uid : String
    , label : String
    }

type alias Model =
    { rootNodes : List (T.Node NodeData)
    , treeModel : TV.Model NodeData String Never ()
    , selectedNode : Maybe NodeData
    }

nodeLabel : T.Node NodeData -> String
nodeLabel n =
    case n of
        T.Node node -> node.data.label

nodeUid : T.Node NodeData -> TV.NodeUid String
nodeUid n =
    case n of
        T.Node node -> TV.NodeUid node.data.uid

configuration : TV.Configuration NodeData String
configuration =
    TV.Configuration nodeUid nodeLabel TV.defaultCssClasses

initialModel : Model
initialModel =
    let
        rootNodes =
            [ T.Node
              { children =
                [ T.Node { children = [], data = NodeData "1.1" "hello" }
                , T.Node { children = [], data = NodeData "1.2" "goodbye" }
                , T.Node { children = [], data = NodeData "1.3" "you say yes" }
                ]
              , data = NodeData "1" "Beatles"
              }
            , T.Node
              { children =
                [ T.Node
                  { children =
                    [ T.Node { children = [], data = NodeData "2.1.1" "la" }
                    , T.Node { children = [], data = NodeData "2.1.2" "vista" }
                    ]
                  , data = NodeData "2.1" "hasta"
                  }
                , T.Node
                  { children = []
                  , data = NodeData "2.2" "baby"
                  }
                ]
              , data = NodeData "2" "Terminator"
              }
            ]
    in
    { rootNodes = rootNodes
    , treeModel = TV.initializeModel configuration rootNodes
    , selectedNode = Nothing
    }

type Msg =
  TreeViewMsg (TV.Msg String)
  | ExpandAll
  | CollapseAll

update : Msg -> Model -> Model
update message model =
    let
        treeModel =
            case message of
                TreeViewMsg tvMsg ->
                    TV.update tvMsg model.treeModel
                ExpandAll ->
                    TV.expandAll model.treeModel
                CollapseAll ->
                    TV.collapseAll model.treeModel
    in
        { model
        | treeModel = treeModel
        , selectedNode = TV.getSelected treeModel |> Maybe.map .node |> Maybe.map T.dataOf
        }

expandAllCollapseAllButtons : Html Msg
expandAllCollapseAllButtons =
    div
      []
      [ Mwc.Button.view
          [ Mwc.Button.raised
          , Mwc.Button.onClick ExpandAll
          , Mwc.Button.label "Expand all"
          ]
      , Mwc.Button.view
          [ Mwc.Button.raised
          , Mwc.Button.onClick CollapseAll
          , Mwc.Button.label "Collapse all"
          ]
      ]

selectedNodeDetails : Model -> Html Msg
selectedNodeDetails model =
    let
        selectedDetails =
            Maybe.map (\nodeData -> nodeData.uid ++ ": " ++ nodeData.label) model.selectedNode
                |> Maybe.withDefault "(nothing selected)"
    in
        div
            [ css [ width (px 300) ] ]
            [ Mwc.TextField.view
                [ Mwc.TextField.readonly True
                , Mwc.TextField.label selectedDetails
                ]
            ]

view : Model -> Html Msg
view model =
    div
      []
      [ expandAllCollapseAllButtons
      , selectedNodeDetails model
      , map TreeViewMsg (TV.view model.treeModel |> fromUnstyled)
      ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map TreeViewMsg (TV.subscriptions model.treeModel)
