module EditableTreeTab exposing (Model, Msg, initialModel, update, view, subscriptions)

import Html
import Html.Styled exposing (Html, div, input, map, fromUnstyled, text, toUnstyled)
import Css exposing (px, width)
import Html.Styled.Attributes exposing (css, value, type_)
import Html.Styled.Events exposing (onInput)
import Tree as T
import TreeView as TV
import Mwc.Button
import Mwc.TextField

{-
    A little module inside this module for something that could even be
    put to another module
-}
type alias NodeData =
    { uid : String
    , label : String -- TODO rename to content
    , editing : Bool
    }


initNodeData : String -> String -> NodeData
initNodeData uid label =
    NodeData uid label False


type NodeDataMsg
    = EditContent String String -- uid content


updateNodeData : NodeDataMsg -> NodeData -> NodeData
updateNodeData msg nodeData =
    case msg of
        EditContent uid content ->
            if nodeData.uid == uid then
                { nodeData | label = content }

            else
                nodeData


viewNodeData : T.Node NodeData -> Html.Html NodeDataMsg
viewNodeData node =
    let
        nodeData =
            T.dataOf node
    in
        if nodeData.editing then
            input
                [ onInput <| EditContent nodeData.uid
                , type_ "text"
                , value nodeData.label
                ]
                []
                |> toUnstyled

        else
            text nodeData.label
                |> toUnstyled


nodeUidOf : T.Node NodeData -> TV.NodeUid String
nodeUidOf n =
    case n of
        T.Node node -> TV.NodeUid node.data.uid


{-
    Actual stuff for this tab (app)
-}
type alias Model =
    { rootNodes : List (T.Node NodeData)
    , treeModel : TV.Model NodeData String NodeDataMsg
    , selectedNode : Maybe NodeData
    }


configuration : TV.Configuration2 NodeData String NodeDataMsg
configuration =
    TV.Configuration2 nodeUidOf viewNodeData TV.defaultCssClasses


initialModel : Model
initialModel =
    let
        rootNodes =
            [ T.Node
              { children =
                [ T.Node { children = [], data = initNodeData "1.1" "hello" }
                , T.Node { children = [], data = initNodeData "1.2" "goodbye" }
                , T.Node { children = [], data = initNodeData "1.3" "you say yes" }
                ]
              , data = initNodeData "1" "Beatles"
              }
            , T.Node
              { children =
                [ T.Node
                  { children =
                    [ T.Node { children = [], data = initNodeData "2.1.1" "la" }
                    , T.Node { children = [], data = initNodeData "2.1.2" "vista" }
                    ]
                  , data = initNodeData "2.1" "hasta"
                  }
                , T.Node
                  { children = []
                  , data = initNodeData "2.2" "baby"
                  }
                ]
              , data = initNodeData "2" "Terminator"
              }
            ]
    in
    { rootNodes = rootNodes
    , treeModel = TV.initializeModel2 configuration rootNodes
    , selectedNode = Nothing
    }


type Msg =
  TreeViewMsg (TV.Msg2 String NodeDataMsg)
  | ExpandAll
  | CollapseAll


update : Msg -> Model -> Model
update message model =
    let
        treeModel =
            case message of
                TreeViewMsg (TV.CustomMsg nodeDataMsg) ->
                    case nodeDataMsg of
                        EditContent nodeUid content ->
                            setNodeContent nodeUid content model.treeModel

                TreeViewMsg tvMsg ->
                    TV.update2 tvMsg model.treeModel

                ExpandAll ->
                    TV.expandAll model.treeModel

                CollapseAll ->
                    TV.collapseAll model.treeModel

        selectedNode =
            TV.getSelected treeModel |> Maybe.map .node |> Maybe.map T.dataOf

    in
        { model
        | treeModel = toggleEditable model.selectedNode selectedNode treeModel
        , selectedNode = selectedNode
        }


setNodeContent : String -> String -> TV.Model NodeData String NodeDataMsg -> TV.Model NodeData String NodeDataMsg
setNodeContent nodeUid content treeModel =
    TV.updateNodeData
        (\nodeData -> nodeData.uid == nodeUid)
        (\nodeData -> { nodeData | label = content })
        treeModel


areSameNodes : NodeData -> NodeData -> Bool
areSameNodes nodeData1 nodeData2 =
    nodeData1.uid == nodeData2.uid


{- Turns the previously selected node non-editable, and the newly selected to be
editable.
-}
toggleEditable : Maybe NodeData -> Maybe NodeData -> TV.Model NodeData String NodeDataMsg -> TV.Model NodeData String NodeDataMsg
toggleEditable currentlySelected nextSelected treeModel =
    let
        isNodeCurrentlySelected =
            currentlySelected
                |> Maybe.map (\cS -> areSameNodes cS)
                |> Maybe.withDefault (\_ -> False)
        isNodeNextSelected =
            nextSelected
                |> Maybe.map (\nS -> areSameNodes nS)
                |> Maybe.withDefault (\_ -> False)
    in
        TV.updateNodeData
            (\nodeData -> isNodeCurrentlySelected nodeData || isNodeNextSelected nodeData)
            (\nodeData -> { nodeData | editing = isNodeNextSelected nodeData })
            treeModel


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
      , map TreeViewMsg (TV.view2 model.treeModel |> fromUnstyled)
      ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map TreeViewMsg (TV.subscriptions2 model.treeModel)