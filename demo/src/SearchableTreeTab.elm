module SearchableTreeTab exposing (Model, Msg, initialModel, update, view, subscriptions, NodeData, highlightMatches, TextFragment (..))

import Html.Styled exposing (Html, div, map, fromUnstyled, span, text)
import Css exposing (backgroundColor, px, rgb, width)
import Html.Styled.Attributes exposing (css)
import Html
import Tree as T
import TreeView as TV
import Mwc.Button
import Mwc.TextField
import Regex


type alias NodeData =
    { uid : String
    , label : String
    }


nodeData : String -> String -> NodeData
nodeData uid label =
    NodeData uid label


nodeUid : T.Node NodeData -> TV.NodeUid String
nodeUid n =
    case n of
        T.Node node -> TV.NodeUid node.data.uid


type TextFragment
    = Vanilla String
    | Highlit String


renderTextFragment : TextFragment -> Html.Html Never
renderTextFragment fragment =
    case fragment of
        Vanilla string ->
            Html.text string

        Highlit string ->
            Html.strong [] [ Html.text string ]


highlightMatches : Maybe Regex.Regex -> T.Node NodeData -> List TextFragment
highlightMatches searchRegex n =
    let
        nodeLabel =
            T.dataOf n |> .label

        vanillaTextBetween : Int -> Int -> List TextFragment
        vanillaTextBetween begin end =
            if begin == end then
                []
            else
                [ Vanilla <| String.slice begin end nodeLabel ]

        hightlightMatches2 : String -> Regex.Regex -> List TextFragment
        hightlightMatches2 string regex =
            let
                matches =
                    Regex.find regex string

                processor : Regex.Match -> (Int, List TextFragment) -> (Int, List TextFragment)
                processor match (position, fragments) =
                    let
                        nextPosition =
                            match.index
                    in
                        (nextPosition + (String.length match.match)
                        , fragments
                            ++ (vanillaTextBetween position nextPosition)
                            ++ [ Highlit match.match]
                        )

                semiProcessed : (Int, List TextFragment)
                semiProcessed =
                    List.foldl processor (0, []) matches

                lastPosition : Int
                lastPosition =
                    Tuple.first semiProcessed

            in
                (Tuple.second semiProcessed) ++ (vanillaTextBetween lastPosition <| String.length nodeLabel)
    in
        searchRegex
            |> Maybe.map (hightlightMatches2 nodeLabel)
            |> Maybe.withDefault [Vanilla nodeLabel]


viewNodeData : Maybe Regex.Regex -> T.Node NodeData -> Html.Html Never
viewNodeData searchRegex n =
    highlightMatches searchRegex n
        |> List.map renderTextFragment
        |> Html.span []


type alias Model =
    { rootNodes : List (T.Node NodeData)
    , treeModel : TV.Model NodeData String Never (Maybe Regex.Regex) -- interactions: none, cookie: search term
    , selectedNode : Maybe NodeData
    , searchTerm : Maybe String
    , searchRegex : Maybe Regex.Regex
    , termError : Maybe String
    }


configuration : TV.Configuration2 NodeData String Never (Maybe Regex.Regex) -- interactions: none, cookie: search term
configuration =
    TV.Configuration2 nodeUid viewNodeData TV.defaultCssClasses


initialModel : Model
initialModel =
    let
        rootNodes =
            [ T.Node
              { children =
                [ T.Node { children = [], data = nodeData "1.1" "hello" }
                , T.Node { children = [], data = nodeData "1.2" "goodbye" }
                , T.Node { children = [], data = nodeData "1.3" "you say yes" }
                ]
              , data = nodeData "1" "Beatles"
              }
            , T.Node
              { children =
                [ T.Node
                  { children =
                    [ T.Node { children = [], data = nodeData "2.1.1" "la" }
                    , T.Node { children = [], data = nodeData "2.1.2" "vista" }
                    ]
                  , data = nodeData "2.1" "hasta"
                  }
                , T.Node
                  { children = []
                  , data = nodeData "2.2" "baby"
                  }
                ]
              , data = nodeData "2" "Terminator"
              }
            ]
    in
    { rootNodes = rootNodes
    , treeModel = TV.initializeModel2 configuration rootNodes
    , selectedNode = Nothing
    , searchTerm = Nothing
    , searchRegex = Nothing
    , termError = Nothing
    }


type Msg =
  TreeViewMsg (TV.Msg2 String Never)
  | ExpandAll
  | CollapseAll
  | UseSearchTerm String


matchesSearchTerm : Maybe Regex.Regex -> NodeData -> Bool
matchesSearchTerm searchTerm nD =
    searchTerm
        |> Maybe.map (\rx -> Regex.contains rx nD.label)
        |> Maybe.withDefault True


regexOptions : Regex.Options
regexOptions =
    { caseInsensitive = True
    , multiline = False
    }


update : Msg -> Model -> Model
update message model =
    let
        incompleteModel =
            case message of
                TreeViewMsg tvMsg ->
                    { model | treeModel = TV.update2 tvMsg model.treeModel }

                ExpandAll ->
                    { model | treeModel = TV.expandAll model.treeModel }

                CollapseAll ->
                    { model | treeModel = TV.collapseAll model.treeModel }

                UseSearchTerm string ->
                    let
                        (searchTerm, searchRegex, termError) =
                            if String.isEmpty string then
                                (Nothing, Nothing, Nothing)
                            else
                                Regex.fromStringWith regexOptions string
                                    |> Maybe.map (\rx -> (Just string, Just rx, Nothing))
                                    |> Maybe.withDefault (Just string, Nothing, Just "Invalid regular expression")
                    in
                        { model
                        | treeModel = TV.expandOnly (matchesSearchTerm searchRegex) model.treeModel
                        , searchTerm = searchTerm
                        , searchRegex = searchRegex
                        , termError = termError
                        }
    in
        { incompleteModel
        | selectedNode = TV.getSelected incompleteModel.treeModel |> Maybe.map .node |> Maybe.map T.dataOf
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


searchTermDetails : Model -> Html Msg
searchTermDetails model =
    div
        [ css [ width (px 300) ] ]
        [ case model.termError of
              Just termError ->
                  div
                      [ css [ backgroundColor (rgb 255 196 196 ) ] ]
                      [ text termError ]

              _ ->
                  span [] []
        , Mwc.TextField.view
              [ Mwc.TextField.onInput UseSearchTerm
              , Mwc.TextField.placeHolder "(search)"
              , model.searchTerm |> Maybe.withDefault "" |> Mwc.TextField.value
              ]
        ]


selectedNodeDetails : Model -> Html Msg
selectedNodeDetails model =
    let
        selectedDetails =
            Maybe.map (\nD -> nD.uid ++ ": " ++ nD.label) model.selectedNode
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
      , searchTermDetails model
      , selectedNodeDetails model
      , map TreeViewMsg (TV.view2 model.searchRegex model.treeModel |> fromUnstyled)
      ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map TreeViewMsg (TV.subscriptions2 model.treeModel)
