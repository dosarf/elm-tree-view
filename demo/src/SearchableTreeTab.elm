module SearchableTreeTab exposing (
    Model, Msg, initialModel, update, view, subscriptions
    , NodeData, highlightMatches, TextFragment (..), jsonNodeDecoder, jsonNodeToTree, parseJsonNodes)

import Html.Styled exposing (Html, div, map, fromUnstyled, span, text)
import Css exposing (backgroundColor, px, rgb, width)
import Html.Styled.Attributes exposing (css)
import Html
import Tree as T
import TreeView as TV
import Mwc.Button
import Mwc.TextField
import Regex
import Json.Decode as D

{- JSON -}
type alias JsonNode =
    { uid : String
    , content : String
    , children : JsonNodes
    }

type JsonNodes =
    JsonNodes (List JsonNode)


jsonNodeDecoder : D.Decoder JsonNode
jsonNodeDecoder =
    D.map3
        JsonNode
        (D.field "uid" D.string)
        (D.field "content" D.string)
        (D.field "children" (D.map JsonNodes (D.list (D.lazy (\_-> jsonNodeDecoder)))))


jsonNodesDecoder : D.Decoder JsonNodes
jsonNodesDecoder =
    D.map JsonNodes (D.list jsonNodeDecoder)


jsonNodeToTree : JsonNode -> T.Node NodeData
jsonNodeToTree jsonNode =
    T.Node
        { data = nodeData jsonNode.uid jsonNode.content
        , children = jsonNodesToForest jsonNode.children
        }


jsonNodesToForest : JsonNodes -> List (T.Node NodeData)
jsonNodesToForest jsonNodes =
    case jsonNodes of
        JsonNodes jsNodes ->
            List.map jsonNodeToTree jsNodes


parseJsonNodes : String -> List (T.Node NodeData)
parseJsonNodes string =
    D.decodeString jsonNodesDecoder string
        |> Result.map jsonNodesToForest
        |> Result.toMaybe
        |> Maybe.withDefault [ T.Node { data = nodeData "0" "failed", children = [] } ]


{- NodeData, tree -}
type alias NodeData =
    { uid : String
    , content : String
    }


nodeData : String -> String -> NodeData
nodeData uid content =
    NodeData uid content


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
            T.dataOf n |> .content

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



{- Model, Msg, etc -}
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
    { rootNodes = forest
    , treeModel = TV.initializeModel2 configuration forest
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
        |> Maybe.map (\rx -> Regex.contains rx nD.content)
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
            Maybe.map (\nD -> nD.uid ++ ": " ++ nD.content) model.selectedNode
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


forest : List (T.Node NodeData)
forest =
    let
        json =
            """
            [ { "uid" : "1", "content" : "header"
              , "children" : [
                    { "uid" : "1.title" , "content" : "The Bards of Wales", "children" : []}
                  , { "uid" : "1.author" , "content" : "Arany János", "children" : []}
                  , { "uid" : "1.translatedBy" , "content" : "Watson Kirkconnel", "children" : []}
                  ]
              }
            , { "uid" : "2", "content" : "poem"
              , "children" : [
                  { "uid" : "2.1", "content" : "1"
                  , "children" : [
                      { "uid" : "2.1.1" , "content" : "Edward the king, the English king,", "children" : []},
                      { "uid" : "2.1.2" , "content" : "Bestrides his tawny steed,", "children" : []},
                      { "uid" : "2.1.3" , "content" : "'For I will see if Wales,' said he,", "children" : []},
                      { "uid" : "2.1.4" , "content" : "'Accepts my rule indeed.'", "children" : []}
                    ]
                  }
                , { "uid" : "2.2", "content" : "2"
                  , "children" : [
                      { "uid" : "2.2.1" , "content" : "'Are stream and mountain fair to see?", "children" : []},
                      { "uid" : "2.2.2" , "content" : "Are meadow grasses good?", "children" : []},
                      { "uid" : "2.2.3" , "content" : "Do corn-lands bear a crop more rare", "children" : []},
                      { "uid" : "2.2.4" , "content" : "Since wash'd with rebel's blood?'", "children" : []}
                    ]
                  }
                , { "uid" : "2.3", "content" : "3"
                  , "children" : [
                      { "uid" : "2.3.1" , "content" : "'And are the wretched people there,", "children" : []},
                      { "uid" : "2.3.2" , "content" : "Whose insolence I broke", "children" : []},
                      { "uid" : "2.3.3" , "content" : "As happy as the oxen are", "children" : []},
                      { "uid" : "2.3.4" , "content" : "Beneath the driver's yoke?'", "children" : []}
                    ]
                  }
                , { "uid" : "2.4", "content" : "4"
                  , "children" : [
                      { "uid" : "2.4.1" , "content" : "'In truth this Wales, Sire, is a gem,", "children" : []},
                      { "uid" : "2.4.2" , "content" : "The fairest in your crown:", "children" : []},
                      { "uid" : "2.4.3" , "content" : "The stream and field rich harvest yield,", "children" : []},
                      { "uid" : "2.4.4" , "content" : "And fair and dale and down.'", "children" : []}
                    ]
                  }
                , { "uid" : "2.5", "content" : "5"
                  , "children" : [
                      { "uid" : "2.5.1" , "content" : "'And all the wretched people there", "children" : []},
                      { "uid" : "2.5.2" , "content" : "Are calm as man could crave;", "children" : []},
                      { "uid" : "2.5.3" , "content" : "Their hovels stand throughout the land", "children" : []},
                      { "uid" : "2.5.4" , "content" : "As silent as the grave.'", "children" : []}
                    ]
                  }
                , { "uid" : "2.6", "content" : "6"
                  , "children" : [
                      { "uid" : "2.6.1" , "content" : "Edward the king, the English King", "children" : []},
                      { "uid" : "2.6.2" , "content" : "Bestrides his tawny steed;", "children" : []},
                      { "uid" : "2.6.3" , "content" : "A silence deep his subjects keep", "children" : []},
                      { "uid" : "2.6.4" , "content" : "And Wales is mute indeed.", "children" : []}
                    ]
                  }
                , { "uid" : "2.7", "content" : "7"
                  , "children" : [
                      { "uid" : "2.7.1" , "content" : "The castle named Montgomery", "children" : []},
                      { "uid" : "2.7.2" , "content" : "Ends that day's journeying;", "children" : []},
                      { "uid" : "2.7.3" , "content" : "The castle's lord, Montgomery,", "children" : []},
                      { "uid" : "2.7.4" , "content" : "Must entertain the king.", "children" : []}
                    ]
                  }
                , { "uid" : "2.8", "content" : "8"
                  , "children" : [
                      { "uid" : "2.8.1" , "content" : "Then game and fish and ev'ry dish", "children" : []},
                      { "uid" : "2.8.2" , "content" : "That lures the taste and sight", "children" : []},
                      { "uid" : "2.8.3" , "content" : "A hundred hurrying servants bear", "children" : []},
                      { "uid" : "2.8.4" , "content" : "To please the appetite.", "children" : []}
                    ]
                  }
                , { "uid" : "2.9", "content" : "9"
                  , "children" : [
                      { "uid" : "2.9.1" , "content" : "With all of worth the isle brings forth", "children" : []},
                      { "uid" : "2.9.2" , "content" : "In dainty drink and food,", "children" : []},
                      { "uid" : "2.9.3" , "content" : "And all the wines of foreign vines", "children" : []},
                      { "uid" : "2.9.4" , "content" : "Beyond the distant flood.", "children" : []}
                    ]
                  }
                , { "uid" : "2.10", "content" : "10"
                  , "children" : [
                      { "uid" : "2.10.1" , "content" : "'You lords, you lords, will none consent", "children" : []},
                      { "uid" : "2.10.2" , "content" : "His glass with mine to ring?", "children" : []},
                      { "uid" : "2.10.3" , "content" : "What? Each one fails, you dogs of Wales,", "children" : []},
                      { "uid" : "2.10.4" , "content" : "To toast the English king?'", "children" : []}
                    ]
                  }
                , { "uid" : "2.11", "content" : "11"
                  , "children" : [
                      { "uid" : "2.11.1" , "content" : "'Though game and fish and ev'ry dish", "children" : []},
                      { "uid" : "2.11.2" , "content" : "That lures the taste and sight", "children" : []},
                      { "uid" : "2.11.3" , "content" : "Your hand supplies, your mood defies", "children" : []},
                      { "uid" : "2.11.4" , "content" : "My person with a slight.'", "children" : []}
                    ]
                  }
                , { "uid" : "2.12", "content" : "12"
                  , "children" : [
                      { "uid" : "2.12.1" , "content" : "'You rascal lords, you dogs of Wales,", "children" : []},
                      { "uid" : "2.12.2" , "content" : "Will none for Edward cheer?", "children" : []},
                      { "uid" : "2.12.3" , "content" : "To serve my needs and chant my deeds", "children" : []},
                      { "uid" : "2.12.4" , "content" : "Then let a bard appear!'", "children" : []}
                    ]
                  }
                , { "uid" : "2.13", "content" : "13"
                  , "children" : [
                      { "uid" : "2.13.1" , "content" : "The nobles gaze in fierce amaze,", "children" : []},
                      { "uid" : "2.13.2" , "content" : "Their cheeks grow deadly pale;", "children" : []},
                      { "uid" : "2.13.3" , "content" : "Not fear but rage their looks engage,", "children" : []},
                      { "uid" : "2.13.4" , "content" : "They blanch but do not quail.", "children" : []}
                    ]
                  }
                , { "uid" : "2.14", "content" : "14"
                  , "children" : [
                      { "uid" : "2.14.1" , "content" : "All voices cease in soundless peace,", "children" : []},
                      { "uid" : "2.14.2" , "content" : "All breathe in silent pain;", "children" : []},
                      { "uid" : "2.14.3" , "content" : "Then at the door a harper hoar", "children" : []},
                      { "uid" : "2.14.4" , "content" : "Comes in with grave disdain:", "children" : []}
                    ]
                  }
                , { "uid" : "2.15", "content" : "15"
                  , "children" : [
                      { "uid" : "2.15.1" , "content" : "'Lo, here I stand, at your command,", "children" : []},
                      { "uid" : "2.15.2" , "content" : "To chant your deeds, O king!'", "children" : []},
                      { "uid" : "2.15.3" , "content" : "And weapons clash and hauberks crash", "children" : []},
                      { "uid" : "2.15.4" , "content" : "Responsive to his string.", "children" : []}
                    ]
                  }
                , { "uid" : "2.16", "content" : "16"
                  , "children" : [
                      { "uid" : "2.16.1" , "content" : "'Harsh weapons clash and hauberks crash,", "children" : []},
                      { "uid" : "2.16.2" , "content" : "And sunset sees us bleed,", "children" : []},
                      { "uid" : "2.16.3" , "content" : "The crow and wolf our dead engulf -", "children" : []},
                      { "uid" : "2.16.4" , "content" : "This, Edward, is your deed!'", "children" : []}
                    ]
                  }
                , { "uid" : "2.17", "content" : "17"
                  , "children" : [
                      { "uid" : "2.17.1" , "content" : "'A thousand lie beneath the sky,", "children" : []},
                      { "uid" : "2.17.2" , "content" : "They rot beneath the sun,", "children" : []},
                      { "uid" : "2.17.3" , "content" : "And we who live shall not forgive", "children" : []},
                      { "uid" : "2.17.4" , "content" : "This deed your hand hath done!'", "children" : []}
                    ]
                  }
                , { "uid" : "2.18", "content" : "18"
                  , "children" : [
                      { "uid" : "2.18.1" , "content" : "'Now let him perish! I must have'", "children" : []},
                      { "uid" : "2.18.2" , "content" : "(The monarch's voice is hard)", "children" : []},
                      { "uid" : "2.18.3" , "content" : "'Your softest songs, and not your wrongs!'", "children" : []},
                      { "uid" : "2.18.4" , "content" : "In steps a boyish bard:", "children" : []}
                    ]
                  }
                , { "uid" : "2.19", "content" : "19"
                  , "children" : [
                      { "uid" : "2.19.1" , "content" : "'The breeze is soft at eve, that oft", "children" : []},
                      { "uid" : "2.19.2" , "content" : "From Milford Havens moans;", "children" : []},
                      { "uid" : "2.19.3" , "content" : "It whispers maidens' stifled cries,", "children" : []},
                      { "uid" : "2.19.4" , "content" : "It breathes of widows' groans.'", "children" : []}
                    ]
                  }
                , { "uid" : "2.20", "content" : "20"
                  , "children" : [
                      { "uid" : "2.20.1" , "content" : "'You maidens, bear no captive babes!", "children" : []},
                      { "uid" : "2.20.2" , "content" : "You mothers, rear them not!'", "children" : []},
                      { "uid" : "2.20.3" , "content" : "The fierce king nods. The lad is seiz'd", "children" : []},
                      { "uid" : "2.20.4" , "content" : "And hurried from the spot.", "children" : []}
                    ]
                  }
                , { "uid" : "2.21", "content" : "21"
                  , "children" : [
                      { "uid" : "2.21.1" , "content" : "Unbidden then, among the men,", "children" : []},
                      { "uid" : "2.21.2" , "content" : "There comes a dauntless third", "children" : []},
                      { "uid" : "2.21.3" , "content" : "With speech of fire he tunes his lyre,", "children" : []},
                      { "uid" : "2.21.4" , "content" : "And bitter is his word:", "children" : []}
                    ]
                  }
                , { "uid" : "2.22", "content" : "22"
                  , "children" : [
                      { "uid" : "2.22.1" , "content" : "'Our bravest died to slake your pride -", "children" : []},
                      { "uid" : "2.22.2" , "content" : "Proud Edward, hear my lays!", "children" : []},
                      { "uid" : "2.22.3" , "content" : "No Welsh bards live who e'er will give", "children" : []},
                      { "uid" : "2.22.4" , "content" : "Your name a song a praise.'", "children" : []}
                    ]
                  }
                , { "uid" : "2.23", "content" : "23"
                  , "children" : [
                      { "uid" : "2.23.1" , "content" : "'Our harps with dead men's memories weep.", "children" : []},
                      { "uid" : "2.23.2" , "content" : "Welsh bards to you will sing", "children" : []},
                      { "uid" : "2.23.3" , "content" : "One changeless verse - our blackest curse", "children" : []},
                      { "uid" : "2.23.4" , "content" : "To blast your soul, O king!'", "children" : []}
                    ]
                  }
                , { "uid" : "2.24", "content" : "24"
                  , "children" : [
                      { "uid" : "2.24.1" , "content" : "'No more! Enough!' - cries out the king.", "children" : []},
                      { "uid" : "2.24.2" , "content" : "In rage his orders break:", "children" : []},
                      { "uid" : "2.24.3" , "content" : "'Seek through these vales all bards of Wales", "children" : []},
                      { "uid" : "2.24.4" , "content" : "And burn them at the stake!'", "children" : []}
                    ]
                  }
                , { "uid" : "2.25", "content" : "25"
                  , "children" : [
                      { "uid" : "2.25.1" , "content" : "His men ride forth to south and north,", "children" : []},
                      { "uid" : "2.25.2" , "content" : "They ride to west and east.", "children" : []},
                      { "uid" : "2.25.3" , "content" : "Thus ends in grim Montgomery", "children" : []},
                      { "uid" : "2.25.4" , "content" : "The celebrated feast.", "children" : []}
                    ]
                  }
                , { "uid" : "2.26", "content" : "26"
                  , "children" : [
                      { "uid" : "2.26.1" , "content" : "Edward the king, the English king", "children" : []},
                      { "uid" : "2.26.2" , "content" : "Spurs on his tawny steed;", "children" : []},
                      { "uid" : "2.26.3" , "content" : "Across the skies red flames arise", "children" : []},
                      { "uid" : "2.26.4" , "content" : "As if Wales burned indeed.", "children" : []}
                    ]
                  }
                , { "uid" : "2.27", "content" : "27"
                  , "children" : [
                      { "uid" : "2.27.1" , "content" : "In martyrship, with song on lip,", "children" : []},
                      { "uid" : "2.27.2" , "content" : "Five hundred Welsh bards died;", "children" : []},
                      { "uid" : "2.27.3" , "content" : "Not one was mov'd to say he lov'd", "children" : []},
                      { "uid" : "2.27.4" , "content" : "The tyrant in his pride.", "children" : []}
                    ]
                  }
                , { "uid" : "2.28", "content" : "......", "children" : []
                  }
                , { "uid" : "2.29", "content" : "29"
                  , "children" : [
                      { "uid" : "2.29.1" , "content" : "'Ods blood! What songs this night resound", "children" : []},
                      { "uid" : "2.29.2" , "content" : "Upon our London streets?", "children" : []},
                      { "uid" : "2.29.3" , "content" : "The mayor shall feel my irate heel", "children" : []},
                      { "uid" : "2.29.4" , "content" : "If aught that sound repeats!'", "children" : []}
                    ]
                  }
                , { "uid" : "2.30", "content" : "30"
                  , "children" : [
                      { "uid" : "2.30.1" , "content" : "Each voice is hush'd; through silent lanes", "children" : []},
                      { "uid" : "2.30.2" , "content" : "To silent homes they creep.", "children" : []},
                      { "uid" : "2.30.3" , "content" : "'Now dies the hound that makes a sound;", "children" : []},
                      { "uid" : "2.30.4" , "content" : "The sick king cannot sleep.'", "children" : []}
                    ]
                  }
                , { "uid" : "2.31", "content" : "31"
                  , "children" : [
                      { "uid" : "2.31.1" , "content" : "'Ha! Bring me fife and drum and horn,", "children" : []},
                      { "uid" : "2.31.2" , "content" : "And let the trumpet blare!", "children" : []},
                      { "uid" : "2.31.3" , "content" : "In ceaseless hum their curses come -", "children" : []},
                      { "uid" : "2.31.4" , "content" : "I see their dead eyes glare...'", "children" : []}
                    ]
                  }
                , { "uid" : "2.32", "content" : "32"
                  , "children" : [
                      { "uid" : "2.32.1" , "content" : "But high above all drum and fife", "children" : []},
                      { "uid" : "2.32.2" , "content" : "and trumpets' shrill debate,", "children" : []},
                      { "uid" : "2.32.3" , "content" : "Five hundred martyr'd voices chant", "children" : []},
                      { "uid" : "2.32.4" , "content" : "Their hymn of deathless hate.", "children" : []}
                    ]
                  }
                ]
              }
            ]
            """
    in
        parseJsonNodes json
