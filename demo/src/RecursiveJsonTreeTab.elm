module RecursiveJsonTreeTab exposing (
    Model, Msg, initializeModel, update, view, subscriptions
    -- for testing
    , Comment, CommentData, commentDecoder, commentToTree, CommentUid, Responses(..))

import Html.Styled exposing (Html, div, map, fromUnstyled)
import Css exposing (px, width)
import Html.Styled.Attributes exposing (css)
import Tree as T
import TreeView as TV
import Mwc.Button
import Mwc.TextField
import Json.Decode as D
import Http

type alias CommentUid =
    ( String
    , Int
    )

type alias Comment =
    { author : String
    , message : String
    , uid : CommentUid
    , responses : Responses
    }

type Responses =
    Responses (List Comment)

commentDecoder : D.Decoder Comment
commentDecoder =
    D.map4 Comment
        (D.field "author" D.string)
        (D.field "message" D.string)
        (D.field "uid" (D.map2 Tuple.pair (D.index 0 D.string) (D.index 1 D.int)))
        (D.field "responses" (D.map Responses (D.list (D.lazy (\_ -> commentDecoder)))))


type alias CommentData =
    { author : String
    , message : String
    , uid : CommentUid
    }


commentsOfResponses : Responses -> List Comment
commentsOfResponses responses =
    case responses of
        Responses comments ->
            comments

commentToTree : Comment -> T.Node CommentData
commentToTree comment =
    let
        children = List.map commentToTree <| commentsOfResponses comment.responses
        data = CommentData
            comment.author
            comment.message
            comment.uid
    in
        T.Node
            { data = data
            , children = children
            }

type alias CommentThread =
    { treeViewModel : TV.Model CommentData CommentUid
    , selectedComment : Maybe CommentData
    , treeViewActive : Bool
    }

type alias Model =
    { jsonInput : Maybe String
    , commentThread : Result String CommentThread
    }

parseJsonAndUpdate : String -> Model -> Model
parseJsonAndUpdate json model =
    let
        commentThread = D.decodeString commentDecoder json -- Comment
            |> Result.map commentToTree -- T.Node CommentData
            |> Result.map (\commentDataNode -> TV.initializeModel configuration [ commentDataNode ]) -- TV.Model CommentData CommentUid
            |> Result.map (\treeViewModel -> CommentThread treeViewModel Nothing False)
            |> Result.mapError D.errorToString
    in
        { model
        | jsonInput = Just json
        , commentThread = commentThread
        }

commentUidLabel : CommentUid -> String
commentUidLabel (thread, sequence) =
    "[" ++ thread ++ ":" ++ (String.fromInt sequence) ++ "]"

commentDataLabel : CommentData -> String
commentDataLabel { author, message, uid } =
    (commentUidLabel uid) ++ " " ++ author ++ "> " ++ message

nodeLabel : T.Node CommentData -> String
nodeLabel node =
    commentDataLabel <| T.dataOf node

nodeUid : T.Node CommentData -> TV.NodeUid CommentUid
nodeUid node  =
    T.dataOf node |> .uid |> TV.NodeUid

configuration : TV.Configuration CommentData CommentUid
configuration =
    TV.Configuration nodeUid nodeLabel TV.defaultCssClasses

loadJsonRequest : Cmd Msg
loadJsonRequest =
    Http.get
        { url = "data/sample-comment-thread.json"
        , expect = Http.expectString JsonLoaded
        }

emptyModel : Model
emptyModel =
    { jsonInput = Nothing
    , commentThread = Result.Err "Empty JSON"
    }

initializeModel : (Model, Cmd Msg)
initializeModel =
    ( emptyModel
    , loadJsonRequest
    )

type Msg =
    TreeViewMsg (TV.Msg CommentUid)
    | ParseJson
    | JsonEdited String
    | JsonLoaded (Result.Result Http.Error String)

httpErrorToString : Http.Error -> String
httpErrorToString httpError =
    case httpError of
        Http.BadUrl string ->
            "Bad URL " ++ string
        Http.Timeout ->
            "Timeout"
        Http.NetworkError ->
            "Network error"
        Http.BadStatus int ->
            "Bad status " ++ (String.fromInt int)
        Http.BadBody string ->
            "Bad body " ++ string

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
    case message of
        JsonEdited jsonInput ->
            ( { model
              | jsonInput = Just jsonInput
              , commentThread =
                    Result.map (\cT -> { cT | treeViewActive = False }) model.commentThread
              }
            , Cmd.none
            )

        ParseJson ->
            ( Maybe.map (\jsonInput -> parseJsonAndUpdate jsonInput model) model.jsonInput
                |> Maybe.withDefault model
            , Cmd.none
            )

        JsonLoaded result ->
            case result of
                Result.Err httpError ->
                  ( { jsonInput = Nothing
                    , commentThread = Result.Err <| httpErrorToString httpError
                    }
                  , Cmd.none
                  )
                Result.Ok jsonInput ->
                  ( parseJsonAndUpdate jsonInput emptyModel
                  , Cmd.none
                  )

        TreeViewMsg tvMsg ->
            let
                commentThread =
                    model.commentThread
                        |> Result.map (\cT -> {cT | treeViewModel = TV.update tvMsg cT.treeViewModel, treeViewActive = True } )
                        |> Result.map (\cT -> {cT | selectedComment = TV.getSelected cT.treeViewModel |> Maybe.map .node |> Maybe.map T.dataOf })
            in
                ( { model
                  | commentThread = commentThread
                  }
                , Cmd.none
                )


jsonInputTextArea : Model -> Html Msg
jsonInputTextArea model =
    div
      []
      [ Mwc.TextField.view
          [ Mwc.TextField.onInput JsonEdited
          , Mwc.TextField.value <| Maybe.withDefault "" model.jsonInput
          , Mwc.TextField.textArea
          , Mwc.TextField.placeHolder "Comment JSON"
          ]
      ]

parseJsonButton : Model -> Html Msg
parseJsonButton model =
    div
      []
      [ Mwc.Button.view
          [ Mwc.Button.raised
          , Mwc.Button.onClick ParseJson
          , Mwc.Button.label "Parse JSON"
          , Mwc.Button.disabled (Maybe.map (\jsonInput -> False ) model.jsonInput |> Maybe.withDefault True)
          ]
      ]

selectedCommentDetails : CommentThread -> Html Msg
selectedCommentDetails commentThread =
    let
        details =
            commentThread.selectedComment
                |> Maybe.map commentDataLabel
                |> Maybe.withDefault "(nothing selected)"
    in
        div
            [ css [ width (px 600) ] ]
            [ Mwc.TextField.view
                [ Mwc.TextField.readonly True
                , Mwc.TextField.label details
                ]
            ]

errorDetailsArea : String -> Html Msg
errorDetailsArea error =
    div
        [ css [ width (px 600) ] ]
        [ Mwc.TextField.view
            [ Mwc.TextField.readonly True
            , Mwc.TextField.label error
            ]
        ]

commentTreeView : CommentThread -> Html Msg
commentTreeView commentThread =
    map TreeViewMsg (TV.view commentThread.treeViewModel |> fromUnstyled)

eitherFromResult : Result.Result a a -> a
eitherFromResult result =
    case result of
        Result.Ok v ->
            v
        Result.Err v ->
            v

commentThreadView : Model -> Html Msg
commentThreadView model =
    model.commentThread
        |> Result.map (\cT -> div [] [ selectedCommentDetails cT, commentTreeView cT ])
        |> Result.mapError (\error -> errorDetailsArea error)
        |> eitherFromResult

view : Model -> Html Msg
view model =
    div
      []
      [ jsonInputTextArea model
      , parseJsonButton model
      , commentThreadView model
      ]


subscriptions : Model -> Sub Msg
subscriptions model =
    model.commentThread
        |> Result.map (\cT ->
            if cT.treeViewActive then
                Sub.map TreeViewMsg (TV.subscriptions cT.treeViewModel)
            else
                Sub.none
            )
        |> Result.withDefault Sub.none
