module Main exposing (main)

import Browser
import Css exposing (Color, hex, px, rgb, width)
import Html.Styled exposing (a, Html, div, footer, h1, header, main_, map, text, toUnstyled)
import Html.Styled.Attributes exposing (css, href, src)
import SimpleTreeTab
import RecursiveJsonTreeTab
import Mwc.Button
import Mwc.Tabs
import Mwc.TextField


type alias Model =
    { currentTab : Int
    , simpleTreeModel : SimpleTreeTab.Model
    , recursiveJsonTreeModel : RecursiveJsonTreeTab.Model
    }


initModel : () -> (Model, Cmd Msg)
initModel () =
    let
        (recursiveJsonTreeModel, recursiveJsonTreeCmd) =
            RecursiveJsonTreeTab.initializeModel
    in
        ( { currentTab = 1
          , simpleTreeModel = SimpleTreeTab.initialModel
          , recursiveJsonTreeModel = recursiveJsonTreeModel
          }
        , Cmd.map RecurseJsonTreeMsg recursiveJsonTreeCmd
        )


type Msg
    = SelectTab Int
    | SimpleTreeMsg SimpleTreeTab.Msg
    | RecurseJsonTreeMsg RecursiveJsonTreeTab.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SelectTab newTab ->
            ( { model | currentTab = newTab }
            , Cmd.none
            )

        SimpleTreeMsg simpleTreeMsg ->
            ( { model | simpleTreeModel = SimpleTreeTab.update simpleTreeMsg model.simpleTreeModel }
            , Cmd.none
            )

        RecurseJsonTreeMsg recurseJsonTreeMsg ->
            let
                (recursiveJsonTreeModel, recursiveJsonTreeCmd) =
                    RecursiveJsonTreeTab.update recurseJsonTreeMsg model.recursiveJsonTreeModel
            in
                ( { model | recursiveJsonTreeModel = recursiveJsonTreeModel }
                , Cmd.map RecurseJsonTreeMsg recursiveJsonTreeCmd
                )


{-| A plain old record holding a couple of theme colors.
-}
theme : { secondary : Color, primary : Color }
theme =
    { primary = hex "55af6a"
    , secondary = rgb 250 240 230
    }

referenceLinks : Html Msg
referenceLinks =
    div
      []
      [ text "See "
      , a
          [ href "https://github.com/dosarf/elm-tree-view/tree/master/demo" ]
          [ text "demo" ]
      , text " for Elm package "
      , a
          [ href "https://package.elm-lang.org/packages/dosarf/elm-tree-view/latest" ]
          [ text "'dosarf/elm-tree-view'" ]
      ]

view : Model -> Html Msg
view model =
    main_ []
        [ header
            []
            [ h1 [] [ text "Elm TreeView demo" ] ]
        , div
            [ css [ width (px 600) ] ]
            [ Mwc.Tabs.view
                [ Mwc.Tabs.selected model.currentTab
                , Mwc.Tabs.onClick SelectTab
                , Mwc.Tabs.tabText
                    [ text "Simple TreeView"
                    , text "Recursive Json TreeView"
                    ]
                ]
            , tabContentView model
            ]
        , footer
            []
            [ referenceLinks ]
        ]


tabContentView : Model -> Html Msg
tabContentView model =
    case model.currentTab of
        0 ->
            map SimpleTreeMsg (SimpleTreeTab.view model.simpleTreeModel)

        _ ->
            map RecurseJsonTreeMsg (RecursiveJsonTreeTab.view model.recursiveJsonTreeModel)

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.currentTab of
        0 ->
            Sub.map SimpleTreeMsg (SimpleTreeTab.subscriptions model.simpleTreeModel)

        _ ->
            Sub.map RecurseJsonTreeMsg (RecursiveJsonTreeTab.subscriptions model.recursiveJsonTreeModel)

main =
    Browser.element
        { init = initModel
        , update = update
        , subscriptions = subscriptions
        , view = view >> toUnstyled
        }
