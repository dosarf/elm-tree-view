module Main exposing (main)

import Browser
import Css exposing (Color, hex, px, rgb, width)
import Html.Styled exposing (a, Html, div, footer, h1, header, main_, map, text, toUnstyled)
import Html.Styled.Attributes exposing (css, href, src)
import SimpleTreeTab
import Mwc.Button
import Mwc.Tabs
import Mwc.TextField


type alias Model =
    { currentTab : Int
    , simpleTreeModel : SimpleTreeTab.Model
    }


initModel : () -> (Model, Cmd Msg)
initModel () =
    ( { currentTab = 0
      , simpleTreeModel = SimpleTreeTab.initialModel
      }
    , Cmd.none
    )


type Msg
    = SelectTab Int
    | SimpleTreeMsg SimpleTreeTab.Msg


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
            [ css [ width (px 400) ] ]
            [ Mwc.Tabs.view
                [ Mwc.Tabs.selected model.currentTab
                , Mwc.Tabs.onClick SelectTab
                , Mwc.Tabs.tabText
                    [ text "Simple TreeView"
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
        _ ->
            map SimpleTreeMsg (SimpleTreeTab.view model.simpleTreeModel)

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.currentTab of
        _ ->
            Sub.map SimpleTreeMsg (SimpleTreeTab.subscriptions model.simpleTreeModel)

main =
    Browser.element
        { init = initModel
        , update = update
        , subscriptions = subscriptions
        , view = view >> toUnstyled
        }
