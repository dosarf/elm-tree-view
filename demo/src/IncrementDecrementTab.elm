module IncrementDecrementTab exposing (Model, Msg, initialModel, update, view)

import Css exposing (px, width)
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Mwc.Button
import Mwc.TextField


type alias Model =
    { count : Int }


initialModel : Model
initialModel =
    { count = 0 }


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }

        Decrement ->
            { model | count = model.count - 1 }


view : Model -> Html Msg
view model =
    div
        [ css [ width (px 300) ] ]
        [ Mwc.TextField.view
            [ Mwc.TextField.readonly True
            , Mwc.TextField.label <| String.fromInt model.count
            ]
        , Mwc.Button.view
            [ Mwc.Button.raised
            , Mwc.Button.onClick Increment
            , Mwc.Button.label "increment"
            ]
        , Mwc.Button.view
            [ Mwc.Button.raised
            , Mwc.Button.onClick Decrement
            , Mwc.Button.label "decrement"
            ]
        ]
