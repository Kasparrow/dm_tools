module Components.Atoms.Input exposing (statInput)

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Models.Msg as Msg exposing (Msg(..))


statInput : String -> Int -> Msg -> Msg -> Html Msg
statInput name value onIncrement onDecrease =
    div [ class "stat-box" ]
        [ span [ class "stat-box-title" ] [ text name ]
        , div [ class "stat-box-body" ]
            [ span [ class "stat-box-value" ] [ text (String.fromInt value) ]
            , div [ class "stat-box-controls" ]
                [ span [ onClick onIncrement ] [ text "+" ]
                , span [ onClick onDecrease ] [ text "-" ]
                ]
            ]
        ]
