module Components.Atoms.DataDisplay exposing (valueBox, statReader)

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (..)

import Models.Msg as Msg exposing (Msg(..))

valueBox: String -> String -> Html Msg
valueBox title value =
    div [ class "stat-box" ]
        [ span [ class "stat-box-title" ] [ text title ]
        , div [ class "stat-box-body" ]
              [ span [ class "stat-box-value" ] [ text value ]]
        ]

statReader: String -> String -> String -> Html Msg
statReader name value bonus =
    div [ class "stat-box" ]
        [ span [ class "stat-box-title" ] [ text name ]
        , div [ class "stat-box-body" ]
              [ span [ class "stat-box-value" ] [ text value ]
              , div [ class "stat-box-bonus" ]
                    [ span [] [ text bonus ]
                    , span [] []]
              ]
        ]
