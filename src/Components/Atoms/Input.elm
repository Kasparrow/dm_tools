module Components.Atoms.Input exposing (entitySelector, statInput)

import Html exposing (Html, div, h3, option, select, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
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


entitySelector : String -> List entityKind -> Maybe entityKind -> (String -> Msg) -> (entityKind -> { entity | asString : String }) -> Html Msg
entitySelector entityName entityKinds selectedKind handler getter =
    if List.length entityKinds > 0 then
        div [ class "content" ]
            [ h3 [] [ text entityName ]
            , select [ onInput handler ]
                (option [ value "", selected True, disabled True, hidden True ] [ text ("Select a " ++ entityName) ]
                    :: List.map (\entityKind -> viewEntityOption entityKind getter) entityKinds
                )
            ]

    else
        Html.text ""


viewEntityOption : entityKind -> (entityKind -> { entity | asString : String }) -> Html Msg
viewEntityOption kind getter =
    let
        label =
            (getter kind).asString
    in
    option [ value (stringToId label) ] [ text label ]


stringToId : String -> String
stringToId string =
    String.replace "-" "" (String.replace " " "" string)
