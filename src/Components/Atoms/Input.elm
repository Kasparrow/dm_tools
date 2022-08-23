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


entitySelector : List a -> a -> a -> String -> (String -> Msg) -> (a -> { b | asString : String }) -> Html Msg
entitySelector entityKinds selectedKind noKind entityName handler getter =
    if List.length entityKinds > 0 then
        div [ class "content" ]
            [ h3 [] [ text entityName ]
            , select [ onInput handler ]
                (List.map (\entityKind -> viewEntityOption getter entityKind selectedKind noKind entityName) entityKinds)
            ]

    else
        Html.text ""


viewEntityOption : (a -> { b | asString : String }) -> a -> a -> a -> String -> Html Msg
viewEntityOption getter kind selectedKind noKind entityName =
    let
        label =
            (getter kind).asString
    in
    if kind == noKind then
        option [ value "", selected (selectedKind == noKind) ] [ text ("Select a " ++ entityName) ]

    else
        option [ value (stringToId label) ] [ text label ]


stringToId : String -> String
stringToId string =
    String.replace "-" "" (String.replace " " "" string)
