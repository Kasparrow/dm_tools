module Models.Rules.RuleSetKind exposing (RuleSetKind(..), RuleSetKinds, all, fromString)


type RuleSetKind
    = DnD5
    | AiME


type alias RuleSetKinds =
    List RuleSetKind


all : RuleSetKinds
all =
    [ DnD5, AiME]


fromString : String -> RuleSetKind
fromString string =
    case string of
        "DnD5" ->
            DnD5

        "AiME" ->
            AiME

        _ ->
            DnD5
