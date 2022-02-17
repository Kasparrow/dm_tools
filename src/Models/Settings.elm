module Models.Settings exposing (Settings)

import Models.Rules.RuleSetKind as RuleSetKind exposing (RuleSetKind)

type alias Settings =
    { ruleSetKind: RuleSetKind
    , freeStatsInput: Bool
    }
