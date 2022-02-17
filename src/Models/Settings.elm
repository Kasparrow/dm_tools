module Models.Settings exposing (Settings)

import Models.RuleSetKind as RuleSetKind exposing (RuleSetKind)

type alias Settings =
    { ruleSetKind: RuleSetKind
    , freeStatsInput: Bool
    }
