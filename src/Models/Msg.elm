module Models.Msg exposing (Msg(..))

import Models.Rules.SkillKind as SkillKind exposing (SkillKind)
import Models.Rules.StatKind as StatKind exposing (StatKind)


type Msg
    = UpdateRuleSet String
    | UpdateRemainingPoints
    | UpdateRace String
    | UpdateSubRace String
    | UpdateClass String
    | UpdateBackground String
    | UpdateLevel String
    | UpdateStat StatKind Int
    | CheckFreeStatInput Bool
    | CheckProficiencySkill SkillKind Bool
