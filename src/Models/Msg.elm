module Models.Msg exposing (Msg(..))

import Models.SkillKind as SkillKind exposing (SkillKind)
import Models.StatKind as StatKind exposing (StatKind)

type Msg
    = UpdateRuleSet String
    | UpdateRemainingPoints
    | UpdateRace String
    | UpdateSubRace String
    | UpdateClass String
    | UpdateLevel String
    | UpdateStat StatKind Int
    | CheckFreeStatInput Bool
    | CheckProficiencySkill SkillKind Bool 
