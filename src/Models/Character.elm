module Models.Character exposing (Character)

import Models.Rules.Stat as Stat exposing (Stats)
import Models.Rules.Race as Race exposing (Race)
import Models.Rules.SubRace as SubRace exposing (SubRace)
import Models.Rules.Class as Class exposing (Class)
import Models.Rules.SkillKind as SkillKind exposing (SkillKinds)
import Models.Rules.Background as Background exposing (Background)

type alias Character = 
    { rolledStats: Stats
    , race: Race
    , subRace: SubRace
    , class: Class
    , background: Background
    , remainingPoints: Int
    , level: Int
    , selectedProficiencySkills: SkillKinds
    }
