module Models.Character exposing (Character)

import Models.Stat as Stat exposing (Stats)
import Models.Race as Race exposing (Race)
import Models.SubRace as SubRace exposing (SubRace)
import Models.Class as Class exposing (Class)
import Models.SkillKind as SkillKind exposing (SkillKinds)

type alias Character = 
    { rolledStats: Stats
    , race: Race
    , subRace: SubRace
    , class: Class
    , remainingPoints: Int
    , level: Int
    , selectedProficiencySkills: SkillKinds
    }
