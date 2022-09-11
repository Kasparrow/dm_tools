module Models.Character exposing (Character)

import Models.Rules.BackgroundKind as BackgroundKind exposing (BackgroundKind)
import Models.Rules.ClassKind as ClassKind exposing (ClassKind)
import Models.Rules.RaceKind as RaceKind exposing (RaceKind)
import Models.Rules.SkillKind as SkillKind exposing (SkillKinds)
import Models.Rules.Stat as Stat exposing (Stats)
import Models.Rules.SubRaceKind as SubRaceKind exposing (SubRaceKind)


type alias Character =
    { rolledStats : Stats
    , raceKind : Maybe RaceKind
    , subRaceKind : Maybe SubRaceKind
    , classKind : Maybe ClassKind
    , backgroundKind : Maybe BackgroundKind
    , remainingPoints : Int
    , level : Int
    , selectedProficiencySkills : SkillKinds
    }
