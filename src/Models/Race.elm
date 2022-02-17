
type alias Race =
    { raceKind: RaceKind
    , statBonus: Stats
    , subRaces: SubRaceIdentifiers
    , baseProficiencySkills: SkillIdentifiers
    , ruleSetKinds: RuleSetKinds
    , asString: String
    }
type alias Races = List Race
