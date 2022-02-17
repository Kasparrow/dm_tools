module Models.Rules.Race exposing (Race, Races, get)

import Models.Rules.SkillKind exposing (SkillKind(..), SkillKinds)
import Models.Rules.StatKind exposing (StatKind(..))
import Models.Rules.RuleSetKind exposing (RuleSetKind(..), RuleSetKinds)
import Models.Rules.RaceKind exposing (RaceKind(..))
import Models.Rules.SubRaceKind exposing (SubRaceKind(..), SubRaceKinds)

import Models.Rules.Stat exposing (Stats)

type alias Race =
    { raceKind: RaceKind
    , statBonus: Stats
    , subRaces: SubRaceKinds
    , baseProficiencySkills: SkillKinds
    , ruleSetKinds: RuleSetKinds
    , asString: String
    }
type alias Races = List Race

get: RaceKind -> Race
get raceKind =
    case raceKind of
        Dragonborn ->
            { raceKind = Dragonborn
            , statBonus = [ (Strength, 2), (Charisma, 1) ]
            , subRaces = 
                [ NoSubRace
                , BlackDragonborn
                , BlueDragonborn
                , BrassDragonborn
                , BronzeDragonborn
                , CopperDragonborn
                , GoldDragonborn
                , GreenDragonborn
                , RedDragonborn
                , SilverDragonborn
                , WhiteDragonborn
                ]
            , baseProficiencySkills = []
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Dragonborn"
            }
        Dwarf ->
            { raceKind = Dwarf
            , statBonus = [ (Constitution, 2) ]
            , subRaces = [NoSubRace, HillsDwarf, MountainsDwarf]
            , baseProficiencySkills = []
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Dwarf"
            }
        Elf ->
            { raceKind = Elf
            , statBonus = [ (Dexterity, 2)]
            , subRaces = [ NoSubRace, Drow, WoodElf, HighElf ]
            , baseProficiencySkills = [Perception]
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Elf"
            }
        Gnome ->
            { raceKind = Gnome
            , statBonus = [ (Intelligence, 2) ]
            , subRaces = [ NoSubRace, DeepGnome, RockGnome ]
            , baseProficiencySkills = []
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Gnome"
            }
        HalfElf ->
            { raceKind = HalfElf
            , statBonus = [ (Charisma, 2) ]
            , subRaces = []
            , baseProficiencySkills = []
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Half-Elf"
            }
        Halfling ->
            { raceKind = Halfling
            , statBonus = [ (Dexterity, 2) ]
            , subRaces = [ NoSubRace, LightfootHalfling, StoutHalfling ]
            , baseProficiencySkills = []
            , ruleSetKinds = [DnD5, Laelith ]
            , asString = "Halfling"
            }
        HalfOrc ->
            { raceKind = HalfOrc
            , statBonus = [ (Strength, 2), (Constitution, 1) ]
            , subRaces = []
            , baseProficiencySkills = [Intimidation]
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Half-Orc"
            }
        Human ->
            { raceKind = Human
            , statBonus =
                [ (Strength, 1)
                , (Dexterity, 1)
                , (Constitution, 1)
                , (Intelligence, 1)
                , (Wisdom, 1)
                , (Charisma, 1)
                ]
            , subRaces = []
            , baseProficiencySkills = []
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Human"
            }
        Tiefling ->
            { raceKind = Tiefling
            , statBonus = [ (Intelligence, 1), (Charisma, 2) ]
            , subRaces = []
            , baseProficiencySkills = []
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Tiefling"
            }
        Barding ->
            { raceKind = Barding
            , statBonus = [ (Constitution, 1) ]
            , subRaces = []
            , baseProficiencySkills = [Insight]
            , ruleSetKinds = [AiME]
            , asString = "Barding"
            }
        Beorning ->
            { raceKind = Beorning
            , statBonus = [ (Strength, 1) ]
            , subRaces = []
            , baseProficiencySkills = [Intimidation]
            , ruleSetKinds = [AiME]
            , asString = "Beorning"
            }
        Dunedain ->
            { raceKind = Dunedain
            , statBonus = [ (Constitution, 1), (Wisdom, 1) ]
            , subRaces = []
            , baseProficiencySkills = [Survival]
            , ruleSetKinds = [AiME]
            , asString = "Dunedain"
            }
        LonelyMountainDwarf ->
            { raceKind = LonelyMountainDwarf
            , statBonus = [ (Constitution, 2) ]
            , subRaces = []
            , baseProficiencySkills = []
            , ruleSetKinds = [AiME]
            , asString = "Lonely Mountain Dwarf"
            }
        MirkwoodElf ->
            { raceKind = MirkwoodElf
            , statBonus = [ (Dexterity, 2), (Wisdom, 1) ]
            , subRaces = []
            , baseProficiencySkills = [Stealth]
            , ruleSetKinds = [AiME]
            , asString = "Mirkwood Elf"
            }
        ShireHobbit ->
            { raceKind = ShireHobbit
            , statBonus = [ (Dexterity, 2) ]
            , subRaces = []
            , baseProficiencySkills = [Stealth]
            , ruleSetKinds = [AiME]
            , asString = "Shire Hobbit"
            }
        BreeMen ->
            { raceKind = BreeMen
            , statBonus = [ (Wisdom, 1) ]
            , subRaces = []
            , baseProficiencySkills = [Perception]
            , ruleSetKinds = [AiME]
            , asString = "Bree Men"
            }
        LakeMen ->
            { raceKind = LakeMen
            , statBonus = [ (Charisma, 1) ]
            , subRaces = []
            , baseProficiencySkills = [Persuasion]
            , ruleSetKinds = [AiME]
            , asString = "Lake Men"
            }
        MinasTirithMen ->
            { raceKind = MinasTirithMen
            , statBonus = [ (Intelligence, 1) ]
            , subRaces = []
            , baseProficiencySkills = [History]
            , ruleSetKinds = [AiME]
            , asString = "Minas Tirith Men"
            }
        RohanRider ->
            { raceKind = RohanRider
            , statBonus = [ (Wisdom, 1)]
            , subRaces = []
            , baseProficiencySkills = [AnimalHandling]
            , ruleSetKinds = [AiME]
            , asString = "Rohan Rider"
            }
        WilderlandWoodmen ->
            { raceKind = WilderlandWoodmen
            , statBonus = [ (Dexterity, 1) ]
            , subRaces = []
            , baseProficiencySkills = [Survival]
            , ruleSetKinds = [AiME]
            , asString = "Wilderland Woodmen"
            }
        NoRace ->
            { raceKind = NoRace
            , statBonus = []
            , subRaces = []
            , baseProficiencySkills = []
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = ""
            }

