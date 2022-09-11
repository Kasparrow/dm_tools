module Models.Rules.Race exposing (Race, Races, get, none)

import Models.Rules.RaceKind exposing (RaceKind(..))
import Models.Rules.RuleSetKind exposing (RuleSetKind(..), RuleSetKinds)
import Models.Rules.SkillKind exposing (SkillKind(..), SkillKinds)
import Models.Rules.Stat exposing (Stats)
import Models.Rules.StatKind exposing (StatKind(..))
import Models.Rules.SubRaceKind exposing (SubRaceKind(..), SubRaceKinds)


type alias Race =
    { statBonus : Stats
    , subRaces : SubRaceKinds
    , baseProficiencySkills : SkillKinds
    , asString : String
    }


type alias Races =
    List Race


get : RaceKind -> Race
get raceKind =
    case raceKind of
        Dragonborn ->
            { statBonus = [ ( Strength, 2 ), ( Charisma, 1 ) ]
            , subRaces =
                [ BlackDragonborn
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
            , asString = "Dragonborn"
            }

        Dwarf ->
            { statBonus = [ ( Constitution, 2 ) ]
            , subRaces = [ HillsDwarf, MountainsDwarf ]
            , baseProficiencySkills = []
            , asString = "Dwarf"
            }

        Elf ->
            { statBonus = [ ( Dexterity, 2 ) ]
            , subRaces = [ Drow, WoodElf, HighElf ]
            , baseProficiencySkills = [ Perception ]
            , asString = "Elf"
            }

        Gnome ->
            { statBonus = [ ( Intelligence, 2 ) ]
            , subRaces = [ DeepGnome, RockGnome ]
            , baseProficiencySkills = []
            , asString = "Gnome"
            }

        HalfElf ->
            { statBonus = [ ( Charisma, 2 ) ]
            , subRaces = []
            , baseProficiencySkills = []
            , asString = "Half-Elf"
            }

        Halfling ->
            { statBonus = [ ( Dexterity, 2 ) ]
            , subRaces = [ LightfootHalfling, StoutHalfling ]
            , baseProficiencySkills = []
            , asString = "Halfling"
            }

        HalfOrc ->
            { statBonus = [ ( Strength, 2 ), ( Constitution, 1 ) ]
            , subRaces = []
            , baseProficiencySkills = [ Intimidation ]
            , asString = "Half-Orc"
            }

        Human ->
            { statBonus =
                [ ( Strength, 1 )
                , ( Dexterity, 1 )
                , ( Constitution, 1 )
                , ( Intelligence, 1 )
                , ( Wisdom, 1 )
                , ( Charisma, 1 )
                ]
            , subRaces = []
            , baseProficiencySkills = []
            , asString = "Human"
            }

        Tiefling ->
            { statBonus = [ ( Intelligence, 1 ), ( Charisma, 2 ) ]
            , subRaces = []
            , baseProficiencySkills = []
            , asString = "Tiefling"
            }

        Barding ->
            { statBonus = [ ( Constitution, 1 ) ]
            , subRaces = []
            , baseProficiencySkills = [ Insight ]
            , asString = "Barding"
            }

        Beorning ->
            { statBonus = [ ( Strength, 1 ) ]
            , subRaces = []
            , baseProficiencySkills = [ Intimidation ]
            , asString = "Beorning"
            }

        Dunedain ->
            { statBonus = [ ( Constitution, 1 ), ( Wisdom, 1 ) ]
            , subRaces = []
            , baseProficiencySkills = [ Survival ]
            , asString = "Dunedain"
            }

        LonelyMountainDwarf ->
            { statBonus = [ ( Constitution, 2 ) ]
            , subRaces = []
            , baseProficiencySkills = []
            , asString = "Lonely Mountain Dwarf"
            }

        MirkwoodElf ->
            { statBonus = [ ( Dexterity, 2 ), ( Wisdom, 1 ) ]
            , subRaces = []
            , baseProficiencySkills = [ Stealth ]
            , asString = "Mirkwood Elf"
            }

        ShireHobbit ->
            { statBonus = [ ( Dexterity, 2 ) ]
            , subRaces = []
            , baseProficiencySkills = [ Stealth ]
            , asString = "Shire Hobbit"
            }

        BreeMen ->
            { statBonus = [ ( Wisdom, 1 ) ]
            , subRaces = []
            , baseProficiencySkills = [ Perception ]
            , asString = "Bree Men"
            }

        LakeMen ->
            { statBonus = [ ( Charisma, 1 ) ]
            , subRaces = []
            , baseProficiencySkills = [ Persuasion ]
            , asString = "Lake Men"
            }

        MinasTirithMen ->
            { statBonus = [ ( Intelligence, 1 ) ]
            , subRaces = []
            , baseProficiencySkills = [ History ]
            , asString = "Minas Tirith Men"
            }

        RohanRider ->
            { statBonus = [ ( Wisdom, 1 ) ]
            , subRaces = []
            , baseProficiencySkills = [ AnimalHandling ]
            , asString = "Rohan Rider"
            }

        WilderlandWoodmen ->
            { statBonus = [ ( Dexterity, 1 ) ]
            , subRaces = []
            , baseProficiencySkills = [ Survival ]
            , asString = "Wilderland Woodmen"
            }


none =
    { statBonus = []
    , subRaces = []
    , baseProficiencySkills = []
    , asString = ""
    }
