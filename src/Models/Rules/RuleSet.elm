module Models.Rules.RuleSet exposing (RuleSet, RuleSets, get)

import Models.Rules.ClassKind exposing (ClassKind(..), ClassKinds)
import Models.Rules.RaceKind exposing (RaceKind(..), RaceKinds)
import Models.Rules.RuleSetKind exposing (RuleSetKind(..), RuleSetKinds)
import Models.Rules.SkillKind exposing (SkillKind(..), SkillKinds)
import Models.Rules.SubRaceKind exposing (SubRaceKind(..), SubRaceKinds)


type alias RuleSet =
    { classKinds : ClassKinds
    , raceKinds : RaceKinds
    , subRaceKinds : SubRaceKinds
    , skillKinds : SkillKinds
    }


type alias RuleSets =
    List RuleSet


get : RuleSetKind -> RuleSet
get ruleSetKind =
    case ruleSetKind of
        DnD5 ->
            { classKinds =
                [ Barbarian
                , Bard
                , Cleric
                , Druid
                , Fighter
                , Monk
                , Paladin
                , Ranger
                , Rogue
                , Sorcerer
                , Warlock
                , Wizard
                , Scholar
                ]
            , raceKinds = [ Dragonborn, Dwarf, Elf, Gnome, HalfElf, Halfling, HalfOrc, Human, Tiefling ]
            , subRaceKinds =
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
                , HillsDwarf
                , MountainsDwarf
                , HighElf
                , WoodElf
                , Drow
                , DeepGnome
                , RockGnome
                , LightfootHalfling
                , StoutHalfling
                ]
            , skillKinds =
                [ Acrobatics
                , AnimalHandling
                , Arcana
                , Athletics
                , Deception
                , History
                , Insight
                , Intimidation
                , Investigation
                , Medicine
                , Nature
                , Perception
                , Performance
                , Persuasion
                , Religion
                , SleightOfHand
                , Stealth
                , Survival
                ]
            }

        AiME ->
            { classKinds = [ Slayer, TreasureHunter, Wanderer, Warden, Warrior ]
            , raceKinds =
                [ Barding
                , Beorning
                , Dunedain
                , LonelyMountainDwarf
                , MirkwoodElf
                , ShireHobbit
                , BreeMen
                , LakeMen
                , MinasTirithMen
                , RohanRider
                , WilderlandWoodmen
                ]
            , subRaceKinds = []
            , skillKinds =
                [ Acrobatics
                , AnimalHandling
                , Athletics
                , Deception
                , History
                , Insight
                , Intimidation
                , Investigation
                , Lore
                , Medicine
                , Nature
                , Perception
                , Performance
                , Persuasion
                , Riddle
                , ShadowLore
                , SleightOfHand
                , Stealth
                , Survival
                , Traditions
                ]
            }
