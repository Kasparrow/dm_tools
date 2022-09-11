module Models.Rules.Class exposing (Class, Classes, get, none)

import Models.Rules.ClassKind as ClassKind exposing (ClassKind(..), ClassKinds)
import Models.Rules.RuleSetKind as RuleSetKing exposing (RuleSetKind(..), RuleSetKinds)
import Models.Rules.SkillKind as SkillKind exposing (SkillKind(..), SkillKinds, all)
import Models.Rules.StatKind as StatKind exposing (StatKind(..), StatKinds)


type alias Class =
    { proficiencySaves : StatKinds
    , baseProficiencySkills : SkillKinds
    , optionalProficiencySkills : SkillKinds
    , optionalProficiencySkillsLimit : Int
    , lifeDice : Int
    , asString : String
    }


type alias Classes =
    List Class


get : ClassKind -> Class
get classKind =
    case classKind of
        Barbarian ->
            { proficiencySaves = [ Strength, Constitution ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ AnimalHandling, Athletics, Intimidation, Nature, Perception, Survival ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 12
            , asString = "Barbarian"
            }

        Bard ->
            { proficiencySaves = [ Dexterity, Charisma ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = SkillKind.all
            , optionalProficiencySkillsLimit = 3
            , lifeDice = 8
            , asString = "Bard"
            }

        Cleric ->
            { proficiencySaves = [ Wisdom, Charisma ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ History, Insight, Medicine, Persuasion, Religion ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 8
            , asString = "Cleric"
            }

        Druid ->
            { proficiencySaves = [ Intelligence, Wisdom ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ Arcana, AnimalHandling, Insight, Medicine, Nature, Perception, Religion, Survival ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 8
            , asString = "Druid"
            }

        Fighter ->
            { proficiencySaves = [ Strength, Constitution ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ Acrobatics, AnimalHandling, Athletics, History, Insight, Intimidation, Perception, Survival ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 10
            , asString = "Fighter"
            }

        Monk ->
            { proficiencySaves = [ Strength, Dexterity ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ Acrobatics, Athletics, History, Insight, Religion, Stealth ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 8
            , asString = "Monk"
            }

        Paladin ->
            { proficiencySaves = [ Wisdom, Charisma ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ Athletics, Insight, Intimidation, Medicine, Persuasion ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 10
            , asString = "Paladin"
            }

        Ranger ->
            { proficiencySaves = [ Strength, Dexterity ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ AnimalHandling, Athletics, Insight, Investigation, Nature, Perception, Stealth, Survival ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 10
            , asString = "Ranger"
            }

        Rogue ->
            { proficiencySaves = [ Dexterity, Intelligence ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ Acrobatics, Athletics, Deception, Insight, Intimidation, Investigation, Perception, Performance, Persuasion, SleightOfHand, Stealth ]
            , optionalProficiencySkillsLimit = 4
            , lifeDice = 8
            , asString = "Rogue"
            }

        Sorcerer ->
            { proficiencySaves = [ Constitution, Charisma ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ Arcana, Deception, Insight, Intimidation, Persuasion ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 8
            , asString = "Sorcerer"
            }

        Warlock ->
            { proficiencySaves = [ Wisdom, Charisma ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ Arcana, Deception, History, Intimidation, Investigation, Nature, Religion ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 6
            , asString = "Warlock"
            }

        Wizard ->
            { proficiencySaves = [ Intelligence, Wisdom ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ Arcana, History, Insight, Investigation, Medicine, Religion ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 6
            , asString = "Wizard"
            }

        Scholar ->
            { proficiencySaves = [ Intelligence, Wisdom ]
            , baseProficiencySkills = [ Medicine, Lore ]
            , optionalProficiencySkills = [ History, Riddle, Traditions, Insight, Investigation, Nature, Perception, Survival ]
            , optionalProficiencySkillsLimit = 1
            , lifeDice = 8
            , asString = "Scholar"
            }

        Slayer ->
            { proficiencySaves = [ Strength, Constitution ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ AnimalHandling, Athletics, Intimidation, Nature, Perception, Survival ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 12
            , asString = "Slayer"
            }

        TreasureHunter ->
            { proficiencySaves = [ Dexterity, Intelligence ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ Acrobatics, Athletics, Deception, Insight, Intimidation, Perception, Persuasion, Riddle, SleightOfHand, Stealth ]
            , optionalProficiencySkillsLimit = 4
            , lifeDice = 8
            , asString = "Treasure Hunter"
            }

        Wanderer ->
            { proficiencySaves = [ Strength, Constitution ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ AnimalHandling, Athletics, Insight, Investigation, Nature, Perception, Stealth, Traditions ]
            , optionalProficiencySkillsLimit = 3
            , lifeDice = 10
            , asString = "Wanderer"
            }

        Warden ->
            { proficiencySaves = [ Dexterity, Charisma ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = SkillKind.all
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 8
            , asString = "Warden"
            }

        Warrior ->
            { baseProficiencySkills = []
            , proficiencySaves = [ Strength, Constitution ]
            , optionalProficiencySkills = [ Acrobatics, AnimalHandling, Athletics, History, Insight, Intimidation, Perception, Survival, Traditions ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 10
            , asString = "Warrior"
            }


none =
    { baseProficiencySkills = []
    , proficiencySaves = []
    , optionalProficiencySkills = []
    , optionalProficiencySkillsLimit = 0
    , lifeDice = 0
    , asString = ""
    }
