module Models.Rules.Class exposing (Class, Classes, get)

import Models.Rules.ClassKind as ClassKind exposing (ClassKind(..), ClassKinds)
import Models.Rules.RuleSetKind as RuleSetKing exposing (RuleSetKind(..), RuleSetKinds)
import Models.Rules.SkillKind as SkillKind exposing (SkillKind(..), SkillKinds, all)
import Models.Rules.StatKind as StatKind exposing (StatKind(..), StatKinds)


type alias Class =
    { classKind : ClassKind
    , proficiencySaves : StatKinds
    , baseProficiencySkills : SkillKinds
    , optionalProficiencySkills : SkillKinds
    , optionalProficiencySkillsLimit : Int
    , lifeDice : Int
    , ruleSetKinds : RuleSetKinds
    , asString : String
    }


type alias Classes =
    List Class


get : ClassKind -> Class
get classKind =
    case classKind of
        Barbarian ->
            { classKind = Barbarian
            , proficiencySaves = [ Strength, Constitution ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ AnimalHandling, Athletics, Intimidation, Nature, Perception, Survival ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 12
            , ruleSetKinds = [ DnD5, Laelith ]
            , asString = "Barbarian"
            }

        Bard ->
            { classKind = Bard
            , proficiencySaves = [ Dexterity, Charisma ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = SkillKind.all
            , optionalProficiencySkillsLimit = 3
            , lifeDice = 8
            , ruleSetKinds = [ DnD5, Laelith ]
            , asString = "Bard"
            }

        Cleric ->
            { classKind = Cleric
            , proficiencySaves = [ Wisdom, Charisma ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ History, Insight, Medicine, Persuasion, Religion ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 8
            , ruleSetKinds = [ DnD5, Laelith ]
            , asString = "Cleric"
            }

        Druid ->
            { classKind = Druid
            , proficiencySaves = [ Intelligence, Wisdom ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ Arcana, AnimalHandling, Insight, Medicine, Nature, Perception, Religion, Survival ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 8
            , ruleSetKinds = [ DnD5, Laelith ]
            , asString = "Druid"
            }

        Fighter ->
            { classKind = Fighter
            , proficiencySaves = [ Strength, Constitution ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ Acrobatics, AnimalHandling, Athletics, History, Insight, Intimidation, Perception, Survival ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 10
            , ruleSetKinds = [ DnD5, Laelith ]
            , asString = "Fighter"
            }

        Monk ->
            { classKind = Monk
            , proficiencySaves = [ Strength, Dexterity ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ Acrobatics, Athletics, History, Insight, Religion, Stealth ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 8
            , ruleSetKinds = [ DnD5, Laelith ]
            , asString = "Monk"
            }

        Paladin ->
            { classKind = Paladin
            , proficiencySaves = [ Wisdom, Charisma ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ Athletics, Insight, Intimidation, Medicine, Persuasion ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 10
            , ruleSetKinds = [ DnD5, Laelith ]
            , asString = "Paladin"
            }

        Ranger ->
            { classKind = Ranger
            , proficiencySaves = [ Strength, Dexterity ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ AnimalHandling, Athletics, Insight, Investigation, Nature, Perception, Stealth, Survival ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 10
            , ruleSetKinds = [ DnD5, Laelith ]
            , asString = "Ranger"
            }

        Rogue ->
            { classKind = Rogue
            , proficiencySaves = [ Dexterity, Intelligence ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ Acrobatics, Athletics, Deception, Insight, Intimidation, Investigation, Perception, Performance, Persuasion, SleightOfHand, Stealth ]
            , optionalProficiencySkillsLimit = 4
            , lifeDice = 8
            , ruleSetKinds = [ DnD5, Laelith ]
            , asString = "Rogue"
            }

        Sorcerer ->
            { classKind = Sorcerer
            , proficiencySaves = [ Constitution, Charisma ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ Arcana, Deception, Insight, Intimidation, Persuasion ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 8
            , ruleSetKinds = [ DnD5, Laelith ]
            , asString = "Sorcerer"
            }

        Warlock ->
            { classKind = Warlock
            , proficiencySaves = [ Wisdom, Charisma ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ Arcana, Deception, History, Intimidation, Investigation, Nature, Religion ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 6
            , ruleSetKinds = [ DnD5, Laelith ]
            , asString = "Warlock"
            }

        Wizard ->
            { classKind = Wizard
            , proficiencySaves = [ Intelligence, Wisdom ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ Arcana, History, Insight, Investigation, Medicine, Religion ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 6
            , ruleSetKinds = [ DnD5, Laelith ]
            , asString = "Wizard"
            }

        Scholar ->
            { classKind = Scholar
            , proficiencySaves = [ Intelligence, Wisdom ]
            , baseProficiencySkills = [ Medicine, Lore ]
            , optionalProficiencySkills = [ History, Riddle, Traditions, Insight, Investigation, Nature, Perception, Survival ]
            , optionalProficiencySkillsLimit = 1
            , lifeDice = 8
            , ruleSetKinds = [ AiME ]
            , asString = "Scholar"
            }

        Slayer ->
            { classKind = Slayer
            , proficiencySaves = [ Strength, Constitution ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ AnimalHandling, Athletics, Intimidation, Nature, Perception, Survival ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 12
            , ruleSetKinds = [ AiME ]
            , asString = "Slayer"
            }

        TreasureHunter ->
            { classKind = TreasureHunter
            , proficiencySaves = [ Dexterity, Intelligence ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ Acrobatics, Athletics, Deception, Insight, Intimidation, Perception, Persuasion, Riddle, SleightOfHand, Stealth ]
            , optionalProficiencySkillsLimit = 4
            , lifeDice = 8
            , ruleSetKinds = [ AiME ]
            , asString = "Treasure Hunter"
            }

        Wanderer ->
            { classKind = Wanderer
            , proficiencySaves = [ Strength, Constitution ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ AnimalHandling, Athletics, Insight, Investigation, Nature, Perception, Stealth, Traditions ]
            , optionalProficiencySkillsLimit = 3
            , lifeDice = 10
            , ruleSetKinds = [ AiME ]
            , asString = "Wanderer"
            }

        Warden ->
            { classKind = Warden
            , proficiencySaves = [ Dexterity, Charisma ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = SkillKind.all
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 8
            , ruleSetKinds = [ AiME ]
            , asString = "Warden"
            }

        Warrior ->
            { classKind = Warrior
            , baseProficiencySkills = []
            , proficiencySaves = [ Strength, Constitution ]
            , optionalProficiencySkills = [ Acrobatics, AnimalHandling, Athletics, History, Insight, Intimidation, Perception, Survival, Traditions ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 10
            , ruleSetKinds = [ AiME ]
            , asString = "Warrior"
            }

        NoClass ->
            { classKind = NoClass
            , baseProficiencySkills = []
            , proficiencySaves = []
            , optionalProficiencySkills = []
            , optionalProficiencySkillsLimit = 0
            , lifeDice = 0
            , ruleSetKinds = [ DnD5, Laelith, AiME ]
            , asString = ""
            }
