module Models.Rules.Skill exposing (Skill, Skills, get)

import Models.Rules.SkillKind exposing (SkillKind(..))
import Models.Rules.StatKind exposing (StatKind(..))
import Models.Rules.RuleSetKind exposing (RuleSetKind(..), RuleSetKinds)

type alias Skill = 
    { skillKind: SkillKind
    , statKind: StatKind
    , ruleSetKinds: RuleSetKinds
    , asString: String
    }

type alias Skills = List Skill

get: SkillKind -> Skill
get skillKind =
    case skillKind of
        Acrobatics ->
            { skillKind = Acrobatics
            , statKind = Dexterity
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = "Acrobatics"
            }
        AnimalHandling ->
            { skillKind = AnimalHandling
            , statKind = Wisdom
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = "Animal Handling"
            }
        Arcana ->
            { skillKind = Arcana
            , statKind = Intelligence
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Arcana"
            }
        Athletics ->
            { skillKind = Athletics
            , statKind = Strength
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = "Athletics"
            }
        Deception ->
            { skillKind = Deception
            , statKind = Charisma
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = "Deception"
            }
        History ->
            { skillKind = History
            , statKind = Intelligence
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = "History"
            }
        Insight ->
            { skillKind = Insight
            , statKind = Wisdom
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = "Insight"
            }
        Intimidation ->
            { skillKind = Intimidation
            , statKind = Charisma
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = "Intimidation"
            }
        Investigation ->
            { skillKind = Investigation
            , statKind = Intelligence
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = "Investigation"
            }
        Lore ->
            { skillKind = Lore
            , statKind = Intelligence
            , ruleSetKinds = [AiME]
            , asString = "Lore"
            }
        Medicine ->
            { skillKind = Medicine
            , statKind = Wisdom
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = "Medicine"
            }
        Nature ->
            { skillKind = Nature
            , statKind = Intelligence
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = "Nature"
            }
        Perception ->
            { skillKind = Perception
            , statKind = Wisdom
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = "Perception"
            }
        Performance ->
            { skillKind = Performance
            , statKind = Charisma
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = "Performance"
            }
        Persuasion ->
            { skillKind = Persuasion
            , statKind = Charisma
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = "Persuasion"
            }
        Religion ->
            { skillKind = Religion
            , statKind = Intelligence
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Religion"
            }
        Riddle ->
            { skillKind = Riddle
            , statKind = Intelligence
            , ruleSetKinds = [AiME]
            , asString = "Riddle"
            }
        ShadowLore ->
            { skillKind = ShadowLore
            , statKind = Intelligence
            , ruleSetKinds = [AiME]
            , asString = "Shadow Lore"
            }
        SleightOfHand ->
            { skillKind = SleightOfHand
            , statKind = Dexterity
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = "Sleight of Hand"
            }
        Stealth ->
            { skillKind = Stealth
            , statKind = Dexterity
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = "Stealth"
            }
        Survival ->
            { skillKind = Survival
            , statKind = Wisdom
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = "Survival"
            }
        Traditions ->
            { skillKind = Traditions
            , statKind = Intelligence
            , ruleSetKinds = [AiME]
            , asString = "Traditions"
            }
        NoSkill ->
            { skillKind = NoSkill
            , statKind = Strength
            , ruleSetKinds = []
            , asString = ""
            }
