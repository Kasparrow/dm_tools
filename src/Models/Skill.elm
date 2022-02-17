module Models.Skill exposing (Skill, Skills, get)

import Models.SkillKind exposing (SkillKind(..))
import Models.StatKind exposing (StatKind(..))
import Models.RuleSetKind exposing (RuleSetKind(..), RuleSetKinds)

type alias Skill = 
    { identifier: SkillKind
    , statKind: StatKind
    , ruleSetKinds: RuleSetKinds
    , asString: String
    }

type alias Skills = List Skill

get: SkillKind -> Skill
get skillKind =
    case skillKind of
        Acrobatics ->
            { identifier = Acrobatics
            , statKind = Dexterity
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = "Acrobatics"
            }
        AnimalHandling ->
            { identifier = AnimalHandling
            , statKind = Wisdom
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = "Animal Handling"
            }
        Arcana ->
            { identifier = Arcana
            , statKind = Intelligence
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Arcana"
            }
        Athletics ->
            { identifier = Athletics
            , statKind = Strength
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = "Athletics"
            }
        Deception ->
            { identifier = Deception
            , statKind = Charisma
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = "Deception"
            }
        History ->
            { identifier = History
            , statKind = Intelligence
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = "History"
            }
        Insight ->
            { identifier = Insight
            , statKind = Wisdom
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = "Insight"
            }
        Intimidation ->
            { identifier = Intimidation
            , statKind = Charisma
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = "Intimidation"
            }
        Investigation ->
            { identifier = Investigation
            , statKind = Intelligence
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = "Investigation"
            }
        Lore ->
            { identifier = Lore
            , statKind = Intelligence
            , ruleSetKinds = [AiME]
            , asString = "Lore"
            }
        Medicine ->
            { identifier = Medicine
            , statKind = Wisdom
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = "Medicine"
            }
        Nature ->
            { identifier = Nature
            , statKind = Intelligence
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = "Nature"
            }
        Perception ->
            { identifier = Perception
            , statKind = Wisdom
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = "Perception"
            }
        Performance ->
            { identifier = Performance
            , statKind = Charisma
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = "Performance"
            }
        Persuasion ->
            { identifier = Persuasion
            , statKind = Charisma
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = "Persuasion"
            }
        Religion ->
            { identifier = Religion
            , statKind = Intelligence
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Religion"
            }
        Riddle ->
            { identifier = Riddle
            , statKind = Intelligence
            , ruleSetKinds = [AiME]
            , asString = "Riddle"
            }
        ShadowLore ->
            { identifier = ShadowLore
            , statKind = Intelligence
            , ruleSetKinds = [AiME]
            , asString = "Shadow Lore"
            }
        SleightOfHand ->
            { identifier = SleightOfHand
            , statKind = Dexterity
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = "Sleight of Hand"
            }
        Stealth ->
            { identifier = Stealth
            , statKind = Dexterity
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = "Stealth"
            }
        Survival ->
            { identifier = Survival
            , statKind = Wisdom
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = "Survival"
            }
        Traditions ->
            { identifier = Traditions
            , statKind = Intelligence
            , ruleSetKinds = [AiME]
            , asString = "Traditions"
            }
        NoSkill ->
            { identifier = NoSkill
            , statKind = Strength
            , ruleSetKinds = []
            , asString = ""
            }
