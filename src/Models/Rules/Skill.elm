module Models.Rules.Skill exposing (Skill, Skills, get, none)

import Models.Rules.RuleSetKind exposing (RuleSetKind(..), RuleSetKinds)
import Models.Rules.SkillKind exposing (SkillKind(..))
import Models.Rules.StatKind exposing (StatKind(..))


type alias Skill =
    { statKind : StatKind
    , asString : String
    }


type alias Skills =
    List Skill


get : SkillKind -> Skill
get skillKind =
    case skillKind of
        Acrobatics ->
            { statKind = Dexterity
            , asString = "Acrobatics"
            }

        AnimalHandling ->
            { statKind = Wisdom
            , asString = "Animal Handling"
            }

        Arcana ->
            { statKind = Intelligence
            , asString = "Arcana"
            }

        Athletics ->
            { statKind = Strength
            , asString = "Athletics"
            }

        Deception ->
            { statKind = Charisma
            , asString = "Deception"
            }

        History ->
            { statKind = Intelligence
            , asString = "History"
            }

        Insight ->
            { statKind = Wisdom
            , asString = "Insight"
            }

        Intimidation ->
            { statKind = Charisma
            , asString = "Intimidation"
            }

        Investigation ->
            { statKind = Intelligence
            , asString = "Investigation"
            }

        Lore ->
            { statKind = Intelligence
            , asString = "Lore"
            }

        Medicine ->
            { statKind = Wisdom
            , asString = "Medicine"
            }

        Nature ->
            { statKind = Intelligence
            , asString = "Nature"
            }

        Perception ->
            { statKind = Wisdom
            , asString = "Perception"
            }

        Performance ->
            { statKind = Charisma
            , asString = "Performance"
            }

        Persuasion ->
            { statKind = Charisma
            , asString = "Persuasion"
            }

        Religion ->
            { statKind = Intelligence
            , asString = "Religion"
            }

        Riddle ->
            { statKind = Intelligence
            , asString = "Riddle"
            }

        ShadowLore ->
            { statKind = Intelligence
            , asString = "Shadow Lore"
            }

        SleightOfHand ->
            { statKind = Dexterity
            , asString = "Sleight of Hand"
            }

        Stealth ->
            { statKind = Dexterity
            , asString = "Stealth"
            }

        Survival ->
            { statKind = Wisdom
            , asString = "Survival"
            }

        Traditions ->
            { statKind = Intelligence
            , asString = "Traditions"
            }


none =
    { statKind = Strength
    , asString = ""
    }
