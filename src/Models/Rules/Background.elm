module Models.Rules.Background exposing (Background, Backgrounds, get)

import Models.Rules.SkillKind as SkillKind exposing (SkillKind(..), SkillKinds, all)
import Models.Rules.BackgroundKind as BackgoundKind exposing (BackgroundKind(..), BackgroundKinds, all)
import Models.Rules.RuleSetKind exposing (RuleSetKind(..), RuleSetKinds)

type alias Background =
    { backgroundKind: BackgroundKind 
    , baseProficiencySkills: SkillKinds
    , ruleSetKinds: RuleSetKinds
    , asString: String
    }
type alias Backgrounds = List Background

get: BackgroundKind -> Background 
get backgroundKind =
    case backgroundKind of
        Acolyte ->
            { backgroundKind = Acolyte
            , baseProficiencySkills = [Insight, Religion]
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Acolyte"
            }
        Charlatan ->
            { backgroundKind = Charlatan
            , baseProficiencySkills = [Deception, SleightOfHand]
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Charlatan"
            }
        Criminal ->
            { backgroundKind = Criminal
            , baseProficiencySkills = [Deception, Stealth]
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Criminal"
            }
        Entertainer ->
            { backgroundKind = Entertainer
            , baseProficiencySkills =  [Acrobatics, Performance]
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Entertainer"
            }
        FolkHero ->
            { backgroundKind = FolkHero
            , baseProficiencySkills =  [AnimalHandling, Survival]
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Folk Hero"
            }
        GuildArtisan ->
            { backgroundKind = GuildArtisan
            , baseProficiencySkills = [Insight, Persuasion]
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Guild Artisan"
            }
        Hermit  ->
            { backgroundKind = Hermit
            , baseProficiencySkills = [Medicine, Religion]
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Hermit"
            }
        Noble ->
            { backgroundKind = Noble
            , baseProficiencySkills = [History, Persuasion]
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Noble"
            }
        Outlander ->
            { backgroundKind = Outlander
            , baseProficiencySkills = [Athletics, Survival]
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Outlander"
            }
        Sage ->
            { backgroundKind = Sage
            , baseProficiencySkills = [Arcana, History]
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Sage"
            }
        Sailor ->
            { backgroundKind = Sailor
            , baseProficiencySkills = [Athletics, Perception]
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Sailor"
            }
        Soldier ->
            { backgroundKind = Soldier
            , baseProficiencySkills = [Athletics, Intimidation]
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Soldier"
            }
        Urchin ->
            { backgroundKind = Urchin
            , baseProficiencySkills = [SleightOfHand, Stealth]
            , ruleSetKinds = [DnD5, Laelith]
            , asString = "Urchin"
            }
        NoBackground ->
            { backgroundKind = NoBackground
            , baseProficiencySkills = []
            , ruleSetKinds = [DnD5, Laelith, AiME]
            , asString = ""
            }
