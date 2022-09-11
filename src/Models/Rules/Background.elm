module Models.Rules.Background exposing (Background, Backgrounds, get, none)

import Models.Rules.BackgroundKind as BackgoundKind exposing (BackgroundKind(..), BackgroundKinds, all)
import Models.Rules.RuleSetKind exposing (RuleSetKind(..), RuleSetKinds)
import Models.Rules.SkillKind as SkillKind exposing (SkillKind(..), SkillKinds, all)


type alias Background =
    { baseProficiencySkills : SkillKinds
    , asString : String
    }


type alias Backgrounds =
    List Background


get : BackgroundKind -> Background
get backgroundKind =
    case backgroundKind of
        Acolyte ->
            { baseProficiencySkills = [ Insight, Religion ]
            , asString = "Acolyte"
            }

        Charlatan ->
            { baseProficiencySkills = [ Deception, SleightOfHand ]
            , asString = "Charlatan"
            }

        Criminal ->
            { baseProficiencySkills = [ Deception, Stealth ]
            , asString = "Criminal"
            }

        Entertainer ->
            { baseProficiencySkills = [ Acrobatics, Performance ]
            , asString = "Entertainer"
            }

        FolkHero ->
            { baseProficiencySkills = [ AnimalHandling, Survival ]
            , asString = "Folk Hero"
            }

        GuildArtisan ->
            { baseProficiencySkills = [ Insight, Persuasion ]
            , asString = "Guild Artisan"
            }

        Hermit ->
            { baseProficiencySkills = [ Medicine, Religion ]
            , asString = "Hermit"
            }

        Noble ->
            { baseProficiencySkills = [ History, Persuasion ]
            , asString = "Noble"
            }

        Outlander ->
            { baseProficiencySkills = [ Athletics, Survival ]
            , asString = "Outlander"
            }

        Sage ->
            { baseProficiencySkills = [ Arcana, History ]
            , asString = "Sage"
            }

        Sailor ->
            { baseProficiencySkills = [ Athletics, Perception ]
            , asString = "Sailor"
            }

        Soldier ->
            { baseProficiencySkills = [ Athletics, Intimidation ]
            , asString = "Soldier"
            }

        Urchin ->
            { baseProficiencySkills = [ SleightOfHand, Stealth ]
            , asString = "Urchin"
            }


none =
    { baseProficiencySkills = []
    , asString = ""
    }
