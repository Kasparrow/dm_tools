module Models.Rules.SkillKind exposing (SkillKind(..), SkillKinds, all, fromString)


type SkillKind
    = Acrobatics
    | AnimalHandling
    | Arcana
    | Athletics
    | Deception
    | History
    | Insight
    | Intimidation
    | Investigation
    | Lore
    | Medicine
    | Nature
    | Perception
    | Performance
    | Persuasion
    | Riddle
    | Religion
    | ShadowLore
    | SleightOfHand
    | Stealth
    | Survival
    | Traditions
    | NoSkill


type alias SkillKinds =
    List SkillKind


all : SkillKinds
all =
    [ Acrobatics
    , AnimalHandling
    , Arcana
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
    , Religion
    , ShadowLore
    , SleightOfHand
    , Stealth
    , Survival
    , Traditions
    , NoSkill
    ]


fromString : String -> SkillKind
fromString string =
    case string of
        "Acrobatics" ->
            Acrobatics

        "AnimalHandling" ->
            AnimalHandling

        "Arcana" ->
            Arcana

        "Athletics" ->
            Athletics

        "Deception" ->
            Deception

        "History" ->
            History

        "Insight" ->
            Insight

        "Intimidation" ->
            Intimidation

        "Investigation" ->
            Investigation

        "Lore" ->
            Lore

        "Medicine" ->
            Medicine

        "Nature" ->
            Nature

        "Perception" ->
            Perception

        "Performance" ->
            Performance

        "Persuasion" ->
            Persuasion

        "Riddle" ->
            Riddle

        "Religion" ->
            Religion

        "ShadowLore" ->
            ShadowLore

        "SleightOfHand" ->
            SleightOfHand

        "Stealth" ->
            Stealth

        "Survival" ->
            Survival

        "Traditions" ->
            Traditions

        _ ->
            NoSkill
