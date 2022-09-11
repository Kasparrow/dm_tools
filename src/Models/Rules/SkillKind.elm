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
    ]


fromString : String -> Maybe SkillKind
fromString string =
    case string of
        "Acrobatics" ->
            Just Acrobatics

        "AnimalHandling" ->
            Just AnimalHandling

        "Arcana" ->
            Just Arcana

        "Athletics" ->
            Just Athletics

        "Deception" ->
            Just Deception

        "History" ->
            Just History

        "Insight" ->
            Just Insight

        "Intimidation" ->
            Just Intimidation

        "Investigation" ->
            Just Investigation

        "Lore" ->
            Just Lore

        "Medicine" ->
            Just Medicine

        "Nature" ->
            Just Nature

        "Perception" ->
            Just Perception

        "Performance" ->
            Just Performance

        "Persuasion" ->
            Just Persuasion

        "Riddle" ->
            Just Riddle

        "Religion" ->
            Just Religion

        "ShadowLore" ->
            Just ShadowLore

        "SleightOfHand" ->
            Just SleightOfHand

        "Stealth" ->
            Just Stealth

        "Survival" ->
            Just Survival

        "Traditions" ->
            Just Traditions

        _ ->
            Nothing
