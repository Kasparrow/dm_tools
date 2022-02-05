module DmTools exposing (main)

import Html exposing (Html, div, h1, img, span, text, label, input, select, option, br, a, nav, p, footer, h3, h4, ul, li)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onCheck, on)
import Array
import Browser

type Msg
    = UpdateRemainingPoints
    | UpdateRace String
    | UpdateSubRace String
    | UpdateClass String
    | UpdateLevel String
    | IncrementStat StatName
    | DecrementStat StatName
    | CheckFreeStatInput Bool
    | CheckProficiencySkill SkillName Bool 

init: Model
init = 
    { rolledStats = 
        [ (Strength, 8)
        , (Dexterity, 8)
        , (Constitution, 8)
        , (Intelligence, 8)
        , (Wisdom, 8)
        , (Charisma, 8)
        ]
    , race = NoRace
    , subRace = NoSubRace
    , class = NoClass
    , remainingPoints = 27
    , freeStatsInput = False
    , level = 1
    , selectedProficiencySkills = []
    }

-- MODEL

type StatName
    = Strength
    | Dexterity
    | Constitution
    | Intelligence
    | Wisdom
    | Charisma

enumStatName =
    [ Strength
    , Dexterity
    , Constitution
    , Intelligence
    , Wisdom
    , Charisma
    ]

type alias Stat = (StatName, Int)
type alias Stats = List Stat

type Race 
    = Dragonborn
    | Dwarf
    | Elf
    | Gnome
    | HalfElf
    | Halfling
    | HalfOrc
    | Human
    | Tiefling
    | NoRace

type SubRace
    = BlackDragonborn
    | BlueDragonborn
    | BrassDragonborn
    | BronzeDragonborn
    | CopperDragonborn
    | GoldDragonborn
    | GreenDragonborn
    | RedDragonborn
    | SilverDragonborn
    | WhiteDragonborn
    | HillsDwarf
    | MountainsDwarf
    | Drow
    | HighElf
    | WoodElf
    | DeepGnome
    | RockGnome
    | LightfootHalfling
    | StoutHalfling
    | NoSubRace

enumRace = 
    [ NoRace
    , Dragonborn
    , Dwarf
    , Elf
    , Gnome
    , HalfElf
    , Halfling
    , HalfOrc
    , Human
    , Tiefling
    ]

type Class
    = Barbarian
    | Bard
    | Cleric
    | Druid
    | Fighter
    | Monk
    | Paladin
    | Ranger
    | Rogue
    | Sorcerer
    | Warlock
    | Wizard
    | NoClass

enumClass =
    [ NoClass
    , Barbarian
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
    ]

type SkillName
    = Acrobatics
    | AnimalHandling
    | Arcana
    | Athletics
    | Deception
    | History
    | Insight
    | Intimidation
    | Investigation
    | Medicine
    | Nature
    | Perception
    | Performance
    | Persuasion
    | Religion
    | SleightOfHand
    | Stealth
    | Survival
    | NoSkill

type alias Skill = (SkillName, StatName)
type alias Skills = List Stat

enumSkills = 
    [ (Acrobatics, Dexterity)
    , (AnimalHandling, Wisdom)
    , (Arcana, Intelligence)
    , (Athletics, Strength)
    , (Deception, Charisma)
    , (History, Intelligence)
    , (Insight, Wisdom)
    , (Intimidation, Charisma)
    , (Investigation, Intelligence)
    , (Medicine, Wisdom)
    , (Nature, Intelligence)
    , (Perception, Wisdom)
    , (Performance, Charisma)
    , (Persuasion, Charisma)
    , (Religion, Intelligence)
    , (SleightOfHand, Dexterity)
    , (Stealth, Dexterity)
    , (Survival, Wisdom)
    ]

getSubRaces: Race -> List SubRace
getSubRaces race =
    case race of
        Dragonborn -> 
            [ NoSubRace
            , BlackDragonborn
            , BlueDragonborn
            , BrassDragonborn
            , BronzeDragonborn
            , CopperDragonborn
            , GoldDragonborn
            , GreenDragonborn
            , RedDragonborn
            , SilverDragonborn
            , WhiteDragonborn
            ]
        Dwarf -> [ NoSubRace, HillsDwarf, MountainsDwarf ]
        Elf -> [ NoSubRace, Drow, WoodElf, HighElf ]
        Gnome -> [ NoSubRace, DeepGnome, RockGnome ]
        Halfling -> [ NoSubRace, LightfootHalfling, StoutHalfling ]
        _ -> [ NoSubRace ]

type alias Model = 
    { rolledStats: Stats
    , race: Race
    , subRace: SubRace
    , class: Class
    , remainingPoints: Int
    , freeStatsInput: Bool
    , level: Int
    , selectedProficiencySkills: List SkillName
    }

-- VIEW

view: Model -> Html Msg
view model =
    let
        classProficiencySaves = getClassProficiencySave model.class
    in
    div [ class "main-container" ]
        [ nav []
              [ a [ href "#" ]
                  [ span [ class "navbar-item" ] [ text "Dm Tools" ] ]
              ]
        , div [ class "content" ]
              [ h3 [] [ text "Race" ]
              , viewRaceInput 
              , viewSubRaceInput model.race
              , h3 [] [ text "Class" ]
              , viewClassInput
              , h3 [] [ text "Level" ]
              , viewLevelInput
              , h3 [] [ text "Rolled stats" ]
              , div [] [ label [ for "freeStatInput"] [ text "Authorize free stats" ]
                       , input [ type_ "checkbox", id "freeStatInput", onCheck CheckFreeStatInput  ] []
                       ]
              , div [ class "flex-row" ]
                    (List.append 
                        (List.map(\statName -> viewStatInput statName model) enumStatName) 
                        (if model.freeStatsInput then [ Html.text "" ] else [viewValueBox "POINTS" (String.fromInt model.remainingPoints)] )
                    )
              , br [] []
              , h3 [] [ text "Computed stats" ]
              , div [ class "flex-row" ]
                    (List.append
                        (List.map (\statName -> viewStatReader statName model classProficiencySaves) enumStatName)
                        [viewValueBox "PRO" (printWithSign (computeProficiency model.level))]
                    )
                    
              , div [ class "flex-row" ]
                    [ viewCharacterBaseLife model ]
              , div [ class "flex-row" ] 
                    [ viewSavingThrows model classProficiencySaves
                    , viewSkills model
                    ]
              ]
        , footer []
                 [ p [] [ text "/[A-Z]IKWAN/" ] ]
        ]

viewRaceInput =
    select [onInput UpdateRace] (List.map viewRaceOption enumRace)

viewRaceOption race =
    case race of
        Dragonborn -> viewOption "Dragonborn"
        Dwarf -> viewOption "Dwarf"
        Elf -> viewOption "Elf"
        Gnome -> viewOption "Gnome"
        HalfElf -> viewOption "Half-Elf"
        Halfling -> viewOption "Halfling"
        HalfOrc -> viewOption "Half-Orc"
        Human -> viewOption "Human"
        Tiefling -> viewOption "Tielfling"
        NoRace -> option [ value "" ] [ text "Select a race" ]

viewSubRaceInput: Race -> Html Msg
viewSubRaceInput currentRace =
    let 
        subRaces = getSubRaces currentRace
    in
    if List.length subRaces > 1 then
        select [onInput UpdateSubRace] (List.map viewSubRaceOption subRaces)
    else
       Html.text "" 

viewClassInput =
    select [ onInput UpdateClass ] (List.map viewClassOption enumClass)

viewLevelInput =
    select [ onInput UpdateLevel ] (List.map (\level -> option [ value (String.fromInt level)] [ text (String.fromInt level) ]) (List.range 1 20))


viewClassOption class =
    case class of
        Barbarian -> viewOption "Barbarian"
        Bard -> viewOption "Bard"
        Cleric -> viewOption "Cleric"
        Druid -> viewOption "Druid"
        Fighter -> viewOption "Fighter"
        Monk -> viewOption "Monk"
        Paladin -> viewOption "Paladin"
        Ranger -> viewOption "Ranger"
        Rogue -> viewOption "Rogue"
        Sorcerer -> viewOption "Sorcerer"
        Warlock -> viewOption "Warlock"
        Wizard -> viewOption "Wizard"
        NoClass -> option [ value "" ] [ text "Select a class" ]

viewSubRaceOption: SubRace -> Html Msg
viewSubRaceOption subRace =
    case subRace of
        BlackDragonborn -> viewOption "Black Dragonborn"
        BlueDragonborn -> viewOption "Blue Dragonborn"
        BrassDragonborn -> viewOption "Brass Dragonborn"
        BronzeDragonborn -> viewOption "Bronze Dragonborn"
        CopperDragonborn -> viewOption "Bronze Dragonborn"
        GoldDragonborn -> viewOption "Gold Dragonborn"
        GreenDragonborn -> viewOption "Green Dragonborn"
        RedDragonborn -> viewOption "Red Dragonborn"
        SilverDragonborn -> viewOption "Silver Dragonborn"
        WhiteDragonborn -> viewOption "White Dragonborn"
        HillsDwarf -> viewOption "Hills Dwarf"
        MountainsDwarf -> viewOption "Mountains Dwarf"
        HighElf -> viewOption "High Elf"
        WoodElf -> viewOption "Wood Elf"
        Drow -> viewOption "Drow"
        DeepGnome -> viewOption "Deep Gnome"
        RockGnome -> viewOption "Rock Gnome"
        LightfootHalfling -> viewOption "Lightfoot Halfling"
        StoutHalfling -> viewOption "Stout Halfling"
        NoSubRace -> option [ value "" ] [ text "Select a subrace" ]

viewOption: String -> Html Msg
viewOption label =
    option [ value (stringToId label) ] [ text label ]

viewCharacterBaseLife: Model -> Html Msg
viewCharacterBaseLife model =
    if model.class /= NoClass then
        viewValueBox "LIFE" (String.fromInt (getCharacterBaseLife model))
    else
        Html.text ""

stringToId: String -> String
stringToId string =
    String.replace "-" "" (String.replace " " "" string)

viewStatInput: StatName -> Model -> Html Msg
viewStatInput statName model =
    let
        value = getStatValue model.rolledStats statName
    in

    div [ class "stat-box" ]
        [ span [ class "stat-box-title" ] [ text (statNameToString statName) ]
        , div [ class "stat-box-body" ]
              [ span [ class "stat-box-value" ] [ text (String.fromInt value) ]
              , div [ class "stat-box-controls" ]
                    [ span [ onClick (IncrementStat statName) ] [ text "+" ]
                    , span [ onClick (DecrementStat statName) ] [ text "-" ]
                    ]
              ]
        ]

viewStatReader: StatName -> Model -> List StatName -> Html Msg
viewStatReader statName model proficiencySaves =
    let
        hasProficiencySave = List.member statName proficiencySaves
        value = getFinalStatValue model statName
    in
    div [ class "stat-box" ]
        [ span [ class "stat-box-title" ] [ text (statNameToString statName) ]
        , div [ class "stat-box-body" ]
              [ span [ class "stat-box-value" ] [ text (String.fromInt value) ]
              , div [ class "stat-box-bonus" ]
                    [ span [] [ text (printWithSign (computeModifier value)) ]
                    , span [] [ text (if hasProficiencySave then "M" else "")]
                    ]
              ]
        ]

viewValueBox: String -> String -> Html Msg
viewValueBox title value =
    div [ class "stat-box" ]
        [ span [ class "stat-box-title" ] [ text title ]
        , div [ class "stat-box-body" ]
              [ span [ class "stat-box-value" ] [ text value ]
              ]
        ]

viewSkills: Model -> Html Msg
viewSkills model =
    let 
        classProficiencySkills = getClassProficiencySkills model.class
        classProficiencySkillsLimit = getClassProficiencySkillsLimit model.class
        reachProficiencySkillsLimit = (List.length model.selectedProficiencySkills)>= classProficiencySkillsLimit 
    in
    div [ class "margin-right" ]
        [ h4 [] [text "Skills" ]
        , ul []
             (List.map (\skill -> (viewSkill model skill classProficiencySkills reachProficiencySkillsLimit)) enumSkills)
        ]

viewSkill: Model -> Skill -> List SkillName -> Bool -> Html Msg
viewSkill model skill classProficiencySkills reachProficiencySkillsLimit =
    let
        skillName = Tuple.first skill
        skillNameStr = skillNameToString skillName
        associatedStat = Tuple.second skill
        associatedStatValue = getFinalStatValue model associatedStat
        associatedStatName = statNameToString associatedStat
        hasClassProficiencySkill = List.member skillName classProficiencySkills
        hasSelectedProficiencySkill = List.member skillName model.selectedProficiencySkills
        proficiencyBonus = if hasSelectedProficiencySkill then (computeProficiency model.level) else 0
        modifier = printWithSign ((computeModifier associatedStatValue) + proficiencyBonus)
        disableCheckbox = (not hasSelectedProficiencySkill && (not hasClassProficiencySkill || reachProficiencySkillsLimit))
    in
    li [] 
       [ input [ type_ "checkbox", disabled disableCheckbox, onCheck (CheckProficiencySkill skillName), checked hasSelectedProficiencySkill] []
       , text (skillNameStr ++ " (" ++ associatedStatName  ++ ") : " ++ modifier )
       ]

viewSavingThrows: Model -> List StatName -> Html Msg
viewSavingThrows model classProficiencySaves =
    div [ class "margin-right" ]
        [ h4  [] [ text "Saving throw" ]
        , ul []
             (List.map (\stat -> (viewSavingThrow model stat classProficiencySaves)) enumStatName)
        ]

viewSavingThrow: Model -> StatName -> List StatName -> Html Msg
viewSavingThrow model statName classProficiencySaves =
    let
        associatedStatValue = getFinalStatValue model statName
        hasProficiencySave = List.member statName classProficiencySaves
        value = getFinalStatValue model statName 
        proficiencyBonus = if hasProficiencySave then (computeProficiency model.level) else 0
        modifier = printWithSign ((computeModifier value) + proficiencyBonus)
    in
    li [] [ text (statNameToString statName ++ " : " ++ modifier) ]

-- UPDATE

update: Msg -> Model -> Model
update msg ({ rolledStats, freeStatsInput, selectedProficiencySkills } as model) =
    let
        incrementStat: Stats -> StatName -> Stats
        incrementStat stats statName =
            List.map (\stat -> 
                if Tuple.first stat == statName then 
                    let
                        incrementedValue = (Tuple.second stat + 1)
                    in
                    if freeStatsInput then
                        (statName, incrementedValue)
                    else 
                        (statName, (Basics.min (Basics.max 8 incrementedValue) 15)) 
                else stat
            ) stats

        decrementStat: Stats -> StatName -> Stats
        decrementStat stats statName =
            List.map (\stat -> 
                let
                    decrementedValue = (Tuple.second stat - 1)
                in
                if Tuple.first stat == statName then 
                    if freeStatsInput then
                        (statName, decrementedValue)
                    else 
                        (statName, (Basics.min (Basics.max 8 decrementedValue) 15)) 
                else stat
            ) stats

        pushSelectedProficiencySkill: List SkillName -> SkillName -> List SkillName
        pushSelectedProficiencySkill selectedSkillNames skillName =
            skillName :: selectedSkillNames

        removeSelectedProficiencySkill: List SkillName -> SkillName -> List SkillName
        removeSelectedProficiencySkill selectedSkillNames skillName =
            List.filter (\selectedSkillName -> selectedSkillName /= skillName) selectedSkillNames
    in
        case msg of
            IncrementStat statName -> update UpdateRemainingPoints { model | rolledStats = (incrementStat rolledStats statName) }
            DecrementStat statName -> update UpdateRemainingPoints { model | rolledStats = (decrementStat rolledStats statName) }
            UpdateRemainingPoints -> { model | remainingPoints = (27 - (computeRemainingPoints model.rolledStats)) }
            UpdateRace value -> { model | race = (stringToRace value), subRace = NoSubRace }
            UpdateSubRace value -> { model | subRace = (stringToSubRace value ) }
            UpdateClass value -> { model | class = (stringToClass value), selectedProficiencySkills = [] }
            UpdateLevel value -> { model | level = (Maybe.withDefault 1 (String.toInt value)) } 
            CheckFreeStatInput checked ->
                case checked of
                    True -> { model | freeStatsInput = True }
                    False -> update UpdateRemainingPoints { model | freeStatsInput = False }
            CheckProficiencySkill skill checked ->
                case checked of
                    True -> { model | selectedProficiencySkills = pushSelectedProficiencySkill selectedProficiencySkills skill }
                    False -> { model | selectedProficiencySkills = removeSelectedProficiencySkill selectedProficiencySkills skill }

-- HELPERS

getFinalStatValue: Model -> StatName -> Int
getFinalStatValue model statName =
    let
        rolledStat = getStatValue model.rolledStats statName
        raceBonusStat = getStatValue (getRaceBonus model.race) statName
        subRaceBonusStat = getStatValue (getSubRaceBonus model.subRace) statName
    in
    rolledStat + raceBonusStat + subRaceBonusStat

getStatValue: Stats -> StatName -> Int
getStatValue stats statName =
    let
        selectedStats = List.filter (\stat -> Tuple.first stat == statName) stats
        tail = List.head selectedStats
    in
    case tail of
        Just stat -> Tuple.second stat
        Nothing -> 0

printWithSign: Int -> String
printWithSign value =
    if value >= 0 then
        "+" ++ (String.fromInt value)
    else
        String.fromInt value


computeRemainingPoints: Stats -> Int
computeRemainingPoints stats =
    List.sum (List.map (\stat -> computeStatCost (Tuple.second stat)) stats)

computeModifier: Int -> Int
computeModifier value =
    Basics.floor (toFloat (value - 10) / 2)


computeProficiency: Int -> Int
computeProficiency level =
    2 + (Basics.floor (toFloat (level - 1) / 4))


statNameToString: StatName -> String
statNameToString statName =
    case statName of
        Strength -> "STR"
        Dexterity -> "DEX"
        Constitution -> "CON"
        Intelligence -> "INT"
        Wisdom -> "WIS"
        Charisma -> "CHA"

raceToString: Race -> String
raceToString race =
    case race of
        Dragonborn -> "Dragonborn"
        Dwarf -> "Dwarf"
        Elf -> "Elf"
        Gnome -> "Gnome"
        HalfElf -> "HalfElf"
        Halfling -> "Halfling"
        HalfOrc -> "HalfOrc"
        Human -> "Human"
        Tiefling -> "Tiefling"
        NoRace -> ""

subRaceToString: SubRace -> String
subRaceToString subRace =
    case subRace of
        BlackDragonborn -> "BlackDragonborn"
        BlueDragonborn -> "BlueDragonborn"
        BrassDragonborn -> "BrassDragonborn"
        BronzeDragonborn -> "BronzeDragonborn"
        CopperDragonborn -> "CopperDragonborn"
        GoldDragonborn -> "GoldDragonborn"
        GreenDragonborn -> "GreenDragonborn"
        RedDragonborn -> "RedDragonborn"
        SilverDragonborn -> "SilverDragonborn"
        WhiteDragonborn -> "WhiteDragonborn"
        HillsDwarf -> "HillsDwarf"
        MountainsDwarf -> "MountainsDwarf"
        Drow -> "Drow"
        HighElf -> "HighElf"
        WoodElf -> "WoodElf"
        DeepGnome -> "DeepGnome"
        RockGnome -> "RockGnome"
        LightfootHalfling -> "LightfootHalfling"
        StoutHalfling -> "StoutHalfling"
        NoSubRace -> ""

stringToRace: String -> Race
stringToRace string =
    case string of
        "Dragonborn" -> Dragonborn
        "Dwarf" -> Dwarf
        "Elf" -> Elf
        "Gnome" -> Gnome
        "HalfElf" -> HalfElf
        "Halfling" -> Halfling
        "HalfOrc" -> HalfOrc
        "Human" -> Human
        "Tiefling" -> Tiefling
        _ -> NoRace

stringToSubRace: String -> SubRace
stringToSubRace string =
    case string of
        "BlackDragonborn" -> BlackDragonborn
        "BlueDragonborn" -> BlueDragonborn
        "BrassDragonborn" -> BrassDragonborn
        "BronzeDragonborn" -> BronzeDragonborn
        "CopperDragonborn" -> CopperDragonborn
        "GoldDragonborn" -> GoldDragonborn
        "GreenDragonborn" -> GreenDragonborn
        "RedDragonborn" -> RedDragonborn
        "SilverDragonborn" -> SilverDragonborn
        "WhiteDragonborn" -> WhiteDragonborn
        "HillsDwarf" -> HillsDwarf
        "MountainsDwarf" -> MountainsDwarf
        "Drow" -> Drow
        "HighElf" -> HighElf
        "WoodElf" -> WoodElf
        "DeepGnome" -> DeepGnome
        "RockGnome" -> RockGnome
        "LightfootHalfling" -> LightfootHalfling
        "StoutHalfling" -> StoutHalfling
        _ -> NoSubRace

stringToClass: String -> Class
stringToClass string =
    case string of
        "Barbarian" -> Barbarian
        "Bard" -> Bard
        "Cleric" -> Cleric
        "Druid" -> Druid
        "Fighter" -> Fighter
        "Monk" -> Monk
        "Paladin" -> Paladin
        "Ranger" -> Ranger
        "Rogue" -> Rogue
        "Sorcerer" -> Rogue
        "Warlock" -> Warlock
        "Wizard" -> Wizard
        _ -> NoClass

skillNameToString: SkillName -> String
skillNameToString skillName =
    case skillName of
        Acrobatics -> "Acrobatics"
        AnimalHandling -> "AnimalHandling"
        Arcana -> "Arcana"
        Athletics -> "Athletics"
        Deception -> "Deception"
        History -> "History"
        Insight -> "Insight"
        Intimidation -> "Intimidation"
        Investigation -> "Investigation"
        Medicine -> "Medicine"
        Nature -> "Nature"
        Perception -> "Perception"
        Performance -> "Performance"
        Persuasion -> "Persuasion"
        Religion -> "Religion"
        SleightOfHand -> "SleightOfHand"
        Stealth -> "Stealth"
        Survival -> "Survival"
        NoSkill -> "Unknown skill"

stringToSkillName: String -> SkillName
stringToSkillName str =
    case str of
        "Acrobatics" -> Acrobatics
        "AnimalHandling" -> AnimalHandling
        "Arcana" -> Arcana
        "Athletics" -> Athletics
        "Deception" -> Deception
        "History" -> History
        "Insight" -> Insight
        "Intimidation" -> Intimidation
        "Investigation" -> Investigation
        "Medicine" -> Medicine
        "Nature" -> Nature
        "Perception" -> Perception
        "Performance" -> Performance
        "Persuasion" -> Persuasion
        "Religion" -> Religion
        "SleightOfHand" -> SleightOfHand
        "Stealth" -> Stealth
        "Survival" -> Survival
        _ -> NoSkill


getRaceBonus: Race -> Stats
getRaceBonus race =
    case race of
        Dragonborn -> [ (Strength, 2), (Charisma, 1) ]
        Dwarf -> [ (Constitution, 2) ]
        Elf -> [ (Dexterity, 2)]
        Gnome -> [ (Intelligence, 2) ]
        HalfElf -> [ (Charisma, 2) ]
        Halfling -> [ (Dexterity, 2) ]
        HalfOrc -> [ (Strength, 2), (Constitution, 1) ]
        Human -> 
            [ (Strength, 1)
            , (Dexterity, 1)
            , (Constitution, 1)
            , (Intelligence, 1)
            , (Wisdom, 1)
            , (Charisma, 1)
            ]
        Tiefling -> [ (Intelligence, 1), (Charisma, 2) ]
        NoRace -> []

getSubRaceBonus: SubRace -> Stats
getSubRaceBonus subRace =
    case subRace of
        HillsDwarf -> [ (Wisdom, 1) ]
        MountainsDwarf -> [ (Strength, 2) ]
        Drow -> [ (Charisma, 1) ]
        WoodElf -> [ (Wisdom, 1) ]
        HighElf -> [ (Intelligence , 1) ]
        DeepGnome -> [ (Dexterity, 1) ]
        RockGnome -> [ (Constitution, 1) ]
        LightfootHalfling -> [ (Charisma, 1) ]
        StoutHalfling -> [ (Constitution, 1) ]
        _ -> []

getClassProficiencySave: Class -> List StatName
getClassProficiencySave class =
    case class of
        Barbarian -> [ Strength, Constitution ]
        Bard -> [ Dexterity, Charisma ]
        Cleric -> [ Wisdom, Charisma ]
        Druid -> [ Intelligence, Wisdom ]
        Fighter -> [ Strength, Constitution ]
        Monk -> [ Strength, Dexterity ]
        Paladin -> [ Wisdom, Charisma ]
        Ranger -> [ Strength, Dexterity ]
        Rogue -> [ Dexterity, Intelligence ]
        Sorcerer -> [ Constitution, Charisma ]
        Warlock -> [ Wisdom, Charisma ]
        Wizard -> [ Intelligence, Wisdom ]
        NoClass -> []

getClassProficiencySkills: Class -> List SkillName
getClassProficiencySkills class =
    case class of
        Barbarian -> [ AnimalHandling, Athletics, Intimidation, Nature, Perception, Survival ]
        Bard -> List.map (\skill -> Tuple.first skill) enumSkills
        Cleric -> [ History, Insight, Medicine, Persuasion, Religion ]
        Druid -> [ Arcana, AnimalHandling, Insight, Medicine, Nature, Perception, Religion, Survival ]
        Fighter -> [ Acrobatics, AnimalHandling, Athletics, History, Insight, Intimidation, Perception, Survival ]
        Monk -> [ Acrobatics, Athletics, History, Insight, Religion, Stealth ]
        Paladin -> [ Athletics, Insight, Intimidation, Medicine, Persuasion ]
        Ranger -> [ AnimalHandling, Athletics, Insight, Investigation, Nature, Perception, Stealth, Survival ]
        Rogue -> [ Acrobatics, Athletics, Deception, Insight, Intimidation, Investigation, Perception, Performance, Persuasion, SleightOfHand, Stealth ]
        Sorcerer -> [ Arcana, Deception, Insight, Intimidation, Persuasion ]
        Warlock -> [ Arcana, Deception, History, Intimidation, Investigation, Nature, Religion ]
        Wizard -> [ Arcana, History, Insight, Investigation, Medicine, Religion ]
        NoClass -> []

getClassProficiencySkillsLimit: Class -> Int
getClassProficiencySkillsLimit class =
    case class of
        Rogue -> 4
        Bard -> 3
        NoClass -> 0
        _ -> 2

getCharacterBaseLife: Model -> Int
getCharacterBaseLife model =
    let
        constitutionModifier = computeModifier (getFinalStatValue model Constitution)
    in

    case model.class of
        Barbarian -> 12 + constitutionModifier
        Bard -> 8 + constitutionModifier
        Cleric -> 8 + constitutionModifier
        Druid -> 8 + constitutionModifier
        Fighter -> 10 + constitutionModifier
        Monk -> 8 + constitutionModifier
        Paladin -> 10 + constitutionModifier
        Ranger -> 10 + constitutionModifier
        Rogue -> 8 + constitutionModifier
        Sorcerer -> 8 + constitutionModifier
        Warlock -> 6 + constitutionModifier
        Wizard -> 6 + constitutionModifier
        NoClass -> 0 + constitutionModifier

computeStatCost: Int -> Int
computeStatCost value =
    case value of
        8 ->
            0
        9 ->
            1
        10 ->
            2
        11 ->
            3
        12 ->
            4
        13 ->
            5
        14 ->
            7
        15 ->
            9
        _ ->
            0


main = 
    Browser.sandbox { init = init, update = update, view = view }
