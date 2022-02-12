module DmTools exposing (main)

import Html exposing (Html, div, h1, img, span, text, label, input, select, option, br, a, nav, p, footer, h3, h4, ul, li)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onCheck, on)
import Array
import Browser

-- TYPES

type Msg
    = UpdateRemainingPoints
    | UpdateRace String
    | UpdateSubRace String
    | UpdateClass String
    | UpdateLevel String
    | UpdateStat StatIdentifier Int
    | CheckFreeStatInput Bool
    | CheckProficiencySkill SkillIdentifier Bool 

type GameVersion
    = DnD5
    | AiME
    | Laelith
type alias GameVersions = List GameVersion

type StatIdentifier
    = Strength
    | Dexterity
    | Constitution
    | Intelligence
    | Wisdom
    | Charisma
type alias StatIdentifiers = List StatIdentifier
type alias Stat = (StatIdentifier, Int)
type alias Stats = List Stat

type RaceIdentifier
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
type alias RaceIdentifiers = List RaceIdentifier
type alias Race =
    { identifier: RaceIdentifier
    , statBonus: Stats
    , subRaces: SubRaceIdentifiers
    , gameVersions: GameVersions
    , asString: String
    }
type alias Races = List Race

type SubRaceIdentifier
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
    | HighElf
    | WoodElf
    | Drow
    | DeepGnome
    | RockGnome
    | LightfootHalfling
    | StoutHalfling
    | NoSubRace
type alias SubRaceIdentifiers = List SubRaceIdentifier
type alias SubRace =
    { identifier: SubRaceIdentifier
    , statBonus: Stats
    , gameVersions: GameVersions
    , asString: String
    }
type alias SubRaces = List SubRace

type ClassIdentifier
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
type alias ClassIdentifiers = List ClassIdentifier

type alias Class =
    { identifier: ClassIdentifier 
    , proficiencySaves: StatIdentifiers
    , proficiencySkills: SkillIdentifiers
    , proficiencySkillsLimit: Int
    , lifeDice: Int
    , gameVersions: GameVersions
    , asString: String
    }
type alias Classes = List Class

type SkillIdentifier
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
type alias SkillIdentifiers = List SkillIdentifier
type alias Skill = 
    { identifier: SkillIdentifier
    , statIdentifier: StatIdentifier
    , gameVersions: GameVersions
    , asString: String
    }
type alias Skills = List Skill

-- MODEL

type alias Model = 
    { rolledStats: Stats
    , race: Race
    , subRace: SubRace
    , class: Class
    , remainingPoints: Int
    , freeStatsInput: Bool
    , level: Int
    , selectedProficiencySkills: SkillIdentifiers
    }

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
    , race = getRace NoRace
    , subRace = getSubRace NoSubRace
    , class = getClass NoClass
    , remainingPoints = 27
    , freeStatsInput = False
    , level = 1
    , selectedProficiencySkills = []
    }

-- VIEW

view: Model -> Html Msg
view model =
    let
        finalStats = computeFinalStats model
        proficiencyBonus = computeProficiency model.level
        characterBaseLife = model.class.lifeDice + (computeModifier (getStatScore finalStats Constitution))
    in
    div [ class "main-container" ]
        [ nav []
              [ a [ href "#" ]
                  [ span [ class "navbar-item" ] [ text "Dm Tools" ] ]
              ]
        , div [ class "content" ]
              [ h3 [] [ text "Race" ]
              , viewRaceSelector allRaceIdentifiers
              , viewSubRaceSelector model.race.subRaces model.subRace.identifier
              , h3 [] [ text "Class" ]
              , viewClassSelector allClassIdentifiers
              , h3 [] [ text "Level" ]
              , viewLevelSelector
              , h3 [] [ text "Rolled stats" ]
              , div [] [ label [ for "freeStatInput" ] [ text "Authorize free stats" ] 
                       , input [ type_ "checkbox", id "freeStatInput", onCheck CheckFreeStatInput ] []
                       ]
              , div [ class "flex-row" ]
                    (List.append
                        (List.map(\statIdentifier -> viewStatInput model.rolledStats statIdentifier) allStatIdentifiers) 
                        (if model.freeStatsInput then [ Html.text "" ] else [viewValueBox "POINTS" (String.fromInt model.remainingPoints)])
                    )
              , br [] []
              , h3 [] [ text "Computed stats" ]
              , div [ class "flex-row" ]
                    (List.append
                    (List.map (\statIdentifier -> viewStatReader finalStats statIdentifier) allStatIdentifiers)
                        [viewValueBox "PRO" (printWithSign proficiencyBonus)]
                    )
              , div [ class "flex-row" ]
                    [ viewCharacterBaseLife model.class.identifier characterBaseLife ]
              , div [ class "flex-row" ]
                    [ viewSavingThrows finalStats model.class.proficiencySaves proficiencyBonus
                    , viewSkills model]
              ]
        , footer []
                 [ p [] [ text "/[A-Z]IKWAN/" ] ]
        ]

viewRaceSelector: RaceIdentifiers -> Html Msg
viewRaceSelector raceIdentifiers =
    select [onInput UpdateRace] (List.map viewRaceOption raceIdentifiers)

viewRaceOption: RaceIdentifier -> Html Msg
viewRaceOption raceIdentifier =
    case raceIdentifier of
        NoRace -> option [ value "" ] [ text "Select a race" ]
        _ -> viewOption (getRace raceIdentifier).asString

viewSubRaceSelector: SubRaceIdentifiers -> SubRaceIdentifier -> Html Msg
viewSubRaceSelector subRaceIdentifiers selectedSubRace =
    if List.length subRaceIdentifiers > 0 then
        select [onInput UpdateSubRace] 
               (List.map (\subRaceIdentifier -> viewSubRaceOption subRaceIdentifier selectedSubRace
               ) subRaceIdentifiers)
    else 
        Html.text ""

viewSubRaceOption: SubRaceIdentifier -> SubRaceIdentifier -> Html Msg
viewSubRaceOption subRaceIdentifier selectedSubRace =
    case subRaceIdentifier of
        NoSubRace -> option [ value "", selected (subRaceIdentifier == selectedSubRace) ] [ text "Select a subrace" ]
        _ -> viewOption (getSubRace subRaceIdentifier).asString

viewClassSelector: ClassIdentifiers -> Html Msg
viewClassSelector classIdentifiers =
    select [onInput UpdateClass ] (List.map viewClassOption classIdentifiers)

viewClassOption: ClassIdentifier -> Html Msg
viewClassOption classIdentifier =
    case classIdentifier of
        NoClass -> option [ value "" ] [ text "Select a class" ]
        _ -> viewOption (getClass classIdentifier).asString

viewOption: String -> Html Msg
viewOption label =
    option [ value (stringToId label) ] [ text label ]

viewLevelSelector: Html Msg
viewLevelSelector =
    let
        levels = (List.range 1 20)
    in
    select [ onInput UpdateLevel ] 
           (List.map (\level -> option [ value (String.fromInt level)] [ text (String.fromInt level) ]) levels)

viewStatInput: Stats -> StatIdentifier -> Html Msg
viewStatInput stats statIdentifier =
    let
        score = getStatScore stats statIdentifier
    in
    div [ class "stat-box" ]
        [ span [ class "stat-box-title" ] [ text (statToString statIdentifier) ]
        , div [ class "stat-box-body" ]
              [ span [ class "stat-box-value" ] [ text (String.fromInt score) ]
        , div [ class "stat-box-controls" ]
              [ span [ onClick (UpdateStat statIdentifier (score + 1)) ] [ text "+" ]
              , span [ onClick (UpdateStat statIdentifier (score - 1)) ] [ text "-" ]
              ]
        ]
    ]

viewValueBox: String -> String -> Html Msg
viewValueBox title value =
    div [ class "stat-box" ]
        [ span [ class "stat-box-title" ] [ text title ]
        , div [ class "stat-box-body" ]
              [ span [ class "stat-box-value" ] [ text value ]]
        ]

viewStatReader: Stats -> StatIdentifier -> Html Msg
viewStatReader stats statIdentifier =
    let
        score = getStatScore stats statIdentifier
    in
    div [ class "stat-box" ]
        [ span [ class "stat-box-title" ] [ text (statToString statIdentifier) ]
        , div [ class "stat-box-body" ]
              [ span [ class "stat-box-value" ] [ text (printWithSign (computeModifier score)) ]
              , div [ class "stat-box-bonus" ]
                    [ span [] [ text (String.fromInt score) ]
                    , span [] []]
              ]
        ]

viewCharacterBaseLife: ClassIdentifier -> Int -> Html Msg
viewCharacterBaseLife classIdentifier baseLife =
    if classIdentifier /= NoClass then
        viewValueBox "LIFE" (String.fromInt baseLife)
    else
        Html.text ""

viewSavingThrows: Stats -> StatIdentifiers -> Int -> Html Msg
viewSavingThrows finalStats proficiencySaves proficiencyBonus =
    div [ class "margin-right" ]
        [ h4  [] [ text "Saving throw" ]
        , ul []
             (List.map (\statIdentifier -> 
                 let 
                     statModifier = computeModifier (getStatScore finalStats statIdentifier)
                 in
                 (viewSavingThrow statIdentifier proficiencySaves statModifier  proficiencyBonus)
             ) allStatIdentifiers)
        ]

viewSavingThrow: StatIdentifier -> StatIdentifiers -> Int -> Int -> Html Msg
viewSavingThrow statIdentifier proficiencySaves statModifier proficiencyBonus =
    let
        hasProficiencySave = List.member statIdentifier proficiencySaves
        savingThrowScore = if hasProficiencySave then statModifier + proficiencyBonus else statModifier
        modifier = printWithSign savingThrowScore
    in
    li [] [ text (statToString statIdentifier ++ " : " ++ modifier) ]

viewSkills: Model -> Html Msg
viewSkills model =
    let
        proficiencySkillsLimitReached = (List.length model.selectedProficiencySkills) >= model.class.proficiencySkillsLimit 
    in
    div [ class "margin-right" ]
        [ h4 [] [text "Skills" ]
        , ul []
             (List.map (\skillIdentifier -> 
                 viewSkill model skillIdentifier proficiencySkillsLimitReached
             ) allSkillIdentifiers)
        ]

viewSkill: Model -> SkillIdentifier -> Bool -> Html Msg
viewSkill model skillIdentifier proficiencySkillsLimitReached =
    let
        skill = getSkill skillIdentifier
        statIdentifier = skill.statIdentifier
        statScore = getStatScore (computeFinalStats model) statIdentifier
        hasClassProficiencySkill = List.member skillIdentifier model.class.proficiencySkills
        hasSelectedProficiencySkill = List.member skillIdentifier model.selectedProficiencySkills
        proficiencyBonus = if hasSelectedProficiencySkill then (computeProficiency model.level) else 0
        modifier = printWithSign ((computeModifier statScore) + proficiencyBonus)
        disableCheckbox = (not hasSelectedProficiencySkill && (not hasClassProficiencySkill || proficiencySkillsLimitReached))
    in
    li []
       [ input [ type_ "checkbox", disabled disableCheckbox, onCheck (CheckProficiencySkill skillIdentifier), checked hasSelectedProficiencySkill] []
       , text (skill.asString ++ " (" ++ (statToString statIdentifier) ++ ") :" ++ modifier)
       ]


stringToId: String -> String
stringToId string =
        String.replace "-" "" (String.replace " " "" string)

-- UPDATE

update: Msg -> Model -> Model
update msg ({ rolledStats, freeStatsInput, selectedProficiencySkills } as model) =
    let
        updateStat: Stats -> StatIdentifier -> Int -> Stats
        updateStat stats statIdentifier newScore =
            List.map(\stat ->
                if Tuple.first stat == statIdentifier then
                    if freeStatsInput then
                        (statIdentifier, newScore)
                    else
                        (statIdentifier, (Basics.min (Basics.max 8 newScore) 15))
                else
                    stat
            ) stats

        pushSelectedProficiencySkill: SkillIdentifiers -> SkillIdentifier -> SkillIdentifiers
        pushSelectedProficiencySkill selectedSkillIdentifiers skillIdentifier =
            skillIdentifier :: selectedSkillIdentifiers

        popSelectedProficiencySkill: SkillIdentifiers -> SkillIdentifier -> SkillIdentifiers
        popSelectedProficiencySkill selectedSkillIdentifiers skillIdentifier =
            List.filter (\selectedSkillIdentifier -> selectedSkillIdentifier /= skillIdentifier) selectedSkillIdentifiers
    in
    case msg of
        UpdateRace raceIdentifier -> 
            { model | race = (getRace (stringToRace raceIdentifier)), subRace = getSubRace NoSubRace }
        UpdateSubRace subRaceIdentifier ->
            { model | subRace = (getSubRace (stringToSubRace subRaceIdentifier)) }
        UpdateClass classIdentifier ->
            { model | class = (getClass (stringToClass classIdentifier )), selectedProficiencySkills = [] }
        UpdateLevel level ->
            { model | level = (Maybe.withDefault 1 (String.toInt level)) }
        UpdateStat statIdentifier newScore ->
            update UpdateRemainingPoints { model | rolledStats = (updateStat rolledStats statIdentifier newScore) }
        UpdateRemainingPoints -> 
            { model | remainingPoints = (27 - (computeRemainingPoints model.rolledStats)) }
        CheckFreeStatInput checked ->
            case checked of
                True -> { model | freeStatsInput = True }
                False -> update UpdateRemainingPoints { model | freeStatsInput = False }
        CheckProficiencySkill skill checked ->
            case checked of
                True -> { model | selectedProficiencySkills = pushSelectedProficiencySkill selectedProficiencySkills skill }
                False -> { model | selectedProficiencySkills = popSelectedProficiencySkill selectedProficiencySkills skill }

-- MAIN

main = 
    Browser.sandbox { init = init, update = update, view = view }

-- HELPERS

computeRemainingPoints: Stats -> Int
computeRemainingPoints stats =
    List.sum (List.map (\stat -> computeStatCost (Tuple.second stat)) stats)

computeStatCost: Int -> Int
computeStatCost value =
    case value of
        8 -> 0
        9 -> 1
        10 -> 2
        11 -> 3
        12 -> 4
        13 -> 5
        14 -> 7
        15 -> 9
        _ -> 0

computeFinalStats: Model -> Stats
computeFinalStats model =
        (List.map (\statIdentifier -> 
            let 
                finalScore = (getStatScore model.rolledStats statIdentifier) +
                             (getStatScore model.race.statBonus statIdentifier) +
                             (getStatScore model.subRace.statBonus statIdentifier)
            in
            (statIdentifier, finalScore)
         ) allStatIdentifiers)

computeModifier: Int -> Int
computeModifier value =
    Basics.floor (toFloat (value - 10) / 2)

computeProficiency: Int -> Int
computeProficiency level =
    2 + (Basics.floor (toFloat (level - 1) / 4))

getStatScore: Stats -> StatIdentifier -> Int
getStatScore stats statIdentifier =
    let
        selectedStats = List.filter (\stat -> Tuple.first stat == statIdentifier) stats
        head = List.head selectedStats
    in
    case head of
        Just stat -> Tuple.second stat
        Nothing -> 0

printWithSign: Int -> String
printWithSign value =
    if value >= 0 then
        "+" ++ (String.fromInt value)
    else
        String.fromInt value

-- DATA

allStatIdentifiers: StatIdentifiers
allStatIdentifiers =
    [ Strength
    , Dexterity
    , Constitution
    , Intelligence
    , Wisdom
    , Charisma
    ]

allRaceIdentifiers: RaceIdentifiers
allRaceIdentifiers = 
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

allSubRaceIdentifiers: SubRaceIdentifiers
allSubRaceIdentifiers =
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
    , HillsDwarf
    , MountainsDwarf
    , HighElf
    , WoodElf
    , Drow
    , DeepGnome
    , RockGnome
    , LightfootHalfling
    , StoutHalfling
    ]

allClassIdentifiers: ClassIdentifiers
allClassIdentifiers =
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

allSkillIdentifiers: SkillIdentifiers
allSkillIdentifiers = 
    [ Acrobatics
    , AnimalHandling
    , Arcana
    , Athletics
    , Deception
    , History
    , Insight
    , Intimidation
    , Investigation
    , Medicine
    , Nature
    , Perception
    , Performance
    , Persuasion
    , Religion
    , SleightOfHand
    , Stealth
    , Survival
    ]


getRace: RaceIdentifier -> Race
getRace identifier =
    case identifier of
        Dragonborn ->
            { identifier = Dragonborn
            , statBonus = [ (Strength, 2), (Charisma, 1) ]
            , subRaces = 
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
            , gameVersions = [DnD5, Laelith]
            , asString = "Dragonborn"
            }
        Dwarf ->
            { identifier = Dwarf
            , statBonus = [ (Constitution, 2) ]
            , subRaces = [NoSubRace, HillsDwarf, MountainsDwarf]
            , gameVersions = [DnD5, Laelith]
            , asString = "Dwarf"
            }
        Elf ->
            { identifier = Elf
            , statBonus = [ (Dexterity, 2)]
            , subRaces = [ NoSubRace, Drow, WoodElf, HighElf ]
            , gameVersions = [DnD5, Laelith]
            , asString = "Elf"
            }
        Gnome ->
            { identifier = Gnome
            , statBonus = [ (Intelligence, 2) ]
            , subRaces = [ NoSubRace, DeepGnome, RockGnome ]
            , gameVersions = [DnD5, Laelith]
            , asString = "Gnome"
            }
        HalfElf ->
            { identifier = HalfElf
            , statBonus = [ (Charisma, 2) ]
            , subRaces = []
            , gameVersions = [DnD5, Laelith]
            , asString = "Half-Elf"
            }
        Halfling ->
            { identifier = Halfling
            , statBonus = [ (Dexterity, 2) ]
            , subRaces = [ NoSubRace, LightfootHalfling, StoutHalfling ]
            , gameVersions = [DnD5, Laelith ]
            , asString = "Halfling"
            }
        HalfOrc ->
            { identifier = HalfOrc
            , statBonus = [ (Strength, 2), (Constitution, 1) ]
            , subRaces = []
            , gameVersions = [DnD5, Laelith]
            , asString = "Half-Orc"
            }
        Human ->
            { identifier = Human
            , statBonus =
                [ (Strength, 1)
                , (Dexterity, 1)
                , (Constitution, 1)
                , (Intelligence, 1)
                , (Wisdom, 1)
                , (Charisma, 1)
                ]
            , subRaces = []
            , gameVersions = [DnD5, Laelith]
            , asString = "Human"
            }
        Tiefling ->
            { identifier = Tiefling
            , statBonus = [ (Intelligence, 1), (Charisma, 2) ]
            , subRaces = []
            , gameVersions = [DnD5, Laelith]
            , asString = "Tiefling"
            }
        _ ->
            { identifier = NoRace
            , statBonus = []
            , subRaces = []
            , gameVersions = []
            , asString = ""
            }

getSubRace: SubRaceIdentifier -> SubRace
getSubRace identifier =
    case identifier of
        BlackDragonborn ->
            { identifier = BlackDragonborn
            , statBonus = []
            , gameVersions = [DnD5, Laelith]
            , asString = "Black Dragonborn"
            }
        BlueDragonborn ->
            { identifier = BlueDragonborn
            , statBonus = []
            , gameVersions = [DnD5, Laelith]
            , asString = "Blue Dragonborn"
            }
        BrassDragonborn ->
            { identifier = BrassDragonborn
            , statBonus = []
            , gameVersions = [DnD5, Laelith]
            , asString = "Brass Dragonborn"
            }
        BronzeDragonborn ->
            { identifier = BronzeDragonborn 
            , statBonus = []
            , gameVersions = [DnD5, Laelith]
            , asString = "Bronze Dragonborn"
            }
        CopperDragonborn ->
            { identifier = CopperDragonborn
            , statBonus = []
            , gameVersions = [DnD5, Laelith]
            , asString = "Copper Dragonborn"
            }
        GoldDragonborn ->
            { identifier = GoldDragonborn 
            , statBonus = []
            , gameVersions = [DnD5, Laelith]
            , asString = "Gold Dragonborn"
            }
        GreenDragonborn ->
            { identifier = GreenDragonborn
            , statBonus = []
            , gameVersions = [DnD5, Laelith]
            , asString = "Green Dragonborn"
            }
        RedDragonborn ->
            { identifier = RedDragonborn
            , statBonus = []
            , gameVersions = [DnD5, Laelith]
            , asString = "Red Dragonborn"
            }
        SilverDragonborn ->
            { identifier = SilverDragonborn
            , statBonus = []
            , gameVersions = [DnD5, Laelith]
            , asString = "Silver Dragonborn"
            }
        WhiteDragonborn ->
            { identifier = WhiteDragonborn
            , statBonus = []
            , gameVersions = [DnD5, Laelith]
            , asString = "White Dragonborn"
            }
        HillsDwarf ->
            { identifier = HillsDwarf
            , statBonus = [ (Wisdom, 1) ]
            , gameVersions = [DnD5, Laelith]
            , asString = "Hills Dwarf"
            }
        MountainsDwarf ->
            { identifier = MountainsDwarf
            , statBonus = [ (Strength, 1) ]
            , gameVersions = [DnD5, Laelith]
            , asString = "Mountains Dwarf"
            }
        Drow ->
            { identifier = Drow
            , statBonus = [ (Charisma, 1) ]
            , gameVersions = [DnD5, Laelith]
            , asString = "Drow"
            }
        WoodElf ->
            { identifier = WoodElf
            , statBonus = [ (Wisdom, 1) ]
            , gameVersions = [DnD5, Laelith]
            , asString = "Wood Elf"
            }
        HighElf ->
            { identifier = HighElf
            , statBonus = [ (Intelligence, 1) ]
            , gameVersions = [DnD5, Laelith]
            , asString = "High Elf"
            }
        DeepGnome ->
            { identifier = DeepGnome
            , statBonus = [ (Dexterity, 1) ]
            , gameVersions = [DnD5, Laelith]
            , asString = "Deep Gnome"
            }
        RockGnome ->
            { identifier = RockGnome
            , statBonus = [ (Constitution, 1) ]
            , gameVersions = [DnD5, Laelith]
            , asString = "Rock Gnome"
            }
        LightfootHalfling ->
            { identifier = LightfootHalfling
            , statBonus = [ (Charisma, 1) ]
            , gameVersions = [DnD5, Laelith]
            , asString = "Lightfoot Halfling"
            }
        StoutHalfling ->
            { identifier = StoutHalfling
            , statBonus = [ (Constitution, 1) ]
            , gameVersions = [DnD5, Laelith]
            , asString = "Stout Halfling"
            }
        _ ->
            { identifier = NoSubRace
            , statBonus = []
            , gameVersions = []
            , asString = ""
            }

getClass: ClassIdentifier -> Class 
getClass classIdentifier =
    case classIdentifier of
        Barbarian ->
            { identifier = Barbarian
            , proficiencySaves = [ Strength, Constitution ]
            , proficiencySkills = [ AnimalHandling, Athletics, Intimidation, Nature, Perception, Survival ]
            , proficiencySkillsLimit = 2
            , lifeDice = 12
            , gameVersions = [DnD5, Laelith]
            , asString = "Barbarian"
            }
        Bard ->
            { identifier = Bard
            , proficiencySaves = [ Dexterity, Charisma ]
            , proficiencySkills = allSkillIdentifiers
            , proficiencySkillsLimit = 3
            , lifeDice = 8
            , gameVersions = [DnD5, Laelith]
            , asString = "Bard"
            }
        Cleric ->
            { identifier = Cleric
            , proficiencySaves = [ Wisdom, Charisma ]
            , proficiencySkills = [ History, Insight, Medicine, Persuasion, Religion ]
            , proficiencySkillsLimit = 2
            , lifeDice = 8
            , gameVersions = [DnD5, Laelith]
            , asString = "Cleric"
            }
        Druid ->
            { identifier = Druid
            , proficiencySaves = [ Intelligence, Wisdom ]
            , proficiencySkills = [ Arcana, AnimalHandling, Insight, Medicine, Nature, Perception, Religion, Survival ]
            , proficiencySkillsLimit = 2
            , lifeDice = 8
            , gameVersions = [DnD5, Laelith]
            , asString = "Druid"
            }
        Fighter ->
            { identifier = Fighter
            , proficiencySaves = [ Strength, Constitution ]
            , proficiencySkills = [ Acrobatics, AnimalHandling, Athletics, History, Insight, Intimidation, Perception, Survival ]
            , proficiencySkillsLimit = 2
            , lifeDice = 10
            , gameVersions = [DnD5, Laelith]
            , asString = "Fighter"
            }
        Monk ->
            { identifier = Monk
            , proficiencySaves = [ Strength, Dexterity ]
            , proficiencySkills = [ Acrobatics, Athletics, History, Insight, Religion, Stealth ]
            , proficiencySkillsLimit = 2
            , lifeDice = 8
            , gameVersions = [DnD5, Laelith]
            , asString = "Monk"
            }
        Paladin ->
            { identifier = Paladin
            , proficiencySaves = [ Wisdom, Charisma ]
            , proficiencySkills = [ Athletics, Insight, Intimidation, Medicine, Persuasion ]
            , proficiencySkillsLimit = 2
            , lifeDice = 10
            , gameVersions = [DnD5, Laelith]
            , asString = "Paladin"
            }
        Ranger ->
            { identifier = Ranger
            , proficiencySaves = [ Strength, Dexterity ]
            , proficiencySkills = [ AnimalHandling, Athletics, Insight, Investigation, Nature, Perception, Stealth, Survival ]
            , proficiencySkillsLimit = 2
            , lifeDice = 10
            , gameVersions = [DnD5, Laelith]
            , asString = "Ranger"
            }
        Rogue ->
            { identifier = Rogue
            , proficiencySaves = [ Dexterity, Intelligence ]
            , proficiencySkills = [ Acrobatics, Athletics, Deception, Insight, Intimidation, Investigation, Perception, Performance, Persuasion, SleightOfHand, Stealth ]
            , proficiencySkillsLimit = 4
            , lifeDice = 8
            , gameVersions = [DnD5, Laelith]
            , asString = "Rogue"
            }
        Sorcerer ->
            { identifier = Sorcerer 
            , proficiencySaves = [ Constitution, Charisma ]
            , proficiencySkills = [ Arcana, Deception, Insight, Intimidation, Persuasion ]
            , proficiencySkillsLimit = 2
            , lifeDice = 8
            , gameVersions = [DnD5, Laelith]
            , asString = "Sorcerer"
            }
        Warlock ->
            { identifier = Warlock
            , proficiencySaves = [ Wisdom, Charisma ]
            , proficiencySkills = [ Arcana, Deception, History, Intimidation, Investigation, Nature, Religion ]
            , proficiencySkillsLimit = 2
            , lifeDice = 6
            , gameVersions = [DnD5, Laelith]
            , asString = "Warlock"
            }
        Wizard ->
            { identifier = Wizard
            , proficiencySaves = [ Intelligence, Wisdom ]
            , proficiencySkills = [ Arcana, History, Insight, Investigation, Medicine, Religion ]
            , proficiencySkillsLimit = 2
            , lifeDice = 6
            , gameVersions = [DnD5, Laelith]
            , asString = "Wizard"
            }
        NoClass ->
            { identifier = NoClass
            , proficiencySaves = []
            , proficiencySkills = []
            , proficiencySkillsLimit = 0
            , lifeDice = 0
            , gameVersions = []
            , asString = ""
            }

getSkill: SkillIdentifier -> Skill
getSkill skillIdentifier =
    case skillIdentifier of
        Acrobatics ->
            { identifier = Acrobatics
            , statIdentifier = Dexterity
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "Acrobatics"
            }
        AnimalHandling ->
            { identifier = AnimalHandling
            , statIdentifier = Wisdom
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "Animal Handling"
            }
        Arcana ->
            { identifier = Arcana
            , statIdentifier = Intelligence
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "Arcana"
            }
        Athletics ->
            { identifier = Athletics
            , statIdentifier = Strength
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "Athletics"
            }
        Deception ->
            { identifier = Deception
            , statIdentifier = Charisma
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "Deception"
            }
        History ->
            { identifier = History
            , statIdentifier = Intelligence
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "History"
            }
        Insight ->
            { identifier = Insight
            , statIdentifier = Wisdom
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "Insight"
            }
        Intimidation ->
            { identifier = Intimidation
            , statIdentifier = Charisma
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "Intimidation"
            }
        Investigation ->
            { identifier = Investigation
            , statIdentifier = Intelligence
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "Investigation"
            }
        Medicine ->
            { identifier = Medicine
            , statIdentifier = Wisdom
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "Medicine"
            }
        Nature ->
            { identifier = Nature
            , statIdentifier = Intelligence
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "Nature"
            }
        Perception ->
            { identifier = Perception
            , statIdentifier = Wisdom
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "Perception"
            }
        Performance ->
            { identifier = Performance
            , statIdentifier = Charisma
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "Performance"
            }
        Persuasion ->
            { identifier = Persuasion
            , statIdentifier = Charisma
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "Persuasion"
            }
        Religion ->
            { identifier = Religion
            , statIdentifier = Intelligence
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "Religion"
            }
        SleightOfHand ->
            { identifier = SleightOfHand
            , statIdentifier = Dexterity
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "Sleight of Hand"
            }
        Stealth ->
            { identifier = Stealth
            , statIdentifier = Dexterity
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "Stealth"
            }
        Survival ->
            { identifier = Survival
            , statIdentifier = Wisdom
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "Survival"
            }
        NoSkill ->
            { identifier = NoSkill
            , statIdentifier = Strength
            , gameVersions = []
            , asString = ""
            }

-- IDENTIFIER FROM STRING

stringToRace: String -> RaceIdentifier
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

stringToSubRace: String -> SubRaceIdentifier
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

stringToClass: String -> ClassIdentifier
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

stringToSkill: String -> SkillIdentifier
stringToSkill str =
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

-- TO STRING

statToString: StatIdentifier -> String
statToString statIdentifier =
    case statIdentifier of
        Strength -> "STR"
        Dexterity -> "DEX"
        Constitution -> "CON"
        Intelligence -> "INT"
        Wisdom -> "WIS"
        Charisma -> "CHA"
