module DmTools exposing (main)

import Html exposing (Html, div, h1, img, span, text, label, input, select, option, br, a, nav, p, footer, h3, h4, ul, li)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onCheck, on)
import Array
import Browser

import Models.Stat as Stat exposing (Stat, Stats)
import Models.StatKind as StatKind exposing (StatKind(..), StatKinds, all)

-- TYPES

type Msg
    = UpdateGameVersion String
    | UpdateRemainingPoints
    | UpdateRace String
    | UpdateSubRace String
    | UpdateClass String
    | UpdateLevel String
    | UpdateStat StatKind Int
    | CheckFreeStatInput Bool
    | CheckProficiencySkill SkillIdentifier Bool 

type GameVersion
    = DnD5
    | AiME
    | Laelith
type alias GameVersions = List GameVersion

type Language
    = French
    | English


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
    | Barding
    | Beorning
    | Dunedain
    | LonelyMountainDwarf
    | MirkwoodElf
    | ShireHobbit
    | BreeMen
    | LakeMen
    | MinasTirithMen
    | RohanRider
    | WilderlandWoodmen
    | NoRace
type alias RaceIdentifiers = List RaceIdentifier
type alias Race =
    { identifier: RaceIdentifier
    , statBonus: Stats
    , subRaces: SubRaceIdentifiers
    , baseProficiencySkills: SkillIdentifiers
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
    | Scholar
    | Slayer
    | TreasureHunter
    | Wanderer
    | Warden
    | Warrior
    | NoClass
type alias ClassIdentifiers = List ClassIdentifier

type alias Class =
    { identifier: ClassIdentifier 
    , proficiencySaves: StatKinds
    , baseProficiencySkills: SkillIdentifiers
    , optionalProficiencySkills: SkillIdentifiers
    , optionalProficiencySkillsLimit: Int
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
type alias SkillIdentifiers = List SkillIdentifier
type alias Skill = 
    { identifier: SkillIdentifier
    , statKind: StatKind
    , gameVersions: GameVersions
    , asString: String
    }
type alias Skills = List Skill

-- MODEL
type alias Character = 
    { rolledStats: Stats
    , race: Race
    , subRace: SubRace
    , class: Class
    , remainingPoints: Int
    , level: Int
    , selectedProficiencySkills: SkillIdentifiers
    }
type alias Settings =
    { gameVersion: GameVersion
    , freeStatsInput: Bool
    , language: Language
    }

type alias Model = 
    { character: Character
    , settings: Settings
    }

init: Model
init = 
    { character =
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
        , level = 1
        , selectedProficiencySkills = []
        }
    , settings = 
        { gameVersion = DnD5
        , freeStatsInput = False
        , language = French
        }
    }

-- VIEW

view: Model -> Html Msg
view model =
    let
        finalStats = computeFinalStats model.character
        proficiencyBonus = computeProficiency model.character.level
        characterBaseLife = model.character.class.lifeDice + (getStatScore finalStats Constitution |> computeModifier)
        availableRaces = getGameVersionRaces model.settings.gameVersion
        availableClasses = getGameVersionClasses model.settings.gameVersion
        availableSkills = getGameVersionSkills model.settings.gameVersion
    in
    div [ class "main-container" ]
        [ nav []
              [ a [ href "#" ]
                  [ span [ class "navbar-item" ] [ text "Dm Tools" ] ]
              ]
        , div [ class "content" ]
              [ h3 [] [ text "Game Version" ]
              , viewGameVersionSelector model.settings.gameVersion
              , h3 [] [ text "Race" ]
              , viewRaceSelector availableRaces model.character.race.identifier
              , viewSubRaceSelector model.character.race.subRaces model.character.subRace.identifier
              , h3 [] [ text "Class" ]
              , viewClassSelector availableClasses model.character.class.identifier
              , h3 [] [ text "Level" ]
              , viewLevelSelector
              , h3 [] [ text "Rolled stats" ]
              , div [] [ label [ for "freeStatInput" ] [ text "Authorize free stats" ] 
                       , input [ type_ "checkbox", id "freeStatInput", onCheck CheckFreeStatInput ] []
                       ]
              , div [ class "flex-row" ]
                    (List.append
                        (List.map(\statKind -> viewStatInput model.character.rolledStats statKind) StatKind.all) 
                        (if model.settings.freeStatsInput then 
                            [ Html.text "" ] 
                         else [viewValueBox "POINTS" (String.fromInt model.character.remainingPoints)]
                         )
                    )
              , br [] []
              , h3 [] [ text "Computed stats" ]
              , div [ class "flex-row" ]
                    (List.append
                    (List.map (\statKind -> viewStatReader finalStats statKind) StatKind.all)
                        [viewValueBox "PRO" (printWithSign proficiencyBonus)]
                    )
              , div [ class "flex-row" ]
                    [ viewCharacterBaseLife model.character.class.identifier characterBaseLife ]
              , div [ class "flex-row" ]
                    [ viewSavingThrows finalStats model.character.class.proficiencySaves proficiencyBonus
                    , viewSkills model.character model.settings.gameVersion]
              ]
        , footer []
                 [ p [] [ text "/[A-Z]IKWAN/" ] ]
        ]

viewGameVersionSelector: GameVersion -> Html Msg
viewGameVersionSelector selectedGameVersion =
    select [ onInput UpdateGameVersion ] 
           [ option [ value "DnD5", selected (selectedGameVersion == DnD5) ] [ text "Dungeon & Dragon 5" ]
           , option [ value "AiME", selected (selectedGameVersion == AiME) ] [ text "Adventures in Middle Earth" ]
           ]

viewRaceSelector: RaceIdentifiers -> RaceIdentifier -> Html Msg
viewRaceSelector raceIdentifiers selectedRaceIdentifier =
    select [onInput UpdateRace] (List.map (\raceIdentifier -> viewRaceOption raceIdentifier selectedRaceIdentifier) raceIdentifiers )

viewRaceOption: RaceIdentifier -> RaceIdentifier -> Html Msg
viewRaceOption raceIdentifier selectedRaceIdentifier =
    case raceIdentifier of
        NoRace -> option [ value "", selected (raceIdentifier == selectedRaceIdentifier) ] [ text "Select a race" ]
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

viewClassSelector: ClassIdentifiers -> ClassIdentifier -> Html Msg
viewClassSelector classIdentifiers selectedClassIdentifier =
    select [onInput UpdateClass ] (List.map (\classIdentifier -> viewClassOption classIdentifier selectedClassIdentifier) classIdentifiers)

viewClassOption: ClassIdentifier -> ClassIdentifier -> Html Msg
viewClassOption classIdentifier selectedClassIdentifier =
    case classIdentifier of
        NoClass -> option [ value "", selected (classIdentifier == selectedClassIdentifier) ] [ text "Select a class" ]
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

viewStatInput: Stats -> StatKind -> Html Msg
viewStatInput stats statKind =
    let
        score = getStatScore stats statKind
    in
    div [ class "stat-box" ]
        [ span [ class "stat-box-title" ] [ text (statToString statKind) ]
        , div [ class "stat-box-body" ]
              [ span [ class "stat-box-value" ] [ text (String.fromInt score) ]
        , div [ class "stat-box-controls" ]
              [ span [ onClick (UpdateStat statKind (score + 1)) ] [ text "+" ]
              , span [ onClick (UpdateStat statKind (score - 1)) ] [ text "-" ]
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

viewStatReader: Stats -> StatKind -> Html Msg
viewStatReader stats statKind =
    let
        score = getStatScore stats statKind
    in
    div [ class "stat-box" ]
        [ span [ class "stat-box-title" ] [ text (statToString statKind) ]
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

viewSavingThrows: Stats -> StatKinds -> Int -> Html Msg
viewSavingThrows finalStats proficiencySaves proficiencyBonus =
    div [ class "margin-right" ]
        [ h4  [] [ text "Saving throw" ]
        , ul []
             (List.map (\statKind -> 
                 let 
                     statModifier = computeModifier (getStatScore finalStats statKind)
                 in
                 (viewSavingThrow statKind proficiencySaves statModifier  proficiencyBonus)
             ) StatKind.all)
        ]

viewSavingThrow: StatKind -> StatKinds -> Int -> Int -> Html Msg
viewSavingThrow statKind proficiencySaves statModifier proficiencyBonus =
    let
        hasProficiencySave = List.member statKind proficiencySaves
        savingThrowScore = if hasProficiencySave then statModifier + proficiencyBonus else statModifier
        modifier = printWithSign savingThrowScore
    in
    li [] [ text (statToString statKind ++ " : " ++ modifier) ]

viewSkills: Character -> GameVersion -> Html Msg
viewSkills character gameVersion =
    let
        optionalProficiencySkillsLimitReached = (List.length character.selectedProficiencySkills) >= character.class.optionalProficiencySkillsLimit 
    in
    div [ class "margin-right" ]
        [ h4 [] [text "Skills" ]
        , ul []
             (List.map (\skillIdentifier -> 
                 viewSkill character skillIdentifier optionalProficiencySkillsLimitReached
             ) (getGameVersionSkills gameVersion))
        ]

viewSkill: Character -> SkillIdentifier -> Bool -> Html Msg
viewSkill character skillIdentifier optionalProficiencySkillsLimitReached =
    let
        skill = getSkill skillIdentifier
        statKind = skill.statKind
        statScore = getStatScore (computeFinalStats character) statKind
        hasBaseProficiencySkill = List.member skillIdentifier (List.concat [character.class.baseProficiencySkills, character.race.baseProficiencySkills])
        hasClassProficiencySkill = List.member skillIdentifier character.class.optionalProficiencySkills
        hasSelectedProficiencySkill = List.member skillIdentifier character.selectedProficiencySkills
        proficiencyBonus = if (hasSelectedProficiencySkill || hasBaseProficiencySkill) then (computeProficiency character.level) else 0
        modifier = printWithSign ((computeModifier statScore) + proficiencyBonus)
        disableCheckbox = (hasBaseProficiencySkill || not hasSelectedProficiencySkill && (not hasClassProficiencySkill || optionalProficiencySkillsLimitReached))
    in
    li []
       [ input [ type_ "checkbox", disabled disableCheckbox, onCheck (CheckProficiencySkill skillIdentifier), checked (hasBaseProficiencySkill || hasSelectedProficiencySkill)] []
       , text (skill.asString ++ " (" ++ (statToString statKind) ++ ") :" ++ modifier)
       ]


stringToId: String -> String
stringToId string =
        String.replace "-" "" (String.replace " " "" string)

-- UPDATE

update: Msg -> Model -> Model
update msg ({ settings, character } as model) =
    let
        updateStat: Stats -> StatKind -> Int -> Stats
        updateStat stats statKind newScore =
            List.map(\stat ->
                if Tuple.first stat == statKind then
                    if settings.freeStatsInput then
                        (statKind, newScore)
                    else
                        (statKind, (Basics.min (Basics.max 8 newScore) 15))
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
        UpdateGameVersion gameVersion ->
            { model | settings =
                { settings | gameVersion = stringToGameVersion gameVersion }, 
                character = { character | race = getRace NoRace, subRace = getSubRace NoSubRace, class = getClass NoClass }
            }
        UpdateRace raceIdentifier -> 
            { model | character = 
                { character | race = (getRace (stringToRace raceIdentifier)), subRace = getSubRace NoSubRace }
            }
        UpdateSubRace subRaceIdentifier ->
            { model | character = 
                { character | subRace = (getSubRace (stringToSubRace subRaceIdentifier)) }
            }
        UpdateClass classIdentifier ->
            { model | character = 
                { character | class = (getClass (stringToClass classIdentifier )), selectedProficiencySkills = [] }
            }
        UpdateLevel level ->
            { model | character = 
                { character | level = (Maybe.withDefault 1 (String.toInt level)) }
            }
        UpdateStat statKind newScore ->
            update UpdateRemainingPoints { model | character = 
                { character | rolledStats = (updateStat character.rolledStats statKind newScore) }
            }
        UpdateRemainingPoints -> 
            { model | character = 
                { character | remainingPoints = (27 - (computeRemainingPoints character.rolledStats)) }
            }
        CheckFreeStatInput checked ->
            case checked of
                True -> 
                    { model | settings = 
                        { settings | freeStatsInput = True }
                    }
                False -> 
                    update UpdateRemainingPoints { model | settings = 
                        { settings | freeStatsInput = False }
                    }
        CheckProficiencySkill skill checked ->
            case checked of
                True -> 
                    { model | character = 
                        { character | selectedProficiencySkills = pushSelectedProficiencySkill character.selectedProficiencySkills skill }
                    }
                False -> 
                    { model | character = 
                        { character | selectedProficiencySkills = popSelectedProficiencySkill character.selectedProficiencySkills skill }
                    }

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

computeFinalStats: Character -> Stats
computeFinalStats character =
        (List.map (\statKind -> 
            let 
                finalScore = (getStatScore character.rolledStats statKind) +
                             (getStatScore character.race.statBonus statKind) +
                             (getStatScore character.subRace.statBonus statKind)
            in
            (statKind, finalScore)
         ) StatKind.all)

computeModifier: Int -> Int
computeModifier value =
    Basics.floor (toFloat (value - 10) / 2)

computeProficiency: Int -> Int
computeProficiency level =
    2 + (Basics.floor (toFloat (level - 1) / 4))

getGameVersionRaces: GameVersion -> RaceIdentifiers
getGameVersionRaces gameVersion =
    List.map (\race -> race.identifier)
             (List.filter(\race -> List.member gameVersion race.gameVersions) (List.map getRace allRaceIdentifiers))

getGameVersionClasses: GameVersion -> ClassIdentifiers
getGameVersionClasses gameVersion =
    List.map (\class -> class.identifier)
             (List.filter(\class -> List.member gameVersion class.gameVersions) (List.map getClass allClassIdentifiers))

getGameVersionSkills: GameVersion -> SkillIdentifiers
getGameVersionSkills gameVersion =
    List.map (\skill -> skill.identifier)
             (List.filter(\skill -> List.member gameVersion skill.gameVersions) (List.map getSkill allSkillIdentifiers))


getStatScore: Stats -> StatKind -> Int
getStatScore stats statKind =
    let
        selectedStats = List.filter (\stat -> Tuple.first stat == statKind) stats
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
    , Barding
    , Beorning
    , Dunedain
    , LonelyMountainDwarf
    , MirkwoodElf
    , ShireHobbit
    , BreeMen
    , LakeMen
    , MinasTirithMen
    , RohanRider
    , WilderlandWoodmen
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
    , Scholar
    , Slayer
    , TreasureHunter
    , Wanderer
    , Warden
    , Warrior
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
    , Lore
    , Medicine
    , Nature
    , Perception
    , Performance
    , Persuasion
    , Religion
    , Riddle
    , ShadowLore
    , SleightOfHand
    , Stealth
    , Survival
    , Traditions
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
            , baseProficiencySkills = []
            , gameVersions = [DnD5, Laelith]
            , asString = "Dragonborn"
            }
        Dwarf ->
            { identifier = Dwarf
            , statBonus = [ (Constitution, 2) ]
            , subRaces = [NoSubRace, HillsDwarf, MountainsDwarf]
            , baseProficiencySkills = []
            , gameVersions = [DnD5, Laelith]
            , asString = "Dwarf"
            }
        Elf ->
            { identifier = Elf
            , statBonus = [ (Dexterity, 2)]
            , subRaces = [ NoSubRace, Drow, WoodElf, HighElf ]
            , baseProficiencySkills = [Perception]
            , gameVersions = [DnD5, Laelith]
            , asString = "Elf"
            }
        Gnome ->
            { identifier = Gnome
            , statBonus = [ (Intelligence, 2) ]
            , subRaces = [ NoSubRace, DeepGnome, RockGnome ]
            , baseProficiencySkills = []
            , gameVersions = [DnD5, Laelith]
            , asString = "Gnome"
            }
        HalfElf ->
            { identifier = HalfElf
            , statBonus = [ (Charisma, 2) ]
            , subRaces = []
            , baseProficiencySkills = []
            , gameVersions = [DnD5, Laelith]
            , asString = "Half-Elf"
            }
        Halfling ->
            { identifier = Halfling
            , statBonus = [ (Dexterity, 2) ]
            , subRaces = [ NoSubRace, LightfootHalfling, StoutHalfling ]
            , baseProficiencySkills = []
            , gameVersions = [DnD5, Laelith ]
            , asString = "Halfling"
            }
        HalfOrc ->
            { identifier = HalfOrc
            , statBonus = [ (Strength, 2), (Constitution, 1) ]
            , subRaces = []
            , baseProficiencySkills = [Intimidation]
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
            , baseProficiencySkills = []
            , gameVersions = [DnD5, Laelith]
            , asString = "Human"
            }
        Tiefling ->
            { identifier = Tiefling
            , statBonus = [ (Intelligence, 1), (Charisma, 2) ]
            , subRaces = []
            , baseProficiencySkills = []
            , gameVersions = [DnD5, Laelith]
            , asString = "Tiefling"
            }
        Barding ->
            { identifier = Barding
            , statBonus = [ (Constitution, 1) ]
            , subRaces = []
            , baseProficiencySkills = [Insight]
            , gameVersions = [AiME]
            , asString = "Barding"
            }
        Beorning ->
            { identifier = Beorning
            , statBonus = [ (Strength, 1) ]
            , subRaces = []
            , baseProficiencySkills = [Intimidation]
            , gameVersions = [AiME]
            , asString = "Beorning"
            }
        Dunedain ->
            { identifier = Dunedain
            , statBonus = [ (Constitution, 1), (Wisdom, 1) ]
            , subRaces = []
            , baseProficiencySkills = [Survival]
            , gameVersions = [AiME]
            , asString = "Dunedain"
            }
        LonelyMountainDwarf ->
            { identifier = LonelyMountainDwarf
            , statBonus = [ (Constitution, 2) ]
            , subRaces = []
            , baseProficiencySkills = []
            , gameVersions = [AiME]
            , asString = "Lonely Mountain Dwarf"
            }
        MirkwoodElf ->
            { identifier = MirkwoodElf
            , statBonus = [ (Dexterity, 2), (Wisdom, 1) ]
            , subRaces = []
            , baseProficiencySkills = [Stealth]
            , gameVersions = [AiME]
            , asString = "Mirkwood Elf"
            }
        ShireHobbit ->
            { identifier = ShireHobbit
            , statBonus = [ (Dexterity, 2) ]
            , subRaces = []
            , baseProficiencySkills = [Stealth]
            , gameVersions = [AiME]
            , asString = "Shire Hobbit"
            }
        BreeMen ->
            { identifier = BreeMen
            , statBonus = [ (Wisdom, 1) ]
            , subRaces = []
            , baseProficiencySkills = [Perception]
            , gameVersions = [AiME]
            , asString = "Bree Men"
            }
        LakeMen ->
            { identifier = LakeMen
            , statBonus = [ (Charisma, 1) ]
            , subRaces = []
            , baseProficiencySkills = [Persuasion]
            , gameVersions = [AiME]
            , asString = "Lake Men"
            }
        MinasTirithMen ->
            { identifier = MinasTirithMen
            , statBonus = [ (Intelligence, 1) ]
            , subRaces = []
            , baseProficiencySkills = [History]
            , gameVersions = [AiME]
            , asString = "Minas Tirith Men"
            }
        RohanRider ->
            { identifier = RohanRider
            , statBonus = [ (Wisdom, 1)]
            , subRaces = []
            , baseProficiencySkills = [AnimalHandling]
            , gameVersions = [AiME]
            , asString = "Rohan Rider"
            }
        WilderlandWoodmen ->
            { identifier = WilderlandWoodmen
            , statBonus = [ (Dexterity, 1) ]
            , subRaces = []
            , baseProficiencySkills = [Survival]
            , gameVersions = [AiME]
            , asString = "Wilderland Woodmen"
            }
        NoRace ->
            { identifier = NoRace
            , statBonus = []
            , subRaces = []
            , baseProficiencySkills = []
            , gameVersions = [DnD5, Laelith, AiME]
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
        NoSubRace ->
            { identifier = NoSubRace
            , statBonus = []
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = ""
            }

getClass: ClassIdentifier -> Class 
getClass classIdentifier =
    case classIdentifier of
        Barbarian ->
            { identifier = Barbarian
            , proficiencySaves = [ Strength, Constitution ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ AnimalHandling, Athletics, Intimidation, Nature, Perception, Survival ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 12
            , gameVersions = [DnD5, Laelith]
            , asString = "Barbarian"
            }
        Bard ->
            { identifier = Bard
            , proficiencySaves = [ Dexterity, Charisma ]
            , baseProficiencySkills =  []
            , optionalProficiencySkills = allSkillIdentifiers
            , optionalProficiencySkillsLimit = 3
            , lifeDice = 8
            , gameVersions = [DnD5, Laelith]
            , asString = "Bard"
            }
        Cleric ->
            { identifier = Cleric
            , proficiencySaves = [ Wisdom, Charisma ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ History, Insight, Medicine, Persuasion, Religion ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 8
            , gameVersions = [DnD5, Laelith]
            , asString = "Cleric"
            }
        Druid ->
            { identifier = Druid
            , proficiencySaves = [ Intelligence, Wisdom ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ Arcana, AnimalHandling, Insight, Medicine, Nature, Perception, Religion, Survival ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 8
            , gameVersions = [DnD5, Laelith]
            , asString = "Druid"
            }
        Fighter ->
            { identifier = Fighter
            , proficiencySaves = [ Strength, Constitution ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ Acrobatics, AnimalHandling, Athletics, History, Insight, Intimidation, Perception, Survival ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 10
            , gameVersions = [DnD5, Laelith]
            , asString = "Fighter"
            }
        Monk ->
            { identifier = Monk
            , proficiencySaves = [ Strength, Dexterity ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ Acrobatics, Athletics, History, Insight, Religion, Stealth ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 8
            , gameVersions = [DnD5, Laelith]
            , asString = "Monk"
            }
        Paladin ->
            { identifier = Paladin
            , proficiencySaves = [ Wisdom, Charisma ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ Athletics, Insight, Intimidation, Medicine, Persuasion ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 10
            , gameVersions = [DnD5, Laelith]
            , asString = "Paladin"
            }
        Ranger ->
            { identifier = Ranger
            , proficiencySaves = [ Strength, Dexterity ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ AnimalHandling, Athletics, Insight, Investigation, Nature, Perception, Stealth, Survival ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 10
            , gameVersions = [DnD5, Laelith]
            , asString = "Ranger"
            }
        Rogue ->
            { identifier = Rogue
            , proficiencySaves = [ Dexterity, Intelligence ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ Acrobatics, Athletics, Deception, Insight, Intimidation, Investigation, Perception, Performance, Persuasion, SleightOfHand, Stealth ]
            , optionalProficiencySkillsLimit = 4
            , lifeDice = 8
            , gameVersions = [DnD5, Laelith]
            , asString = "Rogue"
            }
        Sorcerer ->
            { identifier = Sorcerer 
            , proficiencySaves = [ Constitution, Charisma ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ Arcana, Deception, Insight, Intimidation, Persuasion ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 8
            , gameVersions = [DnD5, Laelith]
            , asString = "Sorcerer"
            }
        Warlock ->
            { identifier = Warlock
            , proficiencySaves = [ Wisdom, Charisma ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ Arcana, Deception, History, Intimidation, Investigation, Nature, Religion ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 6
            , gameVersions = [DnD5, Laelith]
            , asString = "Warlock"
            }
        Wizard ->
            { identifier = Wizard
            , proficiencySaves = [ Intelligence, Wisdom ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [ Arcana, History, Insight, Investigation, Medicine, Religion ]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 6
            , gameVersions = [DnD5, Laelith]
            , asString = "Wizard"
            }
        Scholar ->
            { identifier = Scholar
            , proficiencySaves = [ Intelligence, Wisdom ]
            , baseProficiencySkills = [Medicine, Lore]
            , optionalProficiencySkills = [History, Riddle, Traditions, Insight, Investigation, Nature, Perception, Survival]
            , optionalProficiencySkillsLimit = 1
            , lifeDice = 8
            , gameVersions = [AiME]
            , asString = "Scholar"
            }
        Slayer ->
            { identifier = Slayer
            , proficiencySaves = [ Strength, Constitution ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [AnimalHandling, Athletics, Intimidation, Nature, Perception, Survival]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 12
            , gameVersions = [AiME]
            , asString = "Slayer"
            }
        TreasureHunter ->
            { identifier = TreasureHunter
            , proficiencySaves = [ Dexterity, Intelligence ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [Acrobatics, Athletics, Deception, Insight, Intimidation, Perception, Persuasion, Riddle, SleightOfHand, Stealth]
            , optionalProficiencySkillsLimit = 4
            , lifeDice = 8
            , gameVersions = [AiME]
            , asString = "Treasure Hunter"
            }
        Wanderer ->
            { identifier = Wanderer
            , proficiencySaves = [ Strength, Constitution ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = [AnimalHandling, Athletics, Insight, Investigation, Nature, Perception, Stealth, Traditions]
            , optionalProficiencySkillsLimit = 3
            , lifeDice = 10
            , gameVersions = [AiME]
            , asString = "Wanderer"
            }
        Warden ->
            { identifier = Warden
            , proficiencySaves = [ Dexterity, Charisma ]
            , baseProficiencySkills = []
            , optionalProficiencySkills = allSkillIdentifiers
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 8
            , gameVersions = [AiME]
            , asString = "Warden"
            }
        Warrior ->
            { identifier = Warrior
            , baseProficiencySkills = []
            , proficiencySaves = [ Strength, Constitution ]
            , optionalProficiencySkills = [Acrobatics, AnimalHandling, Athletics, History, Insight, Intimidation, Perception, Survival, Traditions]
            , optionalProficiencySkillsLimit = 2
            , lifeDice = 10
            , gameVersions = [AiME]
            , asString = "Warrior"
            }
        NoClass ->
            { identifier = NoClass
            , baseProficiencySkills = []
            , proficiencySaves = []
            , optionalProficiencySkills = []
            , optionalProficiencySkillsLimit = 0
            , lifeDice = 0
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = ""
            }

getSkill: SkillIdentifier -> Skill
getSkill skillIdentifier =
    case skillIdentifier of
        Acrobatics ->
            { identifier = Acrobatics
            , statKind = Dexterity
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "Acrobatics"
            }
        AnimalHandling ->
            { identifier = AnimalHandling
            , statKind = Wisdom
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "Animal Handling"
            }
        Arcana ->
            { identifier = Arcana
            , statKind = Intelligence
            , gameVersions = [DnD5, Laelith]
            , asString = "Arcana"
            }
        Athletics ->
            { identifier = Athletics
            , statKind = Strength
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "Athletics"
            }
        Deception ->
            { identifier = Deception
            , statKind = Charisma
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "Deception"
            }
        History ->
            { identifier = History
            , statKind = Intelligence
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "History"
            }
        Insight ->
            { identifier = Insight
            , statKind = Wisdom
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "Insight"
            }
        Intimidation ->
            { identifier = Intimidation
            , statKind = Charisma
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "Intimidation"
            }
        Investigation ->
            { identifier = Investigation
            , statKind = Intelligence
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "Investigation"
            }
        Lore ->
            { identifier = Lore
            , statKind = Intelligence
            , gameVersions = [AiME]
            , asString = "Lore"
            }
        Medicine ->
            { identifier = Medicine
            , statKind = Wisdom
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "Medicine"
            }
        Nature ->
            { identifier = Nature
            , statKind = Intelligence
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "Nature"
            }
        Perception ->
            { identifier = Perception
            , statKind = Wisdom
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "Perception"
            }
        Performance ->
            { identifier = Performance
            , statKind = Charisma
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "Performance"
            }
        Persuasion ->
            { identifier = Persuasion
            , statKind = Charisma
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "Persuasion"
            }
        Religion ->
            { identifier = Religion
            , statKind = Intelligence
            , gameVersions = [DnD5, Laelith]
            , asString = "Religion"
            }
        Riddle ->
            { identifier = Riddle
            , statKind = Intelligence
            , gameVersions = [AiME]
            , asString = "Riddle"
            }
        ShadowLore ->
            { identifier = ShadowLore
            , statKind = Intelligence
            , gameVersions = [AiME]
            , asString = "Shadow Lore"
            }
        SleightOfHand ->
            { identifier = SleightOfHand
            , statKind = Dexterity
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "Sleight of Hand"
            }
        Stealth ->
            { identifier = Stealth
            , statKind = Dexterity
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "Stealth"
            }
        Survival ->
            { identifier = Survival
            , statKind = Wisdom
            , gameVersions = [DnD5, Laelith, AiME]
            , asString = "Survival"
            }
        Traditions ->
            { identifier = Traditions
            , statKind = Intelligence
            , gameVersions = [AiME]
            , asString = "Traditions"
            }
        NoSkill ->
            { identifier = NoSkill
            , statKind = Strength
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
        "Barding" -> Barding
        "Beorning" -> Beorning
        "Dunedain" -> Dunedain
        "LonelyMountainDwarf" -> LonelyMountainDwarf
        "MirkwoodElf" -> MirkwoodElf
        "ShireHobbit" -> ShireHobbit
        "BreeMen" -> BreeMen
        "LakeMen" -> LakeMen
        "MinasTirithMen" -> MinasTirithMen
        "RohanRider" -> RohanRider
        "WilderlandWoodmen" -> WilderlandWoodmen
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
        "Scholar" -> Scholar
        "Slayer" -> Slayer
        "TreasureHunter" -> TreasureHunter
        "Wanderer" -> Wanderer
        "Warden" -> Warden
        "Warrior" -> Warrior
        _ -> NoClass

stringToGameVersion: String -> GameVersion
stringToGameVersion string =
    case string of
        "DnD5" -> DnD5
        "AiME" -> AiME
        "Laelith" -> Laelith
        _ -> DnD5

-- TO STRING

statToString: StatKind -> String
statToString statKind =
    case statKind of
        Strength -> "STR"
        Dexterity -> "DEX"
        Constitution -> "CON"
        Intelligence -> "INT"
        Wisdom -> "WIS"
        Charisma -> "CHA"

-- I18N

getKey: Language -> String -> String
getKey language key =
    case language of
        French ->
            case key of
                _ -> "Traduction non disponible"
        English ->
            case key of
                _ -> "Missing translation"
