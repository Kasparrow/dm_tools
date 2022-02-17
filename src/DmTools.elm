module DmTools exposing (main)

import Html exposing (Html, div, h1, img, span, text, label, input, select, option, br, a, nav, p, footer, h3, h4, ul, li)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onCheck, on)
import Array
import Browser

import Models.Rules.RuleSetKind as RuleSetKind exposing (RuleSetKind(..), RuleSetKinds, all, fromString)
import Models.Rules.StatKind as StatKind exposing (StatKind(..), StatKinds, all, toString)
import Models.Rules.SkillKind as SkillKind exposing (SkillKind(..), SkillKinds, all, fromString)
import Models.Rules.RaceKind as RaceKind exposing (RaceKind(..), RaceKinds, all, fromString)
import Models.Rules.SubRaceKind as SubRaceKind exposing (SubRaceKind(..), SubRaceKinds, all, fromString)
import Models.Rules.ClassKind as ClassKind exposing (ClassKind(..), ClassKinds, all, fromString)

import Models.Rules.Stat as Stat exposing (Stat, Stats)
import Models.Rules.Skill as Skill exposing (Skill, Skills, get)
import Models.Rules.Race as Race exposing (Race, Races, get)
import Models.Rules.SubRace as SubRace exposing (SubRace, SubRaces, get)
import Models.Rules.Class as Class exposing (Class, Classes, get)
import Models.Character as Character exposing (Character)
import Models.Settings as Settings exposing (Settings)

import Models.Msg as Msg exposing (Msg(..))

-- MODEL

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
        , race = Race.get NoRace
        , subRace = SubRace.get NoSubRace
        , class = Class.get NoClass
        , remainingPoints = 27
        , level = 1
        , selectedProficiencySkills = []
        }
    , settings = 
        { ruleSetKind = DnD5
        , freeStatsInput = False
        }
    }

-- VIEW

view: Model -> Html Msg
view model =
    let
        finalStats = computeFinalStats model.character
        proficiencyBonus = computeProficiency model.character.level
        characterBaseLife = model.character.class.lifeDice + (getStatScore finalStats Constitution |> computeModifier)
        availableRaces = getRuleSetRaces model.settings.ruleSetKind
        availableClasses = getRuleSetClasses model.settings.ruleSetKind
        availableSkills = getRuleSetSkills model.settings.ruleSetKind
    in
    div [ class "main-container" ]
        [ nav []
              [ a [ href "#" ]
                  [ span [ class "navbar-item" ] [ text "Dm Tools" ] ]
              ]
        , div [ class "content" ]
              [ h3 [] [ text "Game Version" ]
              , viewRuleSetSelector model.settings.ruleSetKind
              , h3 [] [ text "Race" ]
              , viewRaceSelector availableRaces model.character.race.raceKind
              , viewSubRaceSelector model.character.race.subRaces model.character.subRace.subRaceKind
              , h3 [] [ text "Class" ]
              , viewClassSelector availableClasses model.character.class.classKind
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
                    [ viewCharacterBaseLife model.character.class.classKind characterBaseLife ]
              , div [ class "flex-row" ]
                    [ viewSavingThrows finalStats model.character.class.proficiencySaves proficiencyBonus
                    , viewSkills model.character model.settings.ruleSetKind]
              ]
        , footer []
                 [ p [] [ text "/[A-Z]IKWAN/" ] ]
        ]

viewRuleSetSelector: RuleSetKind -> Html Msg
viewRuleSetSelector selectedRuleSetKind =
    select [ onInput UpdateRuleSet ] 
           [ option [ value "DnD5", selected (selectedRuleSetKind == DnD5) ] [ text "Dungeon & Dragon 5" ]
           , option [ value "AiME", selected (selectedRuleSetKind == AiME) ] [ text "Adventures in Middle Earth" ]
           ]

viewRaceSelector: RaceKinds -> RaceKind -> Html Msg
viewRaceSelector raceKinds selectedRaceKind =
    select [onInput UpdateRace] (List.map (\raceKind -> viewRaceOption raceKind selectedRaceKind) raceKinds )

viewRaceOption: RaceKind -> RaceKind -> Html Msg
viewRaceOption raceKind selectedRaceKind =
    case raceKind of
        NoRace -> option [ value "", selected (raceKind == selectedRaceKind) ] [ text "Select a race" ]
        _ -> viewOption (Race.get raceKind).asString

viewSubRaceSelector: SubRaceKinds -> SubRaceKind -> Html Msg
viewSubRaceSelector subRaceKinds selectedSubRaceKind =
    if List.length subRaceKinds > 0 then
        select [onInput UpdateSubRace] 
               (List.map (\subRaceKind -> viewSubRaceOption subRaceKind selectedSubRaceKind
               ) subRaceKinds)
    else 
        Html.text ""

viewSubRaceOption: SubRaceKind -> SubRaceKind -> Html Msg
viewSubRaceOption subRaceKind selectedSubRaceKind =
    case subRaceKind of
        NoSubRace -> option [ value "", selected (subRaceKind == selectedSubRaceKind) ] [ text "Select a subrace" ]
        _ -> viewOption (SubRace.get subRaceKind).asString

viewClassSelector: ClassKinds -> ClassKind -> Html Msg
viewClassSelector classKinds selectedClassKind =
    select [onInput UpdateClass ] (List.map (\classKind -> viewClassOption classKind selectedClassKind) classKinds)

viewClassOption: ClassKind -> ClassKind -> Html Msg
viewClassOption classKind selectedClassKind =
    case classKind of
        NoClass -> option [ value "", selected (classKind == selectedClassKind) ] [ text "Select a class" ]
        _ -> viewOption (Class.get classKind).asString

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
        [ span [ class "stat-box-title" ] [ text (StatKind.toString statKind) ]
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
        [ span [ class "stat-box-title" ] [ text (StatKind.toString statKind) ]
        , div [ class "stat-box-body" ]
              [ span [ class "stat-box-value" ] [ text (printWithSign (computeModifier score)) ]
              , div [ class "stat-box-bonus" ]
                    [ span [] [ text (String.fromInt score) ]
                    , span [] []]
              ]
        ]

viewCharacterBaseLife: ClassKind -> Int -> Html Msg
viewCharacterBaseLife classKind baseLife =
    if classKind /= NoClass then
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
    li [] [ text (StatKind.toString statKind ++ " : " ++ modifier) ]

viewSkills: Character -> RuleSetKind -> Html Msg
viewSkills character ruleSetKind =
    let
        optionalProficiencySkillsLimitReached = (List.length character.selectedProficiencySkills) >= character.class.optionalProficiencySkillsLimit 
    in
    div [ class "margin-right" ]
        [ h4 [] [text "Skills" ]
        , ul []
             (List.map (\skillKind -> 
                 viewSkill character skillKind optionalProficiencySkillsLimitReached
             ) (getRuleSetSkills ruleSetKind))
        ]

viewSkill: Character -> SkillKind -> Bool -> Html Msg
viewSkill character skillKind optionalProficiencySkillsLimitReached =
    let
        skill = Skill.get skillKind
        statKind = skill.statKind
        statScore = getStatScore (computeFinalStats character) statKind
        hasBaseProficiencySkill = List.member skillKind (List.concat [character.class.baseProficiencySkills, character.race.baseProficiencySkills])
        hasClassProficiencySkill = List.member skillKind character.class.optionalProficiencySkills
        hasSelectedProficiencySkill = List.member skillKind character.selectedProficiencySkills
        proficiencyBonus = if (hasSelectedProficiencySkill || hasBaseProficiencySkill) then (computeProficiency character.level) else 0
        modifier = printWithSign ((computeModifier statScore) + proficiencyBonus)
        disableCheckbox = (hasBaseProficiencySkill || not hasSelectedProficiencySkill && (not hasClassProficiencySkill || optionalProficiencySkillsLimitReached))
    in
    li []
       [ input [ type_ "checkbox", disabled disableCheckbox, onCheck (CheckProficiencySkill skillKind), checked (hasBaseProficiencySkill || hasSelectedProficiencySkill)] []
       , text (skill.asString ++ " (" ++ (StatKind.toString statKind) ++ ") :" ++ modifier)
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

        pushSelectedProficiencySkill: SkillKinds -> SkillKind -> SkillKinds
        pushSelectedProficiencySkill selectedSkillKinds skillKind =
            skillKind :: selectedSkillKinds

        popSelectedProficiencySkill: SkillKinds -> SkillKind -> SkillKinds
        popSelectedProficiencySkill selectedSkillKinds skillKind =
            List.filter (\selectedSkillKind -> selectedSkillKind /= skillKind) selectedSkillKinds
    in
    case msg of
        UpdateRuleSet string ->
            { model | settings =
                { settings | ruleSetKind = RuleSetKind.fromString string }, 
                character = { character | race = Race.get NoRace, subRace = SubRace.get NoSubRace, class = Class.get NoClass }
            }
        UpdateRace string -> 
            { model | character = 
                { character | race = (Race.get (RaceKind.fromString string)), subRace = SubRace.get NoSubRace }
            }
        UpdateSubRace string ->
            { model | character = 
                { character | subRace = (SubRace.get (SubRaceKind.fromString string)) }
            }
        UpdateClass string ->
            { model | character = 
                { character | class = (Class.get (ClassKind.fromString string )), selectedProficiencySkills = [] }
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

getRuleSetRaces: RuleSetKind -> RaceKinds
getRuleSetRaces ruleSetKind =
    List.map (\race -> race.raceKind)
             (List.filter(\race -> List.member ruleSetKind race.ruleSetKinds) (List.map Race.get RaceKind.all))

getRuleSetClasses: RuleSetKind -> ClassKinds
getRuleSetClasses ruleSetKind =
    List.map (\class -> class.classKind)
             (List.filter(\class -> List.member ruleSetKind class.ruleSetKinds) (List.map Class.get ClassKind.all))

getRuleSetSkills: RuleSetKind -> SkillKinds
getRuleSetSkills ruleSetKind =
    List.map (\skill -> skill.skillKind)
             (List.filter(\skill -> List.member ruleSetKind skill.ruleSetKinds) (List.map Skill.get SkillKind.all))

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
