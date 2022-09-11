module DmTools exposing (main)

import Array
import Browser
import Components.Atoms.DataDisplay as DataDisplay exposing (statReader, valueBox)
import Components.Atoms.Input as Input exposing (entitySelector, statInput)
import Html exposing (Html, a, br, div, footer, h1, h3, h4, img, input, label, li, nav, option, p, select, span, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onCheck, onClick, onInput)
import Models.Character as Character exposing (Character)
import Models.Msg as Msg exposing (Msg(..))
import Models.Rules.Background as Background exposing (Background, Backgrounds, get)
import Models.Rules.BackgroundKind as BackgroundKind exposing (BackgroundKind(..), BackgroundKinds, all, fromString)
import Models.Rules.Class as Class exposing (Class, Classes, get, none)
import Models.Rules.ClassKind as ClassKind exposing (ClassKind(..), ClassKinds, all, fromString)
import Models.Rules.Race as Race exposing (Race, Races, get, none)
import Models.Rules.RaceKind as RaceKind exposing (RaceKind(..), RaceKinds, all, fromString)
import Models.Rules.RuleSet as RuleSet exposing (RuleSet, RuleSets, get)
import Models.Rules.RuleSetKind as RuleSetKind exposing (RuleSetKind(..), RuleSetKinds, all, fromString)
import Models.Rules.Skill as Skill exposing (Skill, Skills, get)
import Models.Rules.SkillKind as SkillKind exposing (SkillKind(..), SkillKinds, all, fromString)
import Models.Rules.Stat as Stat exposing (Stat, Stats)
import Models.Rules.StatKind as StatKind exposing (StatKind(..), StatKinds, all, toString)
import Models.Rules.SubRace as SubRace exposing (SubRace, SubRaces, get, none)
import Models.Rules.SubRaceKind as SubRaceKind exposing (SubRaceKind(..), SubRaceKinds, all, fromString)
import Models.Settings as Settings exposing (Settings)



-- MODEL


type alias Model =
    { character : Character
    , settings : Settings
    }


init : Model
init =
    { character =
        { rolledStats =
            [ ( Strength, 8 )
            , ( Dexterity, 8 )
            , ( Constitution, 8 )
            , ( Intelligence, 8 )
            , ( Wisdom, 8 )
            , ( Charisma, 8 )
            ]
        , raceKind = Nothing
        , subRaceKind = Nothing
        , classKind = Nothing
        , backgroundKind = Nothing
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


view : Model -> Html Msg
view model =
    let
        currentRace =
            case model.character.raceKind of
                Just raceKind ->
                    Race.get raceKind

                Nothing ->
                    Race.none

        currentSubRace =
            case model.character.subRaceKind of
                Just subRaceKind ->
                    SubRace.get subRaceKind

                Nothing ->
                    SubRace.none

        currentClass =
            case model.character.classKind of
                Just classKind ->
                    Class.get classKind

                Nothing ->
                    Class.none

        currentBackground =
            case model.character.backgroundKind of
                Just backgroundKind ->
                    Background.get backgroundKind

                Nothing ->
                    Background.none

        currentProficiencyBonus =
            computeProficiency model.character.level

        finalStats =
            computeFinalStats model.character.rolledStats currentRace currentSubRace

        characterBaseLife =
            currentClass.lifeDice + (getStatScore finalStats Constitution |> computeModifier)

        availableRaces =
            getRuleSetRaces model.settings.ruleSetKind

        availableSubRaces =
            currentRace.subRaces

        availableClasses =
            getRuleSetClasses model.settings.ruleSetKind

        availableSkills =
            getRuleSetSkills model.settings.ruleSetKind

        availableBackgrounds =
            getRuleSetBackgrounds model.settings.ruleSetKind
    in
    div [ class "main-container" ]
        [ nav []
            [ a [ href "#" ]
                [ span [ class "navbar-item" ] [ text "Dm Tools" ] ]
            ]
        , div [ class "content" ]
            [ h3 [] [ text "Game Version" ]
            , viewRuleSetSelector model.settings.ruleSetKind
            , Input.entitySelector "Race" availableRaces model.character.raceKind UpdateRace Race.get
            , Input.entitySelector "SubRace" availableSubRaces model.character.subRaceKind UpdateSubRace SubRace.get
            , Input.entitySelector "Class" availableClasses model.character.classKind UpdateClass Class.get
            , Input.entitySelector "Background" availableBackgrounds model.character.backgroundKind UpdateBackground Background.get
            , h3 [] [ text "Level" ]
            , viewLevelSelector
            , h3 [] [ text "Rolled stats" ]
            , div []
                [ label [ for "freeStatInput" ] [ text "Authorize free stats" ]
                , input [ type_ "checkbox", id "freeStatInput", onCheck CheckFreeStatInput ] []
                ]
            , div [ class "flex-row" ]
                (List.append
                    (List.map (\statKind -> viewStatInput model.character.rolledStats statKind) StatKind.all)
                    (if model.settings.freeStatsInput then
                        [ Html.text "" ]

                     else
                        [ DataDisplay.valueBox "POINTS" (String.fromInt model.character.remainingPoints) ]
                    )
                )
            , br [] []
            , h3 [] [ text "Computed stats" ]
            , div [ class "flex-row" ]
                (List.append
                    (List.map (\statKind -> viewStatReader finalStats statKind) StatKind.all)
                    [ DataDisplay.valueBox "PRO" (printWithSign currentProficiencyBonus) ]
                )
            , div [ class "flex-row" ]
                [ viewCharacterBaseLife model.character.classKind characterBaseLife ]
            , div [ class "flex-row" ]
                [ viewSavingThrows finalStats currentClass.proficiencySaves currentProficiencyBonus
                , viewSkills finalStats currentClass currentRace currentBackground currentProficiencyBonus model.character.selectedProficiencySkills model.settings.ruleSetKind
                ]
            ]
        , footer []
            [ p [] [ text "/[A-Z]IKWAN/" ] ]
        ]


viewRuleSetSelector : RuleSetKind -> Html Msg
viewRuleSetSelector selectedRuleSetKind =
    select [ onInput UpdateRuleSet ]
        [ option [ value "DnD5", selected (selectedRuleSetKind == DnD5) ] [ text "Dungeon & Dragon 5" ]
        , option [ value "AiME", selected (selectedRuleSetKind == AiME) ] [ text "Adventures in Middle Earth" ]
        ]


viewLevelSelector : Html Msg
viewLevelSelector =
    let
        levels =
            List.range 1 20
    in
    select [ onInput UpdateLevel ]
        (List.map (\level -> option [ value (String.fromInt level) ] [ text (String.fromInt level) ]) levels)


viewStatInput : Stats -> StatKind -> Html Msg
viewStatInput stats statKind =
    let
        statName =
            StatKind.toString statKind

        statValue =
            getStatScore stats statKind

        onIncrease =
            UpdateStat statKind (statValue + 1)

        onDecrease =
            UpdateStat statKind (statValue - 1)
    in
    Input.statInput statName statValue onIncrease onDecrease


viewStatReader : Stats -> StatKind -> Html Msg
viewStatReader stats statKind =
    let
        statName =
            StatKind.toString statKind

        statValue =
            getStatScore stats statKind

        statModifier =
            printWithSign (computeModifier statValue)
    in
    DataDisplay.statReader statName statModifier (String.fromInt statValue)


viewCharacterBaseLife : Maybe ClassKind -> Int -> Html Msg
viewCharacterBaseLife classKind baseLife =
    case classKind of
        Just kind ->
            DataDisplay.valueBox "LIFE" (String.fromInt baseLife)

        Nothing ->
            Html.text ""


viewSavingThrows : Stats -> StatKinds -> Int -> Html Msg
viewSavingThrows finalStats proficiencySaves proficiencyBonus =
    div [ class "margin-right" ]
        [ h4 [] [ text "Saving throw" ]
        , ul []
            (List.map
                (\statKind ->
                    let
                        statModifier =
                            computeModifier (getStatScore finalStats statKind)
                    in
                    viewSavingThrow statKind proficiencySaves statModifier proficiencyBonus
                )
                StatKind.all
            )
        ]


viewSavingThrow : StatKind -> StatKinds -> Int -> Int -> Html Msg
viewSavingThrow statKind proficiencySaves statModifier proficiencyBonus =
    let
        hasProficiencySave =
            List.member statKind proficiencySaves

        savingThrowScore =
            if hasProficiencySave then
                statModifier + proficiencyBonus

            else
                statModifier

        modifier =
            printWithSign savingThrowScore
    in
    li [] [ text (StatKind.toString statKind ++ " : " ++ modifier) ]


viewSkills : Stats -> Class -> Race -> Background -> Int -> SkillKinds -> RuleSetKind -> Html Msg
viewSkills finalStats currentClass currentRace currentBackground currentProficiencyBonus currentProficiencySkills ruleSetKind =
    let
        optionalProficiencySkillsLimitReached =
            List.length currentProficiencySkills >= currentClass.optionalProficiencySkillsLimit
    in
    div
        [ class "margin-right" ]
        [ h4 [] [ text "Skills" ]
        , ul []
            (List.map
                (\skillKind ->
                    viewSkill skillKind finalStats currentClass currentRace currentBackground currentProficiencyBonus currentProficiencySkills optionalProficiencySkillsLimitReached
                )
                (getRuleSetSkills ruleSetKind)
            )
        ]


viewSkill : SkillKind -> Stats -> Class -> Race -> Background -> Int -> SkillKinds -> Bool -> Html Msg
viewSkill skillKind finalStats currentClass currentRace currentBackground currentProficiencyBonus currentProficiencySkills optionalProficiencySkillsLimitReached =
    let
        skill =
            Skill.get skillKind

        statKind =
            skill.statKind

        statScore =
            getStatScore finalStats statKind

        hasBaseProficiencySkill =
            List.member skillKind (List.concat [ currentClass.baseProficiencySkills, currentRace.baseProficiencySkills ])

        hasClassProficiencySkill =
            List.member skillKind currentClass.optionalProficiencySkills

        hasBackgroundProficiencySkill =
            List.member skillKind currentBackground.baseProficiencySkills

        hasSelectedProficiencySkill =
            List.member skillKind currentProficiencySkills

        proficiencyBonus =
            if hasSelectedProficiencySkill || hasBaseProficiencySkill || hasBackgroundProficiencySkill then
                currentProficiencyBonus

            else
                0

        modifier =
            printWithSign (computeModifier statScore + proficiencyBonus)

        isSkillAutomaticallySelected =
            hasBaseProficiencySkill || hasBackgroundProficiencySkill

        isSkillUnavailableForClass =
            not hasClassProficiencySkill

        hasReachedLimitAndNotSelectedByUser =
            optionalProficiencySkillsLimitReached && not hasSelectedProficiencySkill

        disableCheckbox =
            isSkillAutomaticallySelected || isSkillUnavailableForClass || hasReachedLimitAndNotSelectedByUser
    in
    li []
        [ input [ type_ "checkbox", disabled disableCheckbox, onCheck (CheckProficiencySkill skillKind), checked (hasBaseProficiencySkill || hasSelectedProficiencySkill || hasBackgroundProficiencySkill) ] []
        , text (skill.asString ++ " (" ++ StatKind.toString statKind ++ ") :" ++ modifier)
        ]


stringToId : String -> String
stringToId string =
    String.replace "-" "" (String.replace " " "" string)



-- UPDATE


update : Msg -> Model -> Model
update msg ({ settings, character } as model) =
    let
        updateStat : Stats -> StatKind -> Int -> Stats
        updateStat stats statKind newScore =
            List.map
                (\stat ->
                    if Tuple.first stat == statKind then
                        if settings.freeStatsInput then
                            ( statKind, newScore )

                        else
                            ( statKind, Basics.min (Basics.max 8 newScore) 15 )

                    else
                        stat
                )
                stats

        pushSelectedProficiencySkill : SkillKinds -> SkillKind -> SkillKinds
        pushSelectedProficiencySkill selectedSkillKinds skillKind =
            skillKind :: selectedSkillKinds

        popSelectedProficiencySkill : SkillKinds -> SkillKind -> SkillKinds
        popSelectedProficiencySkill selectedSkillKinds skillKind =
            List.filter (\selectedSkillKind -> selectedSkillKind /= skillKind) selectedSkillKinds
    in
    case msg of
        UpdateRuleSet string ->
            { model
                | settings =
                    { settings | ruleSetKind = RuleSetKind.fromString string }
                , character = { character | raceKind = Nothing, subRaceKind = Nothing, classKind = Nothing }
            }

        UpdateRace string ->
            { model
                | character =
                    { character | raceKind = RaceKind.fromString string, subRaceKind = Nothing }
            }

        UpdateSubRace string ->
            { model
                | character =
                    { character | subRaceKind = SubRaceKind.fromString string }
            }

        UpdateClass string ->
            { model
                | character =
                    { character | classKind = ClassKind.fromString string, selectedProficiencySkills = [] }
            }

        UpdateBackground string ->
            { model
                | character =
                    { character | backgroundKind = BackgroundKind.fromString string, selectedProficiencySkills = [] }
            }

        UpdateLevel level ->
            { model
                | character =
                    { character | level = Maybe.withDefault 1 (String.toInt level) }
            }

        UpdateStat statKind newScore ->
            update UpdateRemainingPoints
                { model
                    | character =
                        { character | rolledStats = updateStat character.rolledStats statKind newScore }
                }

        UpdateRemainingPoints ->
            { model
                | character =
                    { character | remainingPoints = 27 - computeRemainingPoints character.rolledStats }
            }

        CheckFreeStatInput checked ->
            case checked of
                True ->
                    { model
                        | settings =
                            { settings | freeStatsInput = True }
                    }

                False ->
                    update UpdateRemainingPoints
                        { model
                            | settings =
                                { settings | freeStatsInput = False }
                        }

        CheckProficiencySkill skill checked ->
            case checked of
                True ->
                    { model
                        | character =
                            { character | selectedProficiencySkills = pushSelectedProficiencySkill character.selectedProficiencySkills skill }
                    }

                False ->
                    { model
                        | character =
                            { character | selectedProficiencySkills = popSelectedProficiencySkill character.selectedProficiencySkills skill }
                    }



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- HELPERS


computeRemainingPoints : Stats -> Int
computeRemainingPoints stats =
    List.sum (List.map (\stat -> computeStatCost (Tuple.second stat)) stats)


computeStatCost : Int -> Int
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


computeFinalStats : Stats -> Race -> SubRace -> Stats
computeFinalStats rolledStats race subRace =
    List.map
        (\statKind ->
            let
                finalScore =
                    getStatScore rolledStats statKind
                        + getStatScore race.statBonus statKind
                        + getStatScore subRace.statBonus statKind
            in
            ( statKind, finalScore )
        )
        StatKind.all


computeModifier : Int -> Int
computeModifier value =
    Basics.floor (toFloat (value - 10) / 2)


computeProficiency : Int -> Int
computeProficiency level =
    2 + Basics.floor (toFloat (level - 1) / 4)


getRuleSetRaces : RuleSetKind -> RaceKinds
getRuleSetRaces ruleSetKind =
    (RuleSet.get ruleSetKind).raceKinds


getRuleSetClasses : RuleSetKind -> ClassKinds
getRuleSetClasses ruleSetKind =
    (RuleSet.get ruleSetKind).classKinds


getRuleSetSkills : RuleSetKind -> SkillKinds
getRuleSetSkills ruleSetKind =
    (RuleSet.get ruleSetKind).skillKinds


getRuleSetBackgrounds : RuleSetKind -> BackgroundKinds
getRuleSetBackgrounds ruleSetKind =
    BackgroundKind.all


getStatScore : Stats -> StatKind -> Int
getStatScore stats statKind =
    let
        selectedStats =
            List.filter (\stat -> Tuple.first stat == statKind) stats

        head =
            List.head selectedStats
    in
    case head of
        Just stat ->
            Tuple.second stat

        Nothing ->
            0


printWithSign : Int -> String
printWithSign value =
    if value >= 0 then
        "+" ++ String.fromInt value

    else
        String.fromInt value
